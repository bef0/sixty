{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-incomplete-record-updates #-}
module Assembler where

import qualified Assembly
import qualified CPSAssembly
import qualified Data.ByteString.Short as ShortByteString
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.String (fromString)
import qualified Literal
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.CallingConvention as LLVM.CallingConvention
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Global as LLVM.Global
import qualified LLVM.AST.Type as LLVM.Type
import qualified Name
import Protolude hiding (IntMap, cast, local, moduleName)
import Query (Query)
import qualified Query
import Rock

type Assembler = StateT AssemblerState (Task Query)

data AssemblerState = AssemblerState
  { _locals :: IntMap Assembly.Local (LLVM.Operand, OperandType)
  , _nextUnName :: !Word
  }

data OperandType
  = Word
  | WordPointer
  | FunctionPointer !Int
  deriving (Eq, Show)

llvmType :: OperandType -> LLVM.Type
llvmType type_ =
  case type_ of
     Word ->
       wordSizedInt

     WordPointer ->
       wordPointer

     FunctionPointer numArgs ->
       LLVM.Type.PointerType
         { pointerReferent = LLVM.FunctionType
           { resultType = LLVM.VoidType
           , argumentTypes = replicate numArgs wordPointer
           , isVarArg = False
           }
           , pointerAddrSpace = LLVM.AddrSpace 0
         }

alignment :: Num a => a
alignment =
  8

wordBits :: Num a => a
wordBits =
  64

wordSizedInt :: LLVM.Type
wordSizedInt =
  LLVM.Type.IntegerType wordBits

wordPointer :: LLVM.Type
wordPointer =
  LLVM.Type.PointerType
    { pointerReferent = wordSizedInt
    , pointerAddrSpace = LLVM.AddrSpace 0
    }

bytePointer :: LLVM.Type
bytePointer =
  LLVM.Type.PointerType
    { pointerReferent = LLVM.Type.IntegerType 8
    , pointerAddrSpace = LLVM.AddrSpace 0
    }

nextUnName :: Assembler LLVM.Name
nextUnName = do
  name <- gets _nextUnName
  modify $ \s -> s
    { _nextUnName = name + 1
    }
  pure $ LLVM.UnName name

activateLocal :: OperandType -> Assembly.Local -> Assembler LLVM.Name
activateLocal type_ local = do
  name <- nextUnName
  modify $ \s -> s
    { _locals = IntMap.insert local (LLVM.LocalReference (llvmType type_) name, type_) $ _locals s
    }
  pure name

assembleName :: Assembly.Name -> LLVM.Name
assembleName name =
  case name of
    Assembly.Name (Name.Lifted (Name.Qualified (Name.Module moduleName) (Name.Name name_)) 0) 0 ->
      LLVM.Name $ ShortByteString.toShort $ toUtf8 $ moduleName <> "." <> name_

    Assembly.Name (Name.Lifted (Name.Qualified (Name.Module moduleName) (Name.Name name_)) 0) j ->
      LLVM.Name $ ShortByteString.toShort $ toUtf8 $ moduleName <> "." <> name_ <> "$" <> show j

    Assembly.Name (Name.Lifted (Name.Qualified (Name.Module moduleName) (Name.Name name_)) i) j ->
      LLVM.Name $ ShortByteString.toShort $ toUtf8 $ moduleName <> "." <> name_ <> "$" <> show i <> "$" <> show j

definitionType :: CPSAssembly.Definition -> OperandType
definitionType definition =
  case definition of
    Assembly.ConstantDefinition {} ->
      WordPointer

    Assembly.FunctionDefinition parameters _ ->
      FunctionPointer $ length parameters

assembleDefinition :: Assembly.Name -> CPSAssembly.Definition -> Task Query LLVM.Definition
assembleDefinition name definition =
  case definition of
   Assembly.ConstantDefinition {} ->
     panic "Assembler: ConstantDefinition" -- TODO

   Assembly.FunctionDefinition parameters basicBlock ->
     flip evalStateT AssemblerState
      { _locals = mempty
      , _nextUnName = 0
      } $ do
       parameters' <- mapM (activateLocal WordPointer) parameters
       basicBlocks <- assembleBasicBlock basicBlock
       pure $
        LLVM.GlobalDefinition
        LLVM.Global.functionDefaults
          { LLVM.Global.callingConvention = LLVM.CallingConvention.GHC
          , LLVM.Global.returnType = LLVM.VoidType
          , LLVM.Global.name = assembleName name
          , LLVM.Global.parameters = ([LLVM.Parameter wordPointer parameter [] | parameter <- parameters'], False)
          , LLVM.Global.alignment = alignment
          , LLVM.Global.basicBlocks = basicBlocks
          }

assembleBasicBlock :: CPSAssembly.BasicBlock -> Assembler [LLVM.BasicBlock]
assembleBasicBlock (CPSAssembly.BasicBlock instructions terminator) = do
  blockLabel <- nextUnName
  compiledInstructions <- mapM assembleInstruction instructions
  assembleTerminator blockLabel (concat compiledInstructions) terminator

assembleTerminator :: LLVM.Name -> [LLVM.Named LLVM.Instruction] -> CPSAssembly.Terminator -> Assembler [LLVM.BasicBlock]
assembleTerminator blockLabel instructions terminator =
  case terminator of
    CPSAssembly.Switch scrutinee branches defaultBranch -> do
      (scrutinee', scrutineeInstructions) <- assembleOperand Word scrutinee
      branches' <- forM branches $ \(int, branchTerminator) -> do
        branchLabel <- nextUnName
        blocks <- assembleTerminator branchLabel [] branchTerminator
        pure (int, branchLabel, blocks)
      defaultLabel <- nextUnName
      defaultBlocks <- assembleTerminator defaultLabel [] defaultBranch
      pure $
        [ LLVM.BasicBlock
          blockLabel
          (scrutineeInstructions <> instructions)
          (LLVM.Do LLVM.Switch
            { operand0' = scrutinee'
            , defaultDest = defaultLabel
            , dests = [(LLVM.Constant.Int wordBits $ fromIntegral int, label) | (int, label, _) <- branches']
            , metadata' = []
            }
          )
        ]
        <> concat [blocks | (_, _, blocks) <- branches']
        <> defaultBlocks

    CPSAssembly.TailCall function arguments -> do
      (function', functionInstructions) <- assembleOperand (FunctionPointer $ length arguments) function
      (arguments', argumentInstructions) <- unzip <$> mapM (assembleOperand WordPointer) arguments
      pure
        [ LLVM.BasicBlock
          blockLabel
          (instructions <>
            functionInstructions <>
            concat argumentInstructions <>
            [ LLVM.Do LLVM.Call
              { tailCallKind = Just LLVM.Tail
              , callingConvention = LLVM.CallingConvention.GHC
              , returnAttributes = []
              , function = Right function'
              , arguments = [(arg, []) | arg <- arguments']
              , functionAttributes = []
              , metadata = []
              }
            ]
          )
          (LLVM.Do LLVM.Ret { returnOperand = Nothing, metadata' = mempty })
        ]

assembleInstruction :: CPSAssembly.Instruction -> Assembler [LLVM.Named LLVM.Instruction]
assembleInstruction instruction =
  case instruction of
    CPSAssembly.Copy destination source size -> do
      (destination', destinationInstructions) <- assembleOperand WordPointer destination
      (source', sourceInstructions) <- assembleOperand WordPointer source
      (size', sizeInstructions) <- assembleOperand Word size
      destination'' <- nextUnName
      source'' <- nextUnName
      let
        memcpyGlobal =
          LLVM.Constant.GlobalReference
            LLVM.FunctionType
              { LLVM.resultType = LLVM.Type.void
              , LLVM.argumentTypes =
                  [ bytePointer
                  , bytePointer
                  , wordSizedInt
                  , LLVM.Type.i32
                  , LLVM.Type.i1
                  ]
              , LLVM.isVarArg = False
              }
            (LLVM.Name $ "llvm.memcpy.p0i8.p0i8.i" <> fromString (show (wordBits :: Int)))
        arguments =
          [ LLVM.LocalReference bytePointer destination''
          , LLVM.LocalReference bytePointer source''
          , size'
          , LLVM.ConstantOperand $ LLVM.Constant.Int 32 alignment
          , LLVM.ConstantOperand $ LLVM.Constant.Int 1 0 -- isvolatile
          ]
      pure $
        destinationInstructions <>
        sourceInstructions <>
        sizeInstructions <>
        [ destination'' LLVM.:=
            LLVM.BitCast
              { operand0 = destination'
              , type' = bytePointer
              , metadata = mempty
              }
        , source'' LLVM.:=
            LLVM.BitCast
              { operand0 = source'
              , type' = bytePointer
              , metadata = mempty
              }
        , LLVM.Do
          LLVM.Call
            { tailCallKind = Nothing
            , callingConvention = LLVM.CallingConvention.C
            , returnAttributes = []
            , function = Right $ LLVM.ConstantOperand memcpyGlobal
            , arguments = [(arg, []) | arg <- arguments]
            , functionAttributes = []
            , metadata = []
            }
        ]

    CPSAssembly.Load destination address -> do
      (address', addressInstructions) <- assembleOperand WordPointer address
      destination' <- activateLocal Word destination
      pure $
        addressInstructions <>
        [ destination' LLVM.:=
          LLVM.Load
            { volatile = False
            , address = address'
            , maybeAtomicity = Nothing
            , alignment = alignment
            , metadata = []
            }
        ]

    CPSAssembly.Store address value -> do
      (address', addressInstructions) <- assembleOperand WordPointer address
      (value', valueInstructions) <- assembleOperand Word value
      pure $
        addressInstructions <>
        valueInstructions <>
        [ LLVM.Do
          LLVM.Store
            { volatile = False
            , address = address'
            , value = value'
            , maybeAtomicity = Nothing
            , alignment = alignment
            , metadata = []
            }
        ]

    CPSAssembly.Add destination operand1 operand2 -> do
      (operand1', operand1Instructions) <- assembleOperand Word operand1
      (operand2', operand2Instructions) <- assembleOperand Word operand2
      destination' <- activateLocal Word destination
      pure $
        operand1Instructions <>
        operand2Instructions <>
        [ destination' LLVM.:=
          LLVM.Add
            { nsw = False
            , nuw = False
            , operand0 = operand1'
            , operand1 = operand2'
            , metadata = []
            }
        ]

    CPSAssembly.Sub destination operand1 operand2 -> do
      (operand1', operand1Instructions) <- assembleOperand Word operand1
      (operand2', operand2Instructions) <- assembleOperand Word operand2
      destination' <- activateLocal Word destination
      pure $
        operand1Instructions <>
        operand2Instructions <>
        [ destination' LLVM.:=
          LLVM.Sub
            { nsw = False
            , nuw = False
            , operand0 = operand1'
            , operand1 = operand2'
            , metadata = []
            }
        ]

    CPSAssembly.HeapAllocate {} ->
      panic "Assember: HeapAllocate" -- TODO

assembleOperand :: OperandType -> Assembly.Operand -> Assembler (LLVM.Operand, [LLVM.Named LLVM.Instruction])
assembleOperand type_ operand =
  case operand of
    Assembly.LocalOperand local -> do
      locals <- gets _locals
      cast type_ $ IntMap.lookupDefault (panic "assembleOperand: no such local") local locals

    Assembly.Global global@(Assembly.Name liftedName _) -> do
      definitions <- fetch $ Query.CPSAssembly liftedName
      case List.lookup global definitions of -- TODO don't use list lookup
        Nothing ->
          panic "assembleOperand: No such global"

        Just definition -> do
          let
            defType =
              definitionType definition
          cast type_
            ( LLVM.ConstantOperand $
              LLVM.Constant.GlobalReference (llvmType defType) $ assembleName global
            , defType
            )

    Assembly.Lit lit ->
      case lit of
        Literal.Integer int ->
          cast type_
            ( LLVM.ConstantOperand LLVM.Constant.Int
              { integerBits = wordBits
              , integerValue = int
              }
            , Word
            )

cast :: OperandType -> (LLVM.Operand, OperandType) -> Assembler (LLVM.Operand, [LLVM.Named LLVM.Instruction])
cast newType (operand, type_)
  | type_ == newType =
    pure (operand, [])

  | otherwise = do
    newOperand <- nextUnName
    pure
      ( LLVM.LocalReference (llvmType newType) newOperand
      , pure $
        newOperand LLVM.:=
        case (type_, newType) of
          (Word, _) ->
            LLVM.IntToPtr
              { operand0 = operand
              , type' = llvmType newType
              , metadata = mempty
              }

          (_, Word) ->
            LLVM.PtrToInt
              { operand0 = operand
              , type' = llvmType newType
              , metadata = mempty
              }

          _ ->
            LLVM.BitCast
              { operand0 = operand
              , type' = llvmType newType
              , metadata = mempty
              }
      )
