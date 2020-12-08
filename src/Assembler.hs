{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-incomplete-record-updates #-}
module Assembler where

import qualified Assembly
import qualified CPSAssembly
import qualified Data.ByteString.Short as ShortByteString
import Data.IntMap (IntMap)
import Data.String (fromString)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.CallingConvention as LLVM.CallingConvention
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Global as LLVM.Global
import qualified LLVM.AST.Type as LLVM.Type
import qualified LLVM.AST.Typed as LLVM
import Protolude hiding (IntMap)

type Assembler = State AssemblerState

data AssemblerState = AssemblerState
  { _locals :: IntMap Assembly.Local LLVM.Operand
  , _nextUnName :: !Word
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
    { pointerReferent = LLVM.Type.i8
    , pointerAddrSpace = LLVM.AddrSpace 0
    }

activateLocal :: Assembly.Local -> Assembler LLVM.Name
activateLocal =
  _

assembleName :: Assembly.Name -> LLVM.Name
assembleName =
  _

assembleDefinition :: Assembly.Name -> CPSAssembly.Definition -> LLVM.Definition
assembleDefinition name definition =
  case definition of
   Assembly.ConstantDefinition basicBlock ->
     panic "Assembler: ConstantDefinition" -- TODO

   Assembly.FunctionDefinition parameters basicBlock ->
     flip evalState AssemblerState
      { _locals = mempty
      , _nextUnName = 0
      } $ do
       parameters' <- mapM activateLocal parameters
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
  compiledInstructions <- mapM assembleInstruction instructions
  assembleTerminator (concat compiledInstructions) terminator

assembleTerminator :: [LLVM.Named LLVM.Instruction] -> CPSAssembly.Terminator -> Assembler [LLVM.BasicBlock]
assembleTerminator instructions terminator =
  _

assembleInstruction :: CPSAssembly.Instruction -> Assembler [LLVM.Named LLVM.Instruction]
assembleInstruction instruction =
  -- TODO casts
  case instruction of
    CPSAssembly.Copy destination source size -> do
      destination' <- assembleOperand destination
      source' <- assembleOperand source
      size' <- assembleOperand size
      let
        memcpyGlob =
          LLVM.Constant.GlobalReference
            LLVM.FunctionType
              { LLVM.resultType = LLVM.Type.void
              , LLVM.argumentTypes =
                  [ wordPointer
                  , wordPointer
                  , wordSizedInt
                  , LLVM.Type.i32
                  , LLVM.Type.i1
                  ]
              , LLVM.isVarArg = False
              }
            (LLVM.Name $ "llvm.memcpy.p0i8.p0i8.i" <> fromString (show (wordBits :: Int)))
        arguments =
          [ destination'
          , source'
          , size'
          , LLVM.ConstantOperand $ LLVM.Constant.Int 32 alignment
          , LLVM.ConstantOperand $ LLVM.Constant.Int 1 0 -- isvolatile
          ]
      pure
        [ LLVM.Do
          LLVM.Call
            { tailCallKind = Nothing
            , callingConvention = LLVM.CallingConvention.C
            , returnAttributes = []
            , function = Right $ LLVM.ConstantOperand memcpyGlob
            , arguments = [(arg, []) | arg <- arguments]
            , functionAttributes = []
            , metadata = []
            }
        ]

    CPSAssembly.Load destination address -> do
      destination' <- activateLocal destination
      address' <- assembleOperand address
      pure
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
      address' <- assembleOperand address
      value' <- assembleOperand value
      pure
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
      destination' <- activateLocal destination
      operand1' <- assembleOperand operand1
      operand2' <- assembleOperand operand2
      pure
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
      destination' <- activateLocal destination
      operand1' <- assembleOperand operand1
      operand2' <- assembleOperand operand2
      pure
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

assembleOperand :: Assembly.Operand -> Assembler LLVM.Operand
assembleOperand operand =
  case operand of
    Assembly.LocalOperand l -> _
    Assembly.Global n -> _
    Assembly.Lit l -> _
