{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-incomplete-record-updates #-}

module Assembler where

import Protolude hiding (IntSet, IntMap, local, state)

import qualified Data.ByteString.Short as ShortByteString
import Data.String (fromString)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.AddrSpace as LLVM
import qualified LLVM.AST.CallingConvention as LLVM.CallingConvention
import qualified LLVM.AST.Constant as LLVM.Constant
import qualified LLVM.AST.Global as LLVM.Global
import qualified LLVM.AST.Type as LLVM.Type
import qualified LLVM.AST.Typed as LLVM

import qualified Assembly
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import qualified Data.Tsil as Tsil
import Data.Tsil (Tsil)
import qualified Literal
import Name (Name(Name))
import qualified Name

data BasicBlock
  = Nil
  | Cons (IntSet Assembly.Local) (Assembly.Instruction BasicBlock) BasicBlock

cons :: Assembly.Instruction BasicBlock -> BasicBlock -> BasicBlock
cons instruction basicBlock =
  Cons (instructionOccurrences instruction <> basicBlockOccurrences basicBlock) instruction basicBlock

basicBlockWithOccurrences :: Assembly.BasicBlock -> BasicBlock
basicBlockWithOccurrences (Assembly.BasicBlock instructions) =
  case instructions of
    [] ->
      Nil

    instruction : instructions' ->
      cons
        (basicBlockWithOccurrences <$> instruction)
        (basicBlockWithOccurrences $ Assembly.BasicBlock instructions')

basicBlockOccurrences :: BasicBlock -> IntSet Assembly.Local
basicBlockOccurrences basicBlock =
  case basicBlock of
    Nil ->
      mempty

    Cons occurrences _ _ ->
      occurrences

instructionOccurrences :: Assembly.Instruction BasicBlock -> IntSet Assembly.Local
instructionOccurrences instruction =
  case instruction of
    Assembly.Copy o1 o2 o3 ->
      operandOccurrences o1 <> operandOccurrences o2 <> operandOccurrences o3

    Assembly.Call l o os ->
      IntSet.singleton l <> operandOccurrences o <> foldMap operandOccurrences os

    Assembly.CallVoid o os ->
      operandOccurrences o <> foldMap operandOccurrences os

    Assembly.Load l o ->
      IntSet.singleton l <> operandOccurrences o

    Assembly.Store o1 o2 ->
      operandOccurrences o1 <> operandOccurrences o2

    Assembly.Add l o1 o2 ->
      IntSet.singleton l <> operandOccurrences o1 <> operandOccurrences o2

    Assembly.Sub l o1 o2 ->
      IntSet.singleton l <> operandOccurrences o1 <> operandOccurrences o2

    Assembly.PointerToInt l o ->
      IntSet.singleton l <> operandOccurrences o

    Assembly.IntToPointer l o ->
      IntSet.singleton l <> operandOccurrences o

    Assembly.HeapAllocate l o ->
      IntSet.singleton l <> operandOccurrences o

    Assembly.Switch o brs d ->
      operandOccurrences o <> foldMap (basicBlockOccurrences . snd) brs <> basicBlockOccurrences d

operandOccurrences :: Assembly.Operand -> IntSet Assembly.Local
operandOccurrences operand =
  case operand of
    Assembly.LocalOperand local ->
      IntSet.singleton local

    Assembly.Global _ ->
      mempty

    Assembly.Lit _ ->
      mempty

-------------------------------------------------------------------------------

data State = State
  { _baseDefinitionName :: !Name.Lifted
  , _nextDefinitionName :: !Int
  , _definitions :: Tsil LLVM.Definition
  , _finishDefinition :: [LLVM.BasicBlock] -> LLVM.Definition
  , _basicBlocks :: Tsil LLVM.BasicBlock
  , _finishBlock :: [LLVM.Named LLVM.Instruction] -> LLVM.Named LLVM.Terminator -> LLVM.BasicBlock
  , _basicBlock :: Tsil (LLVM.Named LLVM.Instruction)
  , _nextUnName :: !Word
  , _locals :: IntMap Assembly.Local LLVM.Operand
  , _stackPointer :: !Assembly.Local
  }

newtype Assembler a = Assembler { unAssembler :: Protolude.State Assembler.State a }
  deriving (Functor, Applicative, Monad, MonadState Assembler.State)

activateLocal :: Assembly.Local -> LLVM.Operand -> Assembler ()
activateLocal local operand =
  modify $ \s -> s { _locals = IntMap.insert local operand $ _locals s }

emitInstruction :: LLVM.Type -> LLVM.Instruction -> Assembler LLVM.Operand
emitInstruction returnType instruction = do
  nameNumber <- gets _nextUnName
  let
    name =
      LLVM.UnName nameNumber
  modify $ \s -> s
    { _basicBlock = _basicBlock s Tsil.:> (name LLVM.:= instruction)
    , _nextUnName = nameNumber + 1
    }
  pure $ LLVM.LocalReference returnType name

emitVoidInstruction :: LLVM.Instruction -> Assembler ()
emitVoidInstruction instruction = do
  modify $ \s -> s
    { _basicBlock = _basicBlock s Tsil.:> LLVM.Do instruction
    }

startNewBlock :: LLVM.Name -> Assembler ()
startNewBlock name =
  modify $ \s -> s
    { _finishBlock = LLVM.BasicBlock name
    }

finishDefinition :: Assembler ()
finishDefinition =
  modify $ \s -> s
    { _definitions = _definitions s Tsil.:> _finishDefinition s (toList $ _basicBlocks s)
    , _finishDefinition = panic "finishDefinition: Definition is finished"
    }

terminateBlock :: LLVM.Named LLVM.Terminator -> Assembler ()
terminateBlock terminator =
  modify $ \s -> s
    { _finishBlock = panic "terminateBlock: Block is terminated"
    , _basicBlocks = _basicBlocks s Tsil.:> _finishBlock s (toList $ _basicBlock s) terminator
    , _basicBlock = mempty
    }

freshUnName :: Assembler LLVM.Name
freshUnName = do
  i <- gets _nextUnName
  modify $ \s -> s { _nextUnName = i + 1 }
  return $ LLVM.UnName i

freshFunctionName :: Assembler LLVM.Name
freshFunctionName = do
  base <- gets _baseDefinitionName
  next <- gets _nextDefinitionName
  modify $ \s -> s { _nextDefinitionName = next + 1 }
  let
    textBase =
      case base of
        Name.Lifted (Name.Qualified (Name.Module module_) (Name name)) 0 ->
          module_ <> "." <> name

        Name.Lifted (Name.Qualified (Name.Module module_) (Name name)) n ->
          module_ <> "." <> name <> "$" <> show n

  return $ LLVM.Name $ ShortByteString.toShort $ toUtf8 $ textBase <> "$$" <> show next

localOperand :: Assembly.Local -> Assembler LLVM.Operand
localOperand local =
  gets $ fromMaybe (panic "Assembler.localOperand") . IntMap.lookup local . _locals

-------------------------------------------------------------------------------

push :: LLVM.Operand -> Assembler ()
push value = do
  stackPointerLocal <- gets _stackPointer
  stackPointer <- localOperand stackPointerLocal
  stackPointer' <- emitInstruction (LLVM.typeOf stackPointer)
    LLVM.GetElementPtr
      { inBounds = True
      , address = stackPointer
      , indices = [LLVM.ConstantOperand $ LLVM.Constant.Int 64 (negate 8)]
      , metadata = []
      }
  store stackPointer' value
  activateLocal stackPointerLocal stackPointer'

pushLocal :: Assembly.Local -> Assembler ()
pushLocal local = do
  value <- localOperand local
  push value

pushLocals :: IntSet Assembly.Local -> Assembler ()
pushLocals =
  mapM_ pushLocal . IntSet.toList

pop :: Assembler ()
pop = do
  stackPointerLocal <- gets _stackPointer
  stackPointer <- localOperand stackPointerLocal
  stackPointer' <- emitInstruction (LLVM.typeOf stackPointer)
    LLVM.GetElementPtr
      { inBounds = True
      , address = stackPointer
      , indices = [LLVM.ConstantOperand $ LLVM.Constant.Int 64 8]
      , metadata = []
      }
  store stackPointer' $ LLVM.ConstantOperand $ LLVM.Constant.Undef $ LLVM.typeOf stackPointer'
  activateLocal stackPointerLocal stackPointer'

popLocals :: IntSet Assembly.Local -> Assembler ()
popLocals locals =
  replicateM_ (IntSet.size locals) pop

pushContinuationFunction :: LLVM.Name -> Assembler ()
pushContinuationFunction name =
  -- TODO type
  push $ LLVM.ConstantOperand $ LLVM.Constant.GlobalReference wordPointer name

startNewContinuationFunction :: LLVM.Name -> Assembly.Local -> Assembler ()
startNewContinuationFunction name result =
  modify $ \s -> s
    { _finishDefinition = \basicBlocks ->
      LLVM.GlobalDefinition
      LLVM.Global.functionDefaults
        { LLVM.Global.callingConvention = LLVM.CallingConvention.GHC
        , LLVM.Global.returnType = LLVM.VoidType
        , LLVM.Global.name = name
        , LLVM.Global.parameters = ([LLVM.Parameter wordPointer (LLVM.UnName 0) []], False)
        , LLVM.Global.alignment = alignment
        , LLVM.Global.basicBlocks = basicBlocks
        }
    , _basicBlocks = mempty
    , _nextUnName = 1
    , _locals =
      IntMap.fromList
        [ (local, LLVM.LocalReference wordPointer $ LLVM.UnName i)
        | (i, local) <- zip [0..] [_stackPointer s, result]
        ]
    }

startNewVoidContinuationFunction :: LLVM.Name -> Assembler ()
startNewVoidContinuationFunction name =
  modify $ \s -> s
    { _finishDefinition = \basicBlocks ->
      LLVM.GlobalDefinition
      LLVM.Global.functionDefaults
        { LLVM.Global.callingConvention = LLVM.CallingConvention.GHC
        , LLVM.Global.returnType = LLVM.VoidType
        , LLVM.Global.name = name
        , LLVM.Global.parameters = ([], False)
        , LLVM.Global.alignment = alignment
        , LLVM.Global.basicBlocks = basicBlocks
        }
    , _basicBlocks = mempty
    , _nextUnName = 1
    , _locals =
      IntMap.fromList
        [ (local, LLVM.LocalReference wordPointer $ LLVM.UnName i)
        | (i, local) <- zip [0..] [_stackPointer s]
        ]
    }

tailCall :: LLVM.Operand -> [LLVM.Operand] -> Assembler ()
tailCall function arguments =
  emitVoidInstruction
    LLVM.Call
      { tailCallKind = Just LLVM.MustTail
      , callingConvention = LLVM.CallingConvention.GHC
      , returnAttributes = []
      , function = Right function
      , arguments = [(arg, []) | arg <- arguments]
      , functionAttributes = []
      , metadata = []
      }

-------------------------------------------------------------------------------

localBlockFunction :: IntSet Assembly.Local -> Assembler () -> Assembler (Assembler ())
localBlockFunction locals body = do
  let
    localsList =
      IntSet.toList locals
  name <- freshFunctionName
  stateBefore <- get
  startNewBlockFunction name localsList
  body
  stateAfter <- get
  put
    State
      { _baseDefinitionName = _baseDefinitionName stateBefore
      , _nextDefinitionName = _nextDefinitionName stateAfter
      , _definitions = _definitions stateAfter
      , _finishDefinition = _finishDefinition stateBefore
      , _basicBlocks = _basicBlocks stateBefore
      , _finishBlock = _finishBlock stateBefore
      , _basicBlock = _basicBlock stateBefore
      , _nextUnName = _nextUnName stateBefore
      , _locals = _locals stateBefore
      , _stackPointer = _stackPointer stateBefore
      }
  pure $ callBlockFunction name localsList

startNewBlockFunction :: LLVM.Name -> [Assembly.Local] -> Assembler ()
startNewBlockFunction name locals = do
  let
    localsCount =
      fromIntegral $ length locals
  modify $ \s -> s
    { _finishDefinition = \basicBlocks ->
      LLVM.GlobalDefinition
      LLVM.Global.functionDefaults
        { LLVM.Global.callingConvention = LLVM.CallingConvention.GHC
        , LLVM.Global.returnType = LLVM.VoidType
        , LLVM.Global.name = name
        , LLVM.Global.parameters =
          ( [ LLVM.Parameter wordPointer (LLVM.UnName i) []
            | i <- [0..localsCount]
            ]
          , False
          )
        , LLVM.Global.alignment = alignment
        , LLVM.Global.basicBlocks = basicBlocks
        }
    , _basicBlocks = mempty
    , _nextUnName = localsCount
    , _locals =
      IntMap.fromList
        [ (local, LLVM.LocalReference wordPointer $ LLVM.UnName i)
        | (i, local) <- zip [0..] $ _stackPointer s : locals
        ]
    }

callBlockFunction :: LLVM.Name -> [Assembly.Local] -> Assembler ()
callBlockFunction name locals = do
  let
    function =
      LLVM.ConstantOperand $
      LLVM.Constant.GlobalReference
        LLVM.FunctionType
          { LLVM.resultType = LLVM.Type.void
          , LLVM.argumentTypes = const wordPointer <$> locals
          , LLVM.isVarArg = False
          }
        name
  arguments <- mapM localOperand locals
  tailCall function arguments

-------------------------------------------------------------------------------

unreachable :: LLVM.Named LLVM.Terminator
unreachable =
  LLVM.Do LLVM.Unreachable { metadata' = mempty }

add :: LLVM.Operand -> LLVM.Operand -> Assembler LLVM.Operand
add a b =
  emitInstruction (LLVM.typeOf a)
    LLVM.Add
      { nsw = False
      , nuw = False
      , operand0 = a
      , operand1 = b
      , metadata = []
      }

sub :: LLVM.Operand -> LLVM.Operand -> Assembler LLVM.Operand
sub a b =
  emitInstruction (LLVM.typeOf a)
    LLVM.Sub
      { nsw = False
      , nuw = False
      , operand0 = a
      , operand1 = b
      , metadata = []
      }

load :: LLVM.Operand -> Assembler LLVM.Operand
load a =
  emitInstruction returnType
    LLVM.Load
      { volatile = False
      , address = a
      , maybeAtomicity = Nothing
      , alignment = alignment
      , metadata = []
      }
  where
    returnType =
      case LLVM.typeOf a of
        LLVM.Type.PointerType ty _ ->
          ty

        _ ->
          panic "Cannot load non-pointer (Malformed AST)."

inttoptr :: LLVM.Operand -> Assembler LLVM.Operand
inttoptr a =
  emitInstruction wordPointer
    LLVM.IntToPtr
      { operand0 = a
      , type' = wordPointer
      , metadata = []
      }

ptrtoint :: LLVM.Operand -> Assembler LLVM.Operand
ptrtoint a =
  emitInstruction wordSizedInt
    LLVM.PtrToInt
      { operand0 = a
      , type' = wordSizedInt
      , metadata = []
      }

store :: LLVM.Operand -> LLVM.Operand -> Assembler ()
store destination value =
  emitVoidInstruction
    LLVM.Store
      { volatile = False
      , address = destination
      , value = value
      , maybeAtomicity = Nothing
      , alignment = alignment
      , metadata = []
      }

memcpy :: LLVM.Operand -> LLVM.Operand -> LLVM.Operand -> Assembler ()
memcpy destination source size = do
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
  let
    arguments =
      [ destination
      , source
      , size
      , LLVM.ConstantOperand $ LLVM.Constant.Int 32 alignment
      , LLVM.ConstantOperand $ LLVM.Constant.Int 1 0 -- isvolatile
      ]
  emitVoidInstruction
    LLVM.Call
      { tailCallKind = Nothing
      , callingConvention = LLVM.CallingConvention.C
      , returnAttributes = []
      , function = Right $ LLVM.ConstantOperand memcpyGlob
      , arguments = [(arg, []) | arg <- arguments]
      , functionAttributes = []
      , metadata = []
      }

-------------------------------------------------------------------------------
-- Constants
wordSizedInt :: LLVM.Type
wordSizedInt =
  LLVM.Type.i64

wordBits :: Num a => a
wordBits =
  64

pointerBytes :: Num a => a
pointerBytes =
  8

wordPointer :: LLVM.Type
wordPointer =
  LLVM.Type.PointerType
    { pointerReferent = LLVM.Type.i8
    , pointerAddrSpace = LLVM.AddrSpace 0
    }

alignment :: Num a => a
alignment =
  8

-------------------------------------------------------------------------------

assembleDefinition :: Name.Lifted -> Assembly.Definition Assembly.BasicBlock -> (LLVM.Name, [LLVM.Definition])
assembleDefinition name definition =
  flip evalState State
    { _baseDefinitionName = name
    , _nextDefinitionName = 0
    , _definitions = mempty
    , _finishDefinition = panic "assembleDefinition: finishDefition"
    , _basicBlocks = panic "assembleDefinition: basicBlocks"
    , _finishBlock = panic "assembleDefinition: finishBlock"
    , _basicBlock = mempty
    , _nextUnName = 0
    , _locals = mempty
    , _stackPointer = Assembly.Local 0
    } $
  unAssembler $
  case definition of
    Assembly.ConstantDefinition stackPointer _ -> do
      undefined

    Assembly.FunctionDefinition stackPointer arguments basicBlock -> do
      name' <- freshFunctionName
      modify $ \s -> s
        { _stackPointer = stackPointer
        }
      startNewBlockFunction name' arguments
      startNewBlock =<< freshUnName
      assembleBasicBlock mempty $ basicBlockWithOccurrences basicBlock
      terminateBlock unreachable
      finishDefinition
      definitions <- gets _definitions
      return (name', toList definitions)

assembleBasicBlock :: IntSet Assembly.Local -> BasicBlock -> Assembler ()
assembleBasicBlock liveLocals basicBlock =
  case basicBlock of
    Nil ->
      return ()

    Cons _ instruction basicBlock' -> do
      assembleInstruction (liveLocals <> basicBlockOccurrences basicBlock') instruction
      assembleBasicBlock liveLocals basicBlock'

assembleInstruction :: IntSet Assembly.Local -> Assembly.Instruction BasicBlock -> Assembler ()
assembleInstruction liveLocals instruction =
  case instruction of
    Assembly.Copy destination source size -> do
      destination' <- assembleOperand destination
      source' <- assembleOperand source
      size' <- assembleOperand size
      memcpy destination' source' size'

    Assembly.Call destination function arguments -> do
      pushLocals liveLocals
      continuationFunctionName <- freshFunctionName
      pushContinuationFunction continuationFunctionName
      function' <- assembleOperand function
      arguments' <- mapM assembleOperand arguments
      tailCall function' arguments'
      terminateBlock unreachable
      finishDefinition
      startNewContinuationFunction continuationFunctionName destination
      popLocals liveLocals

    Assembly.CallVoid function arguments -> do
      pushLocals liveLocals
      continuationFunctionName <- freshFunctionName
      pushContinuationFunction continuationFunctionName
      function' <- assembleOperand function
      arguments' <- mapM assembleOperand arguments
      tailCall function' arguments'
      terminateBlock unreachable
      finishDefinition
      startNewVoidContinuationFunction continuationFunctionName
      popLocals liveLocals

    Assembly.Load destination operand -> do
      operand' <- assembleOperand operand
      result <- load operand'
      activateLocal destination result

    Assembly.Store operand1 operand2 -> do
      operand1' <- assembleOperand operand1
      operand2' <- assembleOperand operand2
      store operand1' operand2'

    Assembly.Add destination operand1 operand2 -> do
      operand1' <- assembleOperand operand1
      operand2' <- assembleOperand operand2
      result <- add operand1' operand2'
      activateLocal destination result

    Assembly.Sub destination operand1 operand2 -> do
      operand1' <- assembleOperand operand1
      operand2' <- assembleOperand operand2
      result <- sub operand1' operand2'
      activateLocal destination result

    Assembly.PointerToInt destination operand -> do
      operand' <- assembleOperand operand
      result <- ptrtoint operand'
      activateLocal destination result

    Assembly.IntToPointer destination operand -> do
      operand' <- assembleOperand operand
      result <- inttoptr operand'
      activateLocal destination result

    Assembly.HeapAllocate l o ->
      undefined

    Assembly.Switch scrutinee branches default_ -> do
      scrutinee' <- assembleOperand scrutinee
      labelledBranches <- forM branches $ \(int, branch) -> do
        label <- freshUnName
        pure (int, label, branch)
      defaultLabel <- freshUnName
      let
        switch =
          LLVM.Do LLVM.Switch
            { operand0' = scrutinee'
            , defaultDest = defaultLabel
            , dests = [(LLVM.Constant.Int wordBits $ fromIntegral int, label) | (int, label, _) <- labelledBranches]
            , metadata' = []
            }
      let
        liveLocalsList =
          IntSet.toList liveLocals
      continuationFunctionName <- freshFunctionName
      callDefault <- localBlockFunction (liveLocals <> basicBlockOccurrences default_) $ do
        startNewBlock =<< freshUnName
        assembleBasicBlock liveLocals default_
        callBlockFunction continuationFunctionName liveLocalsList
        terminateBlock unreachable
        finishDefinition
      terminateBlock switch
      startNewBlock defaultLabel
      callDefault
      terminateBlock unreachable
      forM_ labelledBranches $ \(_, label, branch) -> do
        callBranch <- localBlockFunction (liveLocals <> basicBlockOccurrences branch) $ do
          startNewBlock =<< freshUnName
          assembleBasicBlock liveLocals branch
          callBlockFunction continuationFunctionName liveLocalsList
          terminateBlock unreachable
          finishDefinition
        startNewBlock label
        callBranch
        terminateBlock unreachable
      finishDefinition
      startNewBlockFunction continuationFunctionName liveLocalsList

assembleOperand :: Assembly.Operand -> Assembler LLVM.Operand
assembleOperand operand =
  case operand of
    Assembly.LocalOperand local ->
      localOperand local

    Assembly.Global name ->
      undefined

    Assembly.Lit lit ->
      pure $
        case lit of
          Literal.Integer int ->
            LLVM.ConstantOperand $ LLVM.Constant.Int wordBits int
