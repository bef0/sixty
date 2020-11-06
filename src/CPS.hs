{-# language GeneralizedNewtypeDeriving #-}

module CPS where

import qualified Assembly
import qualified CPSAssembly
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import qualified Name
import Protolude hiding (IntSet, IntMap)
import qualified Literal

data ConverterState = ConverterState
  { _fresh :: !Int
  , _baseDefinitionName :: !Name.Lifted
  , _nextDefinitionName :: !Int
  , _definitions :: Tsil CPSAssembly.Definition
  , _instructions :: Tsil CPSAssembly.Instruction
  , _stackPointer :: !Assembly.Local
  }

newtype Converter a = Converter { unCPS :: State ConverterState a }
  deriving (Functor, Applicative, Monad, MonadState ConverterState)

emitInstruction :: CPSAssembly.Instruction -> Converter ()
emitInstruction instruction =
  modify $ \s -> s { _instructions = _instructions s Tsil.:> instruction }

-------------------------------------------------------------------------------

wordSize :: Num a => a
wordSize = 8

freshFunctionName :: Converter LLVM.Name
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

freshLocal :: Converter Assembly.Local
freshLocal = do
  fresh <- gets _fresh
  modify $ \s -> s { _fresh = fresh + 1 }
  pure $ Assembly.Local fresh

push :: Assembly.Operand -> Converter ()
push operand = do
  stackPointer <- gets _stackPointer
  newStackPointer <- freshLocal
  emitInstruction $
    CPSAssembly.Sub
      newStackPointer
      (Assembly.LocalOperand stackPointer)
      (Assembly.Lit $ Literal.Integer wordSize)
  emitInstruction $
    CPSAssembly.Store
      (Assembly.LocalOperand newStackPointer)
      operand
  modify $ \s -> s { _stackPointer = newStackPointer }

pushLocal :: Assembly.Local -> Converter ()
pushLocal =
  push . Assembly.LocalOperand

pushLocals :: IntSet Assembly.Local -> Converter ()
pushLocals =
  mapM_ pushLocal . IntSet.toList

-------------------------------------------------------------------------------

convertDefinition :: Assembly.Definition Assembly.BasicBlock -> CPSAssembly.Definition
convertDefinition definition = _ 

convertBasicBlock :: IntSet Assembly.Local -> Assembly.BasicBlockWithOccurrences -> Converter ()
convertBasicBlock liveLocals basicBlock =
  case basicBlock of
    Assembly.Nil ->
      return ()

    Assembly.Cons _ instruction basicBlock' -> do
      convertInstruction (liveLocals <> Assembly.basicBlockOccurrences basicBlock') instruction
      convertBasicBlock liveLocals basicBlock'

convertInstruction
  :: IntSet Assembly.Local
  -> Assembly.Instruction Assembly.BasicBlockWithOccurrences
  -> Converter ()
convertInstruction liveLocals instr =
  case instr of
    Assembly.Copy o1 o2 o3 ->
      emitInstruction $ CPSAssembly.Copy o1 o2 o3

    Assembly.Call l o os ->
      pushLocals liveLocals
      continuationFunctionName <- freshFunctionName
      push $ Assembly.Global continuationFunctionName
      -- push continuation function name
      -- tail call function
      -- start new definition for the continuation
      -- pop locals
      undefined

    Assembly.CallVoid o os ->
      undefined

    Assembly.Load l o ->
      emitInstruction $ CPSAssembly.Load l o

    Assembly.Store o1 o2 ->
      emitInstruction $ CPSAssembly.Store o1 o2

    Assembly.Add l o1 o2 ->
      emitInstruction $ CPSAssembly.Add l o1 o2

    Assembly.Sub l o1 o2 ->
      emitInstruction $ CPSAssembly.Sub l o1 o2

    Assembly.PointerToInt l o ->
      emitInstruction $ CPSAssembly.PointerToInt l o

    Assembly.IntToPointer l o ->
      emitInstruction $ CPSAssembly.IntToPointer l o

    Assembly.HeapAllocate l o ->
      emitInstruction $ CPSAssembly.HeapAllocate l o

    Assembly.Switch o brs d ->
      undefined
