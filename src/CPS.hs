module CPS where

import qualified Assembly
import qualified CPSAssembly
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import qualified Name
import Protolude

data ConverterState = ConverterState
  { _baseDefinitionName :: !Name.Lifted
  , _nextDefinitionName :: !Int
  , _definitions :: Tsil CPSAssembly.Definition
  , _instructions :: Tsil CPSAssembly.Instruction
  }

newtype Converter a = Converter { unCPS :: State ConverterState a }

emitInstruction :: CPSAssembly.Instruction -> Converter ()
emitInstruction instruction =
  modify $ \s -> s { _instructions = _instructions s Tsil.:> instruction }

-------------------------------------------------------------------------------

convertDefinition :: Assembly.Definition Assembly.BasicBlock -> CPSAssembly.Definition
convertDefinition definition =
  _

convertBasicBlock
  :: Assembly.Instruction Assembly.BasicBlockWithOccurrences
  -> Converter CPSAssembly.BasicBlock
convertBasicBlock basicBlock =
  case basicBlock of
    Assembly.Nil ->
      undefined

    Assembly.Cons occurrences instruction basicBlock' ->
      undefined

convertInstruction
  :: Assembly.Instruction Assembly.BasicBlockWithOccurrences
  -> Converter ()
convertInstruction instr =
  case instr of
    Assembly.Copy o1 o2 o3 ->
      emitInstruction $ CPSAssembly.Copy o1 o2 o3

    Assembly.Call l o os ->
      undefined

    Assembly.CallVoid o os ->
      undefined

    Assembly.Load l o ->
      emitInstruction $ CPSAssembly.Copy l o

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
