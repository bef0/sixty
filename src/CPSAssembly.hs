{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
module CPSAssembly where

import Assembly (Local, Operand)
import qualified Assembly 
import Protolude
import Data.Persist

data Instruction
  = Copy !Operand !Operand !Operand
  | Load !Local !Operand
  | Store !Operand !Operand
  | Add !Local !Operand !Operand
  | Sub !Local !Operand !Operand
  | PointerToInt !Local !Operand
  | IntToPointer !Local !Operand
  | HeapAllocate !Local !Operand
  deriving (Show, Generic, Persist, Hashable)

data Terminator
  = Switch !Operand [(Int, Terminator)] Terminator
  | TailCall !Operand [Operand]
  deriving (Show, Generic, Persist, Hashable)

data BasicBlock = BasicBlock [Instruction] !Terminator
  deriving (Show, Generic, Persist, Hashable)

type Definition = Assembly.Definition BasicBlock
