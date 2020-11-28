{-# language DeriveAnyClass #-}
{-# language DeriveFunctor #-}
{-# language DeriveGeneric #-}
{-# language GADTs #-}
{-# language OverloadedStrings #-}
module Assembly where

import Protolude hiding (local, IntSet)

import Data.Persist
import Data.Text.Prettyprint.Doc

import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Literal (Literal)
import qualified Name

newtype Local = Local Int
  deriving (Eq, Ord, Show, Generic, Persist, Hashable)

data Name = Name !Name.Lifted !Int
  deriving (Show, Generic, Persist, Hashable)

instance Pretty Name where
  pretty (Name name 0) =
    pretty name

  pretty (Name name n) =
    pretty name <> "$" <> pretty n

data Operand
  = LocalOperand !Local
  | Global !Name
  | Lit !Literal
  deriving (Show, Generic, Persist, Hashable)

data Instruction basicBlock
  = Copy !Operand !Operand !Operand
  | Call !Local !Operand [Operand]
  | CallVoid !Operand [Operand]
  | Load !Local !Operand
  | Store !Operand !Operand
  | Add !Local !Operand !Operand
  | Sub !Local !Operand !Operand
  | StackAllocate !Local !Operand
  | StackDeallocate !Operand
  | HeapAllocate !Local !Operand
  | Switch !Operand [(Int, basicBlock)] basicBlock
  deriving (Show, Generic, Persist, Hashable, Functor)

data Definition basicBlock
  = ConstantDefinition basicBlock
  | FunctionDefinition [Local] basicBlock
  deriving (Show, Generic, Persist, Hashable, Functor)

type StackPointer = Local

newtype BasicBlock = BasicBlock [Instruction BasicBlock]
  deriving (Show, Generic, Persist, Hashable)

instance Pretty Local where
  pretty (Local i) =
    "%" <> pretty i

instance Pretty Operand where
  pretty operand =
    case operand of
      LocalOperand local ->
        pretty local

      Global global ->
        pretty global

      Lit lit ->
        pretty lit

instance Pretty basicBlock => Pretty (Instruction basicBlock) where
  pretty instruction =
    case instruction of
      Copy dst src size ->
        voidInstr "copy" [dst, src, size]

      Call dst fun args ->
        returningInstr dst "call" (fun : args)

      CallVoid fun args ->
        voidInstr "call" (fun : args)

      Load dst src ->
        returningInstr dst "load" [src]

      Store dst src ->
        voidInstr "store" [dst, src]

      Add dst arg1 arg2 ->
        returningInstr dst "add" [arg1, arg2]

      Sub dst arg1 arg2 ->
        returningInstr dst "sub" [arg1, arg2]

      StackAllocate dst size ->
        returningInstr dst "alloca" [size]

      StackDeallocate size ->
        voidInstr "dealloca" [size]

      HeapAllocate dst size ->
        returningInstr dst "gcmalloc" [size]

      Switch scrutinee branches default_ ->
        "switch" <+> pretty scrutinee <> line <>
          indent 2
            (vsep
              [ pretty i <+> "->" <> line <>
                indent 2 (pretty basicBlock)
              | (i, basicBlock) <- branches
              ] <> line <>
              "_ -> " <> line <> indent 2 (pretty default_)
            )

    where
      voidInstr name args =
        name <+> hsep (pretty <$> args)

      returningInstr ret name args =
        pretty ret <+> "=" <+> voidInstr name args

instance Pretty basicBlock => Pretty (Definition basicBlock) where
  pretty definition =
    case definition of
      ConstantDefinition basicBlock ->
        indent 2 (pretty basicBlock)

      FunctionDefinition args basicBlock ->
        tupled (pretty <$> args) <+> "->" <> line <>
          indent 2 (pretty basicBlock)

instance Pretty BasicBlock where
  pretty (BasicBlock instrs) =
    vsep $ pretty <$> instrs

-------------------------------------------------------------------------------

data BasicBlockWithOccurrences
  = Nil
  | Cons (IntSet Assembly.Local) (Assembly.Instruction BasicBlockWithOccurrences) BasicBlockWithOccurrences

cons :: Assembly.Instruction BasicBlockWithOccurrences -> BasicBlockWithOccurrences -> BasicBlockWithOccurrences
cons instruction basicBlock =
  Cons (instructionOccurrences instruction <> basicBlockOccurrences basicBlock) instruction basicBlock

basicBlockWithOccurrences :: Assembly.BasicBlock -> BasicBlockWithOccurrences
basicBlockWithOccurrences (Assembly.BasicBlock instructions) =
  case instructions of
    [] ->
      Nil

    instruction : instructions' ->
      cons
        (basicBlockWithOccurrences <$> instruction)
        (basicBlockWithOccurrences $ Assembly.BasicBlock instructions')

basicBlockOccurrences :: BasicBlockWithOccurrences -> IntSet Assembly.Local
basicBlockOccurrences basicBlock =
  case basicBlock of
    Nil ->
      mempty

    Cons occurrences _ _ ->
      occurrences

instructionOccurrences :: Assembly.Instruction BasicBlockWithOccurrences -> IntSet Assembly.Local
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

    Assembly.StackAllocate l o ->
      IntSet.singleton l <> operandOccurrences o

    Assembly.StackDeallocate o ->
      operandOccurrences o

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
