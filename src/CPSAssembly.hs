{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
module CPSAssembly where

import Assembly (Local, Operand)
import qualified Assembly 
import Data.Persist
import Data.Text.Prettyprint.Doc
import Protolude

data Instruction
  = Copy !Operand !Operand !Operand
  | Load !Local !Operand
  | Store !Operand !Operand
  | Add !Local !Operand !Operand
  | Sub !Local !Operand !Operand
  | PointerToInt !Local !Operand
  | IntToPointer !Local !Operand
  | HeapAllocate !Local !Operand
  deriving (Show, Generic, Hashable, Persist)

data Terminator
  = Switch !Operand [(Int, Terminator)] Terminator
  | TailCall !Operand [Operand]
  deriving (Show, Generic, Hashable, Persist)

data BasicBlock = BasicBlock [Instruction] !Terminator
  deriving (Show, Generic, Hashable, Persist)

type Definition = Assembly.Definition BasicBlock

instance Pretty Instruction where
  pretty instruction =
    case instruction of
      Copy dst src size ->
        voidInstr "copy" [dst, src, size]

      Load dst src ->
        returningInstr dst "load" [src]

      Store dst src ->
        voidInstr "store" [dst, src]

      Add dst arg1 arg2 ->
        returningInstr dst "add" [arg1, arg2]

      Sub dst arg1 arg2 ->
        returningInstr dst "sub" [arg1, arg2]

      PointerToInt dst arg ->
        returningInstr dst "ptrtoint" [arg]

      IntToPointer dst arg ->
        returningInstr dst "inttoptr" [arg]

      HeapAllocate dst size ->
        returningInstr dst "gcmalloc" [size]
    where
      voidInstr name args =
        name <+> hsep (pretty <$> args)

      returningInstr ret name args =
        pretty ret <+> "=" <+> voidInstr name args

instance Pretty Terminator where
  pretty terminator =
    case terminator of
      TailCall fun args ->
        "tail call" <+> hsep (pretty <$> fun : args)

      Switch scrutinee branches default_ ->
        "switch" <+> pretty scrutinee <> line <>
          indent 2
            (vcat
              [ pretty i <+> "->" <> line <>
                indent 2 (pretty basicBlock)
              | (i, basicBlock) <- branches
              ] <> line <>
              "_ -> " <> line <> indent 2 (pretty default_)
            )

instance Pretty BasicBlock where
  pretty (BasicBlock instrs terminator) =
    vcat $ (pretty <$> instrs) <> [pretty terminator]
