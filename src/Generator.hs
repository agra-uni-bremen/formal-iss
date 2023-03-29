{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Generator (generate, evalE) where

import Bindings
import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Data.Maybe
import Executor
import Interface qualified as IF
import Language.C
import LibRISCV.Decoder.Opcode (InstructionType)
import LibRISCV.Spec.AST (instrSemantics)
import LibRISCV.Spec.Expr qualified as E
import LibRISCV.Spec.Operations hiding ((>>))
import Statement qualified as S
import Util

-- Assumption: CExpr is an unsigned value in two's complement (uint32_t).
evalE :: Bindings -> E.Expr CExpr -> CExpr
evalE _ (E.FromImm e) = e
evalE _ (E.FromUInt v) =
    let cint = CInteger (fromIntegral v) HexRepr noFlags
     in CConst (CIntConst cint undefNode)
evalE b (E.ZExtByte v) = castTo (IF.uint32 b) $ castTo (IF.uint8 b) (evalE b v)
evalE b (E.ZExtHalf v) = castTo (IF.uint32 b) $ castTo (IF.uint16 b) (evalE b v)
evalE b (E.SExtByte v) = do
    let i8 = castTo (IF.int8 b) (evalE b v)
    let i32 = castTo (IF.int32 b) i8
    castTo (IF.uint32 b) i32
evalE b (E.SExtHalf v) = do
    let i16 = castTo (IF.int16 b) (evalE b v)
    let i32 = castTo (IF.int32 b) i16
    castTo (IF.uint32 b) i32
evalE b (E.Add e1 e2) = CBinary CAddOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Sub e1 e2) = CBinary CSubOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Eq e1 e2) = CBinary CEqOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Slt e1 e2) =
    CBinary
        CLeOp
        (castTo (IF.int32 b) (evalE b e1))
        (castTo (IF.int32 b) (evalE b e2))
        undefNode
evalE b (E.Sge e1 e2) =
    CBinary
        CGeqOp
        (castTo (IF.int32 b) (evalE b e1))
        (castTo (IF.int32 b) (evalE b e2))
        undefNode
evalE b (E.Ult e1 e2) = CBinary CLeOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Uge e1 e2) = CBinary CGeqOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.And e1 e2) = CBinary CAndOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Or e1 e2) = CBinary COrOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.Xor e1 e2) = CBinary CXorOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.LShl e1 e2) = CBinary CShlOp (evalE b e1) (evalE b e2) undefNode
evalE b (E.LShr e1 e2) = CBinary CShrOp (evalE b e1) (evalE b e2) undefNode
-- XXX: This assumes that the compiler implements shift on signed values
-- using arithmetic shift which is true for GCC/clang but not standard-compliant.
evalE b (E.AShr e1 e2) = CBinary CShrOp (castTo (IF.int32 b) (evalE b e1)) (evalE b e2) undefNode

------------------------------------------------------------------------

-- Helper function for loading values from memory.
doLoad :: IF.ValueSize -> CExpr -> E.Expr CExpr -> Bindings -> CExpr
doLoad size core addr curBinds = IF.load size curBinds core (evalE curBinds addr)

-- Helper function for storing values in memory.
doStore :: IF.ValueSize -> CExpr -> E.Expr CExpr -> E.Expr CExpr -> Bindings -> CExpr
doStore size core addr value curBinds = IF.store size curBinds core (evalE curBinds addr) (evalE curBinds value)

-- Generate C code for the abstract semantics description of a given instruction from LibRISCV.
-- Since C is an imperative programming language we need to be able to emit both statements
-- and expressions. This is achieved by emitting statements as a "side effect" through
-- an algebraic State effect.
--
-- The following invariants need to hold for each Operations handler:
--
--   1. Each handler can only emit a maximum of one statement.
--   2. Handlers for `Operations ()` must emit exactly one statement.
--
-- Failure to satisfy these invariants will result in a runtime error.
buildSemantics :: CExpr -> Bindings -> Eff '[Operations CExpr] w -> [CBlockItem]
buildSemantics core binds req = snd $ run (S.runStatement (runReader binds (reinterpret2 gen req)))
  where
    gen :: Operations CExpr ~> Eff '[Reader Bindings, S.Statement]
    gen (DecodeRD instr) = IF.instrPart IF.RD instr <$> ask
    gen (DecodeRS1 instr) = IF.instrPart IF.RS1 instr <$> ask
    gen (DecodeRS2 instr) = IF.instrPart IF.RS2 instr <$> ask
    gen (DecodeImmI instr) = IF.instrPart IF.ImmI instr <$> ask
    gen (DecodeImmS instr) = IF.instrPart IF.ImmS instr <$> ask
    gen (DecodeImmB instr) = IF.instrPart IF.ImmB instr <$> ask
    gen (DecodeImmU instr) = IF.instrPart IF.ImmU instr <$> ask
    gen (DecodeImmJ instr) = IF.instrPart IF.ImmJ instr <$> ask
    gen (DecodeShamt instr) = IF.instrPart IF.Shamt instr <$> ask
    gen (RunIf expr ifTrue) = do
        gen ifTrue
        trueBlock <- S.pop
        let block = case fromJust trueBlock of
                CBlockStmt c@(CCompound{}) -> c
                e -> CCompound [] [e] undefNode

        curBinds <- ask
        let cond = evalE curBinds expr
        let ifStat = CIf cond block Nothing undefNode

        S.push (CBlockStmt ifStat)
    gen (RunUnless expr unlessTrue) = do
        gen unlessTrue
        unlessBlock <- S.pop
        let block = case fromJust unlessBlock of
                CBlockStmt c@(CCompound{}) -> c
                e -> CCompound [] [e] undefNode

        curBinds <- ask
        let cond = evalE curBinds expr
        let ifStat =
                CIf
                    cond
                    (CCompound [] [] undefNode)
                    (Just block)
                    undefNode

        S.push (CBlockStmt ifStat)
    gen (ReadRegister idx) = ask >>= \b -> pure $ IF.readReg b core idx
    gen (WriteRegister idx val) = do
        curBinds <- ask
        let expr = IF.writeReg curBinds core idx (evalE curBinds val)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (LoadByte addr) = doLoad IF.Byte core addr <$> ask
    gen (LoadHalf addr) = doLoad IF.Half core addr <$> ask
    gen (LoadWord addr) = doLoad IF.Word core addr <$> ask
    gen (StoreByte addr value) = do
        expr <- doStore IF.Byte core addr value <$> ask
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (StoreHalf addr value) = do
        expr <- doStore IF.Half core addr value <$> ask
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (StoreWord addr value) = do
        expr <- doStore IF.Word core addr value <$> ask
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen ReadPC = do
        curBinds <- ask
        let pc = IF.readPC curBinds core

        let ident = getIdent "link" curBinds
        let declr = CDeclr (Just ident) [] Nothing [] undefNode
        let var = CDecl [IF.uint32 curBinds] [(Just declr, Just (CInitExpr pc undefNode), Nothing)] undefNode

        S.push (CBlockDecl var)
        pure $ CVar ident undefNode
    gen (WritePC value) = do
        curBinds <- ask
        let expr = IF.writePC binds core (evalE curBinds value)
        S.push (CBlockStmt $ CExpr (Just expr) undefNode)
    gen (Exception _ _) = do
        curBinds <- ask
        let abort = funcCall (getIdent "abort" curBinds) []
        S.push (CBlockStmt $ CExpr (Just abort) undefNode)
    gen (Ebreak _) = pure ()
    gen (Ecall _) = pure ()
    gen (Append__ o1 o2) = do
        s1 <- gen o1 >> S.pop
        e2 <- gen o2
        s2 <- S.pop

        S.push (CBlockStmt $ CCompound [] [fromJust s1, fromJust s2] undefNode)
        pure e2

generate' :: Bindings -> [Name] -> InstructionType -> (CFunDef, [Name])
generate' binds (nFunc : nCore : nInstr : nPC : ns) inst = (makeExecutor funcIdent funcArgs block, ns)
  where
    -- Identifier for the generated function itself.
    funcIdent :: Ident
    funcIdent = mkIdent nopos ("exec_" ++ foldcase (show inst)) nFunc

    -- Identifier for the core argument of the executor functions.
    coreIdent :: Ident
    coreIdent = mkIdent nopos "core" nCore

    -- Identifier for the instruction argument of the function.
    instrIdent :: Ident
    instrIdent = mkIdent nopos "instr" nInstr

    -- Identifier for the current program counter.
    pcIdent :: Ident
    pcIdent = mkIdent nopos "instrPC" nPC

    -- Function arguments for the executor.
    funcArgs :: [CDecl]
    funcArgs = [voidPtrArg coreIdent, pcArg binds pcIdent, voidPtrArg instrIdent]

    cflow :: Eff '[Operations CExpr] ()
    cflow = instrSemantics @CExpr (CVar pcIdent undefNode) (CVar instrIdent undefNode) inst

    block :: CStat
    block = CCompound [] (buildSemantics (CVar coreIdent undefNode) binds cflow) undefNode
generate' _ _ _ = error "invalid name list"

generate :: [InstructionType] -> [CFunDef]
generate types = snd $ foldl fn (initialNs, []) types
  where
    -- Generate a CFunDef for each instruction and propagate the name supply.
    fn (ns, funcs) ty = let (f, newNs) = generate' bindings ns ty in (newNs, f : funcs)

    -- Bindings for identifiers and initial name supply.
    (bindings, initialNs) = mkBindings newNameSupply
