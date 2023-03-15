{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

-- Algebraic effect to emit C statements during a natural transformation.
module Statement where

import Control.Monad.Freer
import Control.Monad.Freer.State
import Language.C

type Statement = State ([CBlockItem])

push :: forall effs. Member (Statement) effs => CBlockItem -> Eff effs ()
push stat = modify (\lst -> stat : lst)

pop :: forall effs. Member (State [CBlockItem]) effs => Eff effs (Maybe CBlockItem)
pop = do
    s <- get @[CBlockItem]
    if null s
        then pure $ Nothing
        else do
            modify @[CBlockItem] tail
            pure $ Just (head s)

runStatement :: forall effs a. Eff (Statement : effs) a -> Eff effs (a, [CBlockItem])
runStatement eff = do
    (a, s) <- runState [] eff
    pure $ (a, reverse s)
