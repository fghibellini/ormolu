{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Typeclass instance declarations.

module Ormolu.Printer.Meat.Declaration.Instance
  ( p_instDecl
  )
where

import GHC
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type
import Ormolu.Printer.Meat.Declaration.Value
import Ormolu.Utils
import Data.Foldable (traverse_)

p_instDecl :: InstDecl GhcPs -> R ()
p_instDecl = \case
  ClsInstD NoExt x -> p_clsInstD x
  _ -> error "Not implemented"

p_clsInstD :: ClsInstDecl GhcPs -> R ()
p_clsInstD = \case
  ClsInstDecl NoExt pty bds sigs tfins dfins ovpmd -> do
    txt "instance "
    p_implicitBndrs pty
    txt " where "
    p_binds bds
  _ -> error "Not implemented"


p_implicitBndrs :: HsImplicitBndrs GhcPs (LHsType GhcPs) -> R ()
p_implicitBndrs = \case
  HsIB NoExt (L span tp) -> do
    p_hsType tp -- TODO here it's not any type, but the "inst" rule from https://www.haskell.org/onlinereport/haskell2010/haskellch10.html
  XHsImplicitBndrs _ -> error "XHsImplicitBndrs Not implemented"

p_binds :: LHsBinds GhcPs -> R ()
p_binds = traverse_ p_bind

p_bind :: LHsBindLR GhcPs GhcPs -> R ()
p_bind (L _ b) = p_valDecl b


