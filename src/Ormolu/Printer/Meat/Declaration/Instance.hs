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
import Ormolu.Utils

p_instDecl :: InstDecl GhcPs -> R ()
p_instDecl = \case
  ClsInstD NoExt x -> p_clsInstD x
  _ -> error "Not implemented"

p_clsInstD :: ClsInstDecl GhcPs -> R ()
p_clsInstD = \case
  ClsInstDecl NoExt pty bds sigs tfins dfins ovpmd -> do
    txt "instance "
    p_implicitBndrs pty
  _ -> error "Not implemented"


p_implicitBndrs :: HsImplicitBndrs GhcPs (LHsType GhcPs) -> R ()
p_implicitBndrs = \case
  HsIB NoExt (L span tp) -> do
    p_hsType tp
  XHsImplicitBndrs _ -> error "XHsImplicitBndrs Not implemented"




