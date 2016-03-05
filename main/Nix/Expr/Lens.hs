{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TemplateHaskell           #-}

module Nix.Expr.Lens where

import qualified Nix.Expr.Lens.Internal as L

import           Control.Lens.TH
import           Data.Fix
import           Nix.Expr

_NConstant    = L._Fix . L._NConstant
_NStr         = L._Fix . L._NStr
_NSym         = L._Fix . L._NSym
_NList        = L._Fix . L._NList
_NSet         = L._Fix . L._NSet
_NRecSet      = L._Fix . L._NRecSet
_NLiteralPath = L._Fix . L._NLiteralPath
_NEnvPath     = L._Fix . L._NEnvPath
_NUnary       = L._Fix . L._NUnary
_NBinary      = L._Fix . L._NBinary
_NSelect      = L._Fix . L._NSelect
_NHasAttr     = L._Fix . L._NHasAttr
_NAbs         = L._Fix . L._NAbs
_NApp         = L._Fix . L._NApp
_NLet         = L._Fix . L._NLet
_NIf          = L._Fix . L._NIf
_NWith        = L._Fix . L._NWith
_NAssert      = L._Fix . L._NAssert

makePrisms ''Fix
makePrisms ''Binding
makePrisms ''Params
makePrisms ''ParamSet
makePrisms ''Antiquoted
makePrisms ''NString
makePrisms ''NKeyName
makePrisms ''NUnaryOp
makePrisms ''NBinaryOp
