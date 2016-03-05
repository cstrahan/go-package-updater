{-# LANGUAGE TemplateHaskell #-}

module Nix.Expr.Lens.Internal where

import           Control.Lens.TH
import           Data.Fix
import           Nix.Expr

makePrisms ''Fix
makePrisms ''NExprF
