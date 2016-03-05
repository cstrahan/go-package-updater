{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Data.List

import           Control.Lens                 hiding ((.=))
import           Control.Monad.Writer
import           Data.Aeson
import qualified Data.ByteString.Lazy         as BS
import           Data.Data
import           Data.Data.Lens
import           Data.Fix
import           Data.Foldable
import           Data.Function
import           Data.Functor
import qualified Data.Generics.Uniplate.Data  as U
import           Data.Hashable
import qualified Data.HashMap.Strict          as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified GHC.Generics                 as G
import           Nix.Expr
import           Nix.Expr.Lens
import           Nix.Parser                   hiding (Failure, Success)
import qualified Nix.Parser                   as P
import           Nix.Pretty
import           System.IO
import           Text.PrettyPrint.ANSI.Leijen hiding ((<$>), (<>))
import           Text.Show.Pretty

-- jq . < json.txt --sort-keys


-- TODO:These should go away after the next release of data-fix
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)
deriving instance G.Generic (Fix f)

data GitHubPackage = GHP
  { owner         :: T.Text
  , repo          :: T.Text
  , rev           :: T.Text
  , sha256        :: T.Text
  , goPackagePath :: Maybe T.Text
  } deriving (Eq, Ord, Show, G.Generic)

instance ToJSON GitHubPackage

secondIf :: (a -> Bool) -> Traversal' (a,b) b
secondIf p f (x,y) = (\y0 -> (x,y0)) <$> (if p x then f else pure) y

safeFiltered :: (i -> Bool) -> Traversal' a (i, b) -> Traversal' a b
safeFiltered p t = t.(secondIf p)

main = do
    ast <- parseNixFile "go-packages.nix"
    case ast of
      P.Failure e -> hPutStrLn stderr $ "Parse failed: " ++ show e
      P.Success ast -> do
          let (nix, ghPackages) = runWriter $ mapMOf (_var "_self"._NamedVar._2._NWith._2._NSet.traverse) editPackage ast
          --prettyPrintNix nix
          let json = encode (formatGitHubPackages ghPackages)
          BS.putStr json

prettyPrintNix ast = displayIO stdout $ renderPretty 0.4 80 (prettyNix ast)

formatGitHubPackages =
    fromListByGroup owner (HM.fromList . map (\pkg -> (repo pkg, object ["rev" .= rev pkg,
                                                                         "sha256" .= sha256 pkg])))

fromListByGroup :: (Eq b, Hashable b) => (a -> b) -> ([a] -> c) -> [a] -> HM.HashMap b c
fromListByGroup toKey toVal xs = 
    HM.fromList [ (key, val)
        | grouped@(first:_) <- groupBy ((==) `on` toKey) xs
        , let key = toKey first
        , let val = toVal grouped
        ]

-- this probably violates many lens laws.
contemplate :: (Applicative f, Data a, Data b) => LensLike' f a b
contemplate = biplate . go
  where
    go f s = f s *> uniplate (go f) s

_var :: forall s a. (Data s) => Text -> Traversal' s (Binding NExpr)
_var name = contemplate.filtered ((== name) . varName)

_val :: Traversal' (Binding NExpr) Text
_val = _NamedVar._2._NStr._DoubleQuoted.traverse._Plain

varName :: Binding NExpr -> Text
varName (NamedVar [StaticKey n] _) = n
varName _ = ""

editPackage :: Binding NExpr -> Writer [GitHubPackage] (Binding NExpr)
editPackage binding
    | Just ghp <- getGitHubRepo binding
    = do tell [ghp]
         let naughty = [ "date", "version", "rev", "sha256", "src", "goPackagePath"]
                    ++ maybe [] (const ["goPackagePath"]) (goPackagePath ghp)
         return $ binding & _fun .~ "buildFromGitHub"
                          & _bindings %~ (filter (not . flip elem naughty . varName))
                          & maybe id (\path -> _bindings %~ setGoPackagePath path) (goPackagePath ghp)

    | otherwise
    = return binding
  where
    _fun = _NamedVar._2._NApp._1
    _bindings = _NamedVar._2._NApp._2.(failing _NSet _NRecSet)
    setGoPackagePath path bindings = bindings ++ ["goPackagePath" $= mkStr path]

getGitHubRepo :: Binding NExpr -> Maybe GitHubPackage
getGitHubRepo binding
    | Just rev <- binding ^? _var "rev"._val
    , Just owner <- binding ^? _var "owner"._val
    , Just repo <- binding ^? _var "repo"._val
    , Just sha256 <- binding ^? _var "sha256"._val
    , any (`elem` references binding) [ "fetchFromGitHub", "fetchgit"
                                      , "buildFromGitHub" ]
    = let goPackagePath = binding ^? _var "goPackagePath"._val
       in if goPackagePath == Just ("github.com/"<>owner<>"/"<>repo)
          then Just (GHP owner repo rev sha256 Nothing)
          else Just (GHP owner repo rev sha256 goPackagePath)
    | otherwise
    = Nothing

references :: Binding NExpr -> [Text]
references binding =
    [ s | Fix (NSym s) <- U.universeBi binding ]
