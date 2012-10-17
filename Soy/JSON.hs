{-# OPTIONS_GHC -fwarn-unused-imports -fwarn-incomplete-patterns #-}
module Soy.JSON where

import qualified Soy.Types as S
import qualified Data.Aeson as J
import qualified Data.Attoparsec.Number as A
import qualified Data.Vector as V
import qualified Data.Traversable as Trav
import Control.Monad

instance J.ToJSON S.Value where
  toJSON S.ValNull = J.Null
  toJSON (S.ValInt i) = J.toJSON $ i
  toJSON (S.ValFloat d) = J.toJSON $ d
  toJSON (S.ValString s) = J.toJSON $ s
  toJSON (S.ValBool b) = J.toJSON $ b
  toJSON (S.ValList lst) = J.toJSON $ lst
  toJSON (S.ValMap hm) = J.toJSON $ hm

instance J.FromJSON S.Value where
  parseJSON J.Null = return S.ValNull
  parseJSON (J.Number (A.I i)) = return $ S.ValInt i
  parseJSON (J.Number (A.D d)) = return $ S.ValFloat d
  parseJSON (J.String s) = return $ S.ValString s
  parseJSON (J.Bool b) = return $ S.ValBool b
  parseJSON (J.Array ary) = liftM S.ValList $ mapM J.parseJSON $ V.toList ary
  parseJSON (J.Object obj) = liftM S.ValMap $ Trav.traverse J.parseJSON obj
