{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
module Grav2ty.Util.Serialization where

import Flat
import GHC.Generics
import Grav2ty.Core (Hitbox (..), Object (..), Modifier (..), Modification (..))
import Linear.V2 (V2 (..))

deriving instance Generic (Modification a)
deriving instance Generic (Hitbox a)
deriving instance Generic (Object a)
deriving instance Generic Modifier

deriving instance Flat a => Flat (V2 a)
deriving instance Flat a => Flat (Object a)
deriving instance Flat a => Flat (Hitbox a)
deriving instance Flat a => Flat (Modification a)
deriving instance Flat Modifier
