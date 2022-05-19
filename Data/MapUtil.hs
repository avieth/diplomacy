{-|
Module      : Data.MapUtil
Description : Definition of lookupWithKey
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

module Data.MapUtil (

    lookupWithKey

  ) where

import qualified Data.Map as M
import qualified Data.Set as S

-- | Lookup a key in a map and get back the actual key as well. Useful when
--   the key Eq instance is not quite so sharp, i.e. when k ~ Zone and we
--   have a special zone which matches a normal zone, we want to get back the
--   zone that was in the map, not the zone that was used to lookup.
lookupWithKey
    :: Ord k
    => k
    -> M.Map k v
    -> Maybe (k, v)
lookupWithKey k m =
    let v = M.lookup k m
        keys = M.keysSet m
        -- keys `S.intersection` S.singleton k is empty iff v is Nothing, so
        -- this won't be undefined.
        --
        -- NB: the point is that intersection biases to the left, so that
        -- k' is not necessarily the same as k.
        k' = head (S.elems (keys `S.intersection` S.singleton k))
    in fmap (\x -> (k', x)) v
