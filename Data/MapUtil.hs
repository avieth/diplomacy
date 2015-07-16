{-|
Module      : Data.MapUtil
Description : Definition of lookupWithKey
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Data.MapUtil (

    lookupWithKey

  ) where

import qualified Data.Map as M
import qualified Data.Set as S

-- | Lookup a key in a map and get back the actual key as well. Useful when
--   the key Eq instance is not quite so sharp.
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
        k' = head (S.elems (keys `S.intersection` S.singleton k))
    in fmap (\x -> (k', x)) v
