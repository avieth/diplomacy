module Diplomacy.Defecits (

    Defecits

  ) where

import qualified Data.Map as M
import           Diplomacy.Country

type Defecits = M.Map Country Int
