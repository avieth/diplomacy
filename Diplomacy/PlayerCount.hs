module Diplomacy.PlayerCount (

    PlayerCount(..)

  ) where

-- | TODO the rules give alternate ways to play, but we shall implement those
--   later. For now, it's only seven-player.
--   The impact of this datum on the game: special considerations for
--   retreat/disbandment; in five or six player, we have countries which should
--   never take orders, and which should always disband their armies in the
--   retreat phase.
--   In smaller games, some players will control multiple countries, so this
--   will affect the self-attack rules for order resolutions, although this
--   part of the rules is apparently not specified.
data PlayerCount = Seven
  deriving (Eq, Ord, Show)
