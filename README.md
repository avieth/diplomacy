# Diplomacy

These programs aspire to provide everything you need in order to talk about
the board game Diplomacy in Haskell.

# State of the project

As of [this commit](https://github.com/avieth/diplomacy/commit/a19c72e61bd7aaf48071f825fd066596799260f2)
the typical phase resolver passes
87 of the [DATC](http://web.inter.nl.net/users/L.B.Kruijswijk/) test cases.
It probably passes more than that, but not every one of them has been transcribed.

It remains to implement an interface for playing a game of diplomacy, but with
order validation and resolution in place for all phases, this will be
straightforward.

# Components

## Datatypes

- `GreatPower` enumerates the seven competing powers.
- `Province` enumerates the territories of the diplomacy board.
- `ProvinceCoast` enumerates the special coastal areas like the north coast of
  Spain.
- `ProvinceTarget` is the sum of `Province` and `ProvinceCoast`; a unit occupies
  a `ProvinceTarget`, not a `Province`.
- `Zone` is a `ProvinceTarget` where coastal areas of the same `Province` are
  `Eq` equal.
- `Phase` enumerates the three phases of a game, and is used to classify other
  datatypes at the type level.
- `Subject` pairs a `Unit` with a `ProvinceTarget`, identifying an agent on a
  board.
- `OrderType` enumerates the types of orders, and is used to classify other
  datatypes at the type level.
- `OrderObject` describes the objective of some order; paired with a `Subject`
  and a `GreatPower`, is completes an `Order`.

## Typical phase resolver

By far the most complex component of this repository is the *typical phase
resolver*

```Haskell
typicalResolution
    :: M.Map Zone (Aligned Unit, SomeOrderObject Typical)
    -> M.Map Zone (Aligned Unit, SomeResolved OrderObject Typical)
```

which implements the nontrivial details of what's commonly referred to as an
*adjudicator*. This is where the four types of *typical phase orders*--hold,
move, support, and convoy--are analyzed as an ensemble to decide for what
reason, if any, they should fail.

# Thanks

Much thanks to Lucas B. Kruijswijk for giving us the
[DATC](http://web.inter.nl.net/users/L.B.Kruijswijk/), from which
[many tests](AdjudicationTests.hs) were transcribed and consequently many bugs
found and fixed.
