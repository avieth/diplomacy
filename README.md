# Diplomacy

These programs aspire to provide everything you need in order to talk about
the board game [Diplomacy](https://en.wikipedia.org/wiki/Diplomacy_%28game%29)
in Haskell.

## State of the project

Things look good. The order resolution component passes over 100 of the
[DATC](http://web.inter.nl.net/users/L.B.Kruijswijk/) test cases.
It probably passes more than that, but not every one of them has been
transcribed.

## Components

This project is organized into four parts:

- The types and data for the fundamental language of the game.
- The characterizations of valid orders.
- The resolution of orders.
- The description of the state of a particular game.

### Characterization of valid orders

An order is defined to be any subject/object pair. For instance, the subject of
`A Ion S A Bre - Par` is `A Ion` (an army in the Ionian Sea) and the object is
`S A Bre - Par` (support the army in Brest as it moves into Paris). Not every
such order makes sense: that support order is invalid, not only because an
army cannot be in the Ionian Sea, but also because no unit in the Ionian Sea
can support a move into Paris.

As far as I can tell, the characterization of valid orders is too intricate for
Haskell's type system, even with state of the art GHC-only extensions, to handle
well. Perhaps a language with full dependent types such as Idris is up to the
task, but in this project, we do order validation at the value level. However,
instead of giving indicator functions `Order phase orderType -> Bool` for
validity, we give more intricate descriptions of *why* an order is valid, in
the form of an intersection of unions of sets (corresponding to a conjunctive
normal form clause). By actually constructing the valid orders and their
components, we obtain not only a way to check validity (`analyze`) but also a
way to generate all valid orders (`synthesize`), which could be very useful
when implementing a user-facing client.

An order of the typical or retreat phase is either valid or invalid, regardless
of the other orders issued. The mantra for these phases is that a valid order
would succeed if no other orders were issued. The situation is different for
the adjust phase, in which no order is valid on its own. Instead, the whole set
of orders for a given great power is either valid or invalid. This is due to
the deficit constraint: if a great power has more units than supply centres,
it must disband *exactly* the difference; if it has more supply centres than
units, it *may* build at most the magnitude of the difference. In this phase,
a valid *set* of orders would succeed regardless of the orders of the other
great powers (and in fact it *will* succeed, because adjust phase orders from
different great powers never conflict).

### Resolution of orders

In order to carry a game from one round to the next (for instance, to go
from a typical phase to a retreat phase), orders must be checked against one
another to determine which orders succeed, and which orders fail. This process
is known as *order resolution*, and it is defined distinctly for each phase.

While the adjust phase is clearly the most simple to resolve (every valid order
succeeds), the typical phase resolution is far more complex than that of the
retreat phase. This typical phase resolver is the component which determines
which supports are cut, which convoys fail, which moves standoff or are
overpowered. It must also deal with the ambiguities in the rulebook, which
the DATC is very helpful in pointing out and characterizing via tests.

## Thanks

Much thanks to Lucas B. Kruijswijk for giving us the
[DATC](http://web.inter.nl.net/users/L.B.Kruijswijk/), from which
[many tests](AdjudicationTests.hs) were transcribed and consequently many bugs
discovered and fixed.
