# solitaire

A CLI for [solitaire](https://en.wikipedia.org/wiki/Patience_(game)) (Klondike).

# Usage
After install the `solitaire` executable call it to 
- generate a random solitaire game
- display all possible next states

# Setup

- Install [ghcup](https://haskell.org/ghcup) which will furnish you with
  `stack` and a version of `ghc`. 
- Install appropriate versions of `ghc` and `stack` according to the 
  project files.
- Run `stack install` to copy the `solitaire` executable to your local path.

# Game state

``` txt 

-C||**|**|**|**|**|**|AC
-S||**|**|**|**|**|3H
-H||**|**|**|**|4D
-D||**|**|**|XH
  ||**|**|3D
**||**|5C
**||9C

```
- `**` Represents a card with hidden state a.k.a. a face down card.
- `-C` Represents an empty foundation for the club suit.

## Suits
- `C` club
- `S` spade
- `H` heart
- `D` Diamond

## Ranks
The numeric ranks are just the number except 10.
- `X` 10
- `A` Ace
- `J` Jack
- `Q` Queen 
- `K` King
