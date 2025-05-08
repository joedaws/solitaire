# solitaire-hint

A CLI for solitaire hints written in Haskell.

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
- `**` Represnets a card with hidden state
- `-C` Represents an empty foundation for the club suit.

## Suits
- `C` club
- `S` spade
- `H` heart
- `D` Diamond

## Ranks
The numberic ranks are just the number except 10.
- `X` 10
- `A` Ace
- `J` Jack
- `Q` Queen 
- `K` King
