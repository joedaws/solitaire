module Game.Card (
    HasCard (..),
    HasFace (..),
    Card (..),
    GameCard (..),
    Color,
    Face (..),
    Rank (..),
    Suit (..),
    color,
    flipGameCard,
    mkCard,
    mkGameCardDown,
    mkGameCardUp,
) where

class HasCard a where
    toCard :: a -> Card

class HasFace a where
    toFace :: a -> Face
    flipCard :: a -> a

data Suit
    = Hearts
    | Diamonds
    | Clubs
    | Spades
    deriving (Enum, Bounded, Eq)

instance Show Suit where
    show s = case s of
        Hearts -> "H"
        Diamonds -> "D"
        Clubs -> "C"
        Spades -> "S"

data Rank
    = Ace
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    deriving (Enum, Bounded, Eq, Ord)

instance Show Rank where
    show r = case r of
        Ace -> "A"
        Two -> "2"
        Three -> "3"
        Four -> "4"
        Five -> "5"
        Six -> "6"
        Seven -> "7"
        Eight -> "8"
        Nine -> "9"
        Ten -> "X"
        Jack -> "J"
        Queen -> "Q"
        King -> "K"

-- Define a card as a combination of a rank and a suit
data Card = Card
    { rank :: Rank
    , suit :: Suit
    }
    deriving (Eq)

instance Show Card where
    show (Card r s) = show r ++ show s

instance HasCard Card where
    toCard = id

data Face = Up | Down deriving (Show, Eq)

data GameCard = GameCard
    { card :: Card
    , face :: Face
    }
    deriving (Eq)

instance Show GameCard where
    show (GameCard _ Down) = "**"
    show (GameCard c Up) = show c

instance HasCard GameCard where
    toCard = card

instance HasFace GameCard where
    toFace = face
    flipCard = flipGameCard

data Color = Red | Black deriving (Show, Eq)

color :: Card -> Color
color (Card _ Hearts) = Red
color (Card _ Diamonds) = Red
color (Card _ Clubs) = Black
color (Card _ Spades) = Black

mkCard :: Rank -> Suit -> Card
mkCard = Card

mkGameCardDown :: Rank -> Suit -> GameCard
mkGameCardDown r s = GameCard (Card r s) Down

mkGameCardUp :: Rank -> Suit -> GameCard
mkGameCardUp r s = GameCard (Card r s) Up

flipGameCard :: GameCard -> GameCard
flipGameCard (GameCard c Up) = GameCard c Down
flipGameCard (GameCard c Down) = GameCard c Up
