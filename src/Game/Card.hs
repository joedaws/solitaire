module Game.Card (
    HasCard (..),
    HasFace (..),
    IsPlayable (..),
    Card (..),
    KlondikeCard (..),
    Color,
    Face (..),
    Rank (..),
    Suit (..),
    color,
    flipKlondikeCard,
    mkCard,
    mkKlondikeCardDown,
    mkKlondikeCardUp,
) where

class HasCard a where
    toCard :: a -> Card

class HasFace a where
    toFace :: a -> Face
    flipCard :: a -> a

class IsPlayable a where
    isPlayable :: a -> Bool

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

data KlondikeCard = KlondikeCard
    { card :: Card
    , face :: Face
    }
    deriving (Eq)

instance Show KlondikeCard where
    show (KlondikeCard _ Down) = "**"
    show (KlondikeCard c Up) = show c

instance HasCard KlondikeCard where
    toCard = card

instance HasFace KlondikeCard where
    toFace = face
    flipCard = flipKlondikeCard

instance IsPlayable KlondikeCard where
    isPlayable c = face c == Up

data Color = Red | Black deriving (Show, Eq)

color :: Card -> Color
color (Card _ Hearts) = Red
color (Card _ Diamonds) = Red
color (Card _ Clubs) = Black
color (Card _ Spades) = Black

mkCard :: Rank -> Suit -> Card
mkCard = Card

mkKlondikeCardDown :: Rank -> Suit -> KlondikeCard
mkKlondikeCardDown r s = KlondikeCard (Card r s) Down

mkKlondikeCardUp :: Rank -> Suit -> KlondikeCard
mkKlondikeCardUp r s = KlondikeCard (Card r s) Up

flipKlondikeCard :: KlondikeCard -> KlondikeCard
flipKlondikeCard (KlondikeCard c Up) = KlondikeCard c Down
flipKlondikeCard (KlondikeCard c Down) = KlondikeCard c Up
