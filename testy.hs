
data Shape = Circle Float Float Float | Rectangle Float Float Float Float

data Sam a = Rami a | Amira a deriving (Show)
instance Functor Sam where
    fmap f (Rami a) = Rami (f a)
    fmap f (Amira a) = Amira (f a)

newtype Samir a = Sam a deriving (Show, Eq)
instance Functor Samir where
    fmap f (Sam a) = Sam (f a)

newtype SamirRami a = SR (String -> a)
instance Functor SamirRami where
    fmap f (SR g) = SR (\inp -> f (g inp))
instance (Show a) => Show (SamirRami a) where
    show (SR g) = "SR"

-- Test
multi :: Parser Int -> String -> [(Int, String)]
multi (Parser p) inp = case p inp of
                [] -> []
                [(v, out)] -> [(v*42, out)]

