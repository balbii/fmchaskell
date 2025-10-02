module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True  = "tt"
    show False = "ff"

instance Enum Bool where

    fromEnum :: Bool -> Int
    fromEnum True = 1
    fromEnum False = 0

    toEnum :: Int -> Bool
    toEnum 1 = True
    toEnum 0 = False

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
False && _     = False
_     && False = False
True  && True  = True

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
True  || _     = True
_     || True  = True
False || False = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
True /|\ True = False
_    /|\ _    = True 

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
False \|/ False = True
_     \|/ _     = False

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
True  <=/=> False = True
False <=/=> True  = True
_     <=/=> _     = False

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not = undefined

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse = undefined

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
(==>) = undefined

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
(<==) = undefined

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) = undefined

infixr 1 <=>


