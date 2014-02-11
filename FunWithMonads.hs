import Data.List
import Data.Monoid

data MyList a = Empty | a `Cons` (MyList a)

-- type t in MyList t must be Showable
-- show is polymorphic, so it is automatically distinguished
instance (Show t) => Show (MyList t) 
	where show = prettify . show'
		where show' Empty = []
		      show' (a `Cons` as) = (show a)++(show' as)
		      prettify = ('[':).(++"]").(intersperse ',')

--let's go ahead and make our list an instance of the Monoid typeclass
plus :: (MyList a) -> (MyList a) -> (MyList a)
plus as Empty = as
plus Empty bs = bs
plus (a `Cons` as) bs = a `Cons` (plus as bs)

instance Monoid (MyList a) where
	mempty = Empty
	mappend = plus

--to implement bind for lists, we need concat and map
myconcat :: (MyList (MyList a)) -> (MyList a)
myconcat Empty = Empty
myconcat (aa `Cons` aas) = plus aa (myconcat aas)

mymap :: (a -> b) -> (MyList a) -> (MyList b)
mymap f Empty = Empty
mymap f (a `Cons` as) = (f a) `Cons` (mymap f as)


instance Monad MyList where
	as >>= f = myconcat $ mymap f as
	return a = a `Cons` Empty

--lastly, lets convert from standard lists and vice versa
fromMyList :: (MyList a) -> [a]
fromMyList Empty = []
fromMyList (a `Cons` as) = a:(fromMyList as)

toMyList :: [a] -> (MyList a)
toMyList [] = Empty
toMyList (a:as) = a `Cons` (toMyList as)


{--
try it out...
negative numbers come out ugly because they're treated as separate characters and
I didn't want to deal with it
--}

f x = (x*2) `Cons` ( (x+2) `Cons` Empty)

result1 = return 3 >>= f
result2 = (3 `Cons` (4 `Cons` Empty)) >>= f
result3 = (toMyList [1..20]) >>= f

