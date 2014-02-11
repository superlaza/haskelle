import Control.Arrow
import Control.Category

newtype SF a b = SF {runSF :: [a] -> [b]}

--first, we need to make SF an instnace of Category, apparently
instance Category SF where
	(SF g) . (SF f) = SF ( (Prelude..) g f); id = SF Prelude.id}

{--
I have no idea how Arrow defines the rest of the functions for this instance. The >>> used here applies to regular functions as Arrows
--}
instance Arrow SF where
	arr f = SF (map f)
	first (SF f) = SF (unzip >>> first f >>> uncurry zip)

stream ~(x:xs) = x:stream xs

instance ArrowLoop SF where
	loop (SF f) = SF $ \as ->
		(bs,cs) = unzip (f (zip as (stream cs)))
			in bs

delay x = SF (x:)
mul :: Arrow arr => arr (Integer, Integer) Integer
mul arr (uncurry (*))

facSF = loop (mul >>> (arr Prelude.id &&& delay 1))

fac x = runSF facSF [1..x] !! fromInteger (x-1)

loop :: (->) (b,d) (c,d)

{--
the reason the instantiation of (->) as an ArrowLoop works is because the contructor for the function data type looks like
f x = ... x something ...
and in loop f b, b could be the variable of type (b,d)
so when you're invoking the function contructor, you make a funciton that takes type b, which is required to be a tuple by the loop type declaration, and returns a type (b,c) which is what it forces the output c to be. So it turns out that the output type isn't special as the type signature for loop might suggest. In our case the input and ouput type of loop are the same (forcing the ouput to be a tuple, since b c can't comform to (b,d) (c,d)) The real usefulness of loop is that


Actually, you wanna look at it as (loop f) b = let (c,d)= f (b,d) in c, where we take a function from (b,d) to (c,d) and then make a new function (loop f) which takes a b and returns a c.


Actually^2, the implementation of loop for functions does not give insight into how loop works. You basically get the desired functionality from your definition of the function f, which must switch arguments to model the passing of output as input.

case in point: f (b,d) = 
--}

class Arrow a => ArrowLoop a where
        loop :: a (b,d) (c,d) -> a b c

instance ArrowLoop (->) where
        loop f b = let (c,d) = f (b,d) in c