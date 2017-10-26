module Control.Lens.Yocto where
import Data.Functor.Const
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f -> (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter afb s = fmap setter (afb (getter s))

get :: Lens s t a b -> s -> a
get lens = getIdentity . lens Identity

set :: Lens s t a b -> b -> s -> t
set lens b = getConst . lens (Const b)

over :: Lens s t a b -> (a -> b) -> s -> t
over lens f = getConst . lens (Const . f)
