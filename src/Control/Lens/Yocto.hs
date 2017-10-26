{-# LANGUAGE RankNTypes #-}
module Control.Lens.Yocto where
import Data.Functor.Const
import Data.Functor.Identity

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter afb s = fmap (setter s) (afb (getter s))

get :: Lens s t a b -> s -> a
get l = getConst . l Const

set :: Lens s t a b -> b -> s -> t
set l b = runIdentity . l (const (Identity b))

over :: Lens s t a b -> (a -> b) -> s -> t
over l f = runIdentity . l (Identity . f)
