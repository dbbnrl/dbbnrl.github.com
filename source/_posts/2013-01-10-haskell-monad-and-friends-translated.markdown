---
layout: post
title: "Haskell Monad &amp; friends, translated"
date: 2013-01-10 21:00
comments: true
categories: 
published: false
---

**Functor**

 - As defined by Haskell:
```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
 - Laws:
```haskell
fmap id = id
fmap (g . h) = (fmap g) . (fmap h)
```

**Applicative**

 - As defined by Haskell:
```haskell
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```
 - Laws:
```haskell
pure id <*> v = v
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
g <$> x = pure g <*> x
    where <$> = fmap
```

***Monoidal*** is described as equivalent to Applicative.

 - Definition:
```haskell
class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)
```
 - Laws (note ≅ denotes isomorphism, not equality):
```haskell
fmap (f *** g) (u ** v) = fmap f u ** fmap g v
unit ** v ≅ v
u ** unit ≅ u
u ** (v ** w) ≅ (u ** v) ** w
```

**Monad**

 - As defined by Haskell:
```haskell
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    m >> n = m >>= \_ -> n
 
    fail   :: String -> m a
```
 - Equivalently we could say (but don't):
```haskell
class Applicative m => Monad' m where
    (>>=) :: m a -> (a -> m b) -> m b
```
 - Or:
```haskell
class Applicative m => Monad'' m where
    join :: m (m a) -> m a
```
 - Laws:
```haskell
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
 
fmap f xs  =  xs >>= return . f  =  liftM f xs
```
 - Alternatively:
```haskell
return >=> g  =  g
g >=> return  =  g
(g >=> h) >=> k  =  g >=> (h >=> k)
    where
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

**MonadTrans**

 - As defined by Haskell:
```haskell
class MonadTrans t where
    lift :: Monad m => m a -> t m a
```
 - Laws:
```haskell
lift . return   =  return
lift (m >>= f)  =  lift m >>= (lift . f)
```

**MonadFix**

 - Adds another function:
```haskell
mfix :: (a -> m a) -> m a
```

**Semigroup**

 - As defined:
```haskell
class Semigroup a where
    (<>) :: a -> a -> a
 
    sconcat :: NonEmpty a -> a
    sconcat = sconcat (a :| as) = go a as where
        go b (c:cs) = b <> go c cs
        go b []     = b
 
    times1p :: Whole n => n -> a -> a
    times1p = ...
```
 - Laws:
```haskell
(x <> y) <> z = x <> (y <> z)
```

**Monoid**

 - As defined:
```haskell
class Monoid a where
    mempty  :: a
    mappend :: a -> a -> a
 
    mconcat :: [a] -> a
    mconcat = foldr mappend mempty
```
 - Laws:
```haskell
mempty `mappend` x = x
x `mappend` mempty = x
(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)
```

**Alternative**

 - As defined:
```haskell
class Applicative f => Alternative f where
    empty :: f a
    (<|>) :: f a -> f a -> f a
```
 - Follow Monoid laws
 
**MonadZero**

 - No longer defined, but it's just Monad with:
```haskell
mzero :: m a
```

**MonadPlus**

 - As defined:
```haskell
class Monad m => MonadPlus m where
    mzero :: m a
    mplus :: m a -> m a -> m a
```
 - Laws:  Monoid laws and
```haskell
mzero >>= f  =  mzero
v >> mzero   =  mzero
```

**Foldable**

 - As defined:
```haskell
class Foldable t where
    fold    :: Monoid m => t m -> m
    foldMap :: Monoid m => (a -> m) -> t a -> m
 
    foldr   :: (a -> b -> b) -> b -> t a -> b
    foldl   :: (a -> b -> a) -> a -> t b -> a
    foldr1  :: (a -> a -> a) -> t a -> a
    foldl1  :: (a -> a -> a) -> t a -> a
```

**Traversable**

 - As defined:
```haskell
class (Functor t, Foldable t) => Traversable t where
    traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
    sequenceA :: Applicative f => t (f a) -> f (t a)
    mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
    sequence  ::       Monad m => t (m a) -> m (t a)
```
 - Laws:
```haskell
traverse Identity = Identity
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
```

**Category**

 - As defined:
```haskell
class Category arr where
  id  :: a `arr` a
  (.) :: (b `arr` c) -> (a `arr` b) -> (a `arr` c)
```
 - Laws:
```haskell
id . f = f = f.id
f . (g . h) = (f . g) . h
```

**Kleisli**
```haskell
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
 
instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    Kleisli g . Kleisli h = Kleisli (h >=> g)
```

**Arrow**

 - As defined:
```haskell
class Category arr => Arrow arr where
    arr :: (b -> c) -> (b `arr` c)
    first :: (b `arr` c) -> ((b, d) `arr` (c, d))
    second :: (b `arr` c) -> ((d, b) `arr` (d, c))
    (***) :: (b `arr` c) -> (b' `arr` c') -> ((b, b') `arr` (c, c'))
    (&&&) :: (b `arr` c) -> (b `arr` c') -> (b `arr` (c, c'))
    <<< = .
    >>> = flip .
```
 - Laws:
```haskell
arr id  =  id
                  arr (h . g)  =  arr g >>> arr h
                first (arr g)  =  arr (g *** id)
              first (g >>> h)  =  first g >>> first h
   first g >>> arr (id *** h)  =  arr (id *** h) >>> first g
          first g >>> arr fst  =  arr fst >>> g
first (first g) >>> arr assoc  =  arr assoc >>> first g
 
assoc ((x,y),z) = (x,(y,z))
```

**ArrowZero**
```haskell
class Arrow arr => ArrowZero arr where
    zeroArrow :: b `arr` c
```

**ArrowPlus**
```haskell
class ArrowZero arr => ArrowPlus arr where
    (<+>) :: (b `arr` c) -> (b `arr` c) -> (b `arr` c)
```    

**ArrowChoice**
 - As defined:
```haskell
class Arrow arr => ArrowChoice arr where
    left  :: (b `arr` c) -> (Either b d `arr` Either c d)
    right :: (b `arr` c) -> (Either d b `arr` Either d c)
    (+++) :: (b `arr` c) -> (b' `arr` c') -> (Either b b' `arr` Either c c')
    (|||) :: (b `arr` d) -> (c `arr` d) -> (Either b c `arr` d)
```

**ArrowApply**
 - As defined:
```haskell
class Arrow arr => ArrowApply arr where
    app :: (b `arr` c, b) `arr` c
```

**ArrowLoop**
```haskell
class Arrow a => ArrowLoop a where
    loop :: a (b, d) (c, d) -> a b c
 
trace :: ((b,d) -> (c,d)) -> b -> c
trace f b = let (c,d) = f (b,d) in c
```

**Comonad**
```haskell
class Functor w => Comonad w where
    extract :: f a -> a
 
    duplicate :: w a -> w (w a)
    duplicate = extend id
 
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate
```

 - Pointed
 - Catamorphism
