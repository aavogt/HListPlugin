# HListPlugin

The idea is to use a type checker plugin (ghc>7.10) to allow ghc be smarter
when dealing with HList.

## Replacing Constraints with other Constraints

### SameLength/HLength (DONE)
The goal is that in the following session (ghci ex/HList.hs):

    *HList> :set +t -XDataKinds -XFlexibleContexts -XTypeFamilies -XConstraintKinds
    *HList> let a = Proxy :: SameLength x '[(),()] => Proxy x
    a :: Proxy '[y, y1]
    *HList> let b = Proxy :: (HLength y ~ HLength '[(),()]) => Proxy y
    b :: HLength y ~ 'HSucc ('HSucc 'HZero) => Proxy y

the plugin would be able to make both types equal to `Proxy '[y, y1]`.
ex/SameLength.hs `b` is that example in a file.

### Why? (TODO)
More generally, the idea is to allow ghc to reason about equality constraints between
type families. For example we might encode that `F` is injective in the second parameter:

```haskell
type family F a b
type family F_getB a r

type instance F "A" "B" = "R"
type instance F_getB "A" "R" = "B"

-- If ghc replaces (F a b ~ r) with (F_Eq_r a b r),
-- then it becomes possible to infer (b ~ "B") from
-- (F "A" b ~ "R")
type F_Eq_r a b r = (F_getB a r ~ b, F a b ~ r)

-- More generally, ghc might take (F a b ~ F a' b'),
-- and replace it with:
type F_Eq_F a b a' b' = (F_Eq_r a b (F a' b'), F_Eq_r a' b' (F a b))
```

While the `F_Eq_F` doesn't do any more than `F_Eq_r`, the `SameLength` example is
of that form. This type of replacement might be useful when reasoning about
lookups/updates of a record. Currently we use class that encodes the 5 ways
you can relate the variables involved when you have a lens into a Record:

```haskell
class SameLength s t => Labelable (x :: Symbol) s t a b
          | x s b -> t,
            x t b -> a,
            x s -> a,
            x t -> b where
  hLens :: Proxy x -> Lens (Record s) (Record t) a b
```

But ghc expands out the "implementation details" when you call hLens and some
of the type variables are known (see
https://gist.github.com/aavogt/474f7facbae076bdf993). Supposing that we make HList use TFs for lookups/updates, the implementation only needs constraints to calculate results from arguments:

```haskell
type Labelable x s t a b = (HUpdateAtR x b s ~ t, (s ! x) ~ a)
```

But type information should be able to flow backwards as expressed by the functional dependencies attached to `Labelable` above.
The use of `HUpdateAtR` above should allow ghc to use `(HUpdateAtR x a t ~ s, (t ! x) ~ b)` to figure out unknown type variables.  In other words I want to teach ghc to reason: if I got `t` by putting a `b` at label `x` in `s`, I can put an `a` at label `x` in `t` to get `s` back, and I can get a `b` by looking at label `x` in `t`.

## Coercible
`data family HList xs` should have role representational, but ghc doesn't look at instances to see that "I always use the same number of `HCons` for the same length of `xs`".

Mostly working: see `ex/Coerce.hs`. Note that a ghc that has #10031 fixed is needed.
