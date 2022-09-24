---
title: "Existential Optics and OCaml"
date: 2022-09-12
taxonomies:
  tags: [ocaml, fp]
---

First post.

I wanted basic optics in OCaml. Let's try to implement those.

<!-- more -->

I was surprised that nobody seemed to have uploaded any optics packages
in `opam`. Sure, there are at least 3 or 4 lens libraries, but they don't include
prisms. Some even have problems with their polymorphic lenses! As such,
I've decided that it would be a very interesting experience to investigate how
optics are implemented, and potentially upload the resulting library to `opam`
for everyone else.

I won't be doing optics 101 here. I expect you at least have a vague
sense of the more basic types in the family (`Iso`, `Prisms`, `Lenses` and
`Affines`/`AffineTraverals` or whatever they're called). If you want an
introduction to them, read [this](https://www.tweag.io/blog/2022-05-05-existential-optics/) or [this](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/a-little-lens-starter-tutorial).

Now, in order to implement optics in OCaml, I've decided to rely on an
alternative representation to the usual two main and popular implementations -
profunctor optics and the van Laarhoven representation. Existential optics offer
a simple model, that does not rely too heavily on typeclasses compared to
the other representations (except in ad-hoc polymorphic composition, which we
can't do in OCaml anyways). It is also useful in order to understand how the other
representations are implemented.

# Isomorphic Optics

Let us imagine a lens that takes a type `s` and extracts a part of type `a` out
of it. Lenses need to be capable of doing that extraction, but also reversing it
after in the case of a modification. Unfortuantely, as currently presented,
that's not possible. Since `a` is only part of `s`, we have some missing
information in order to rebuild it.

In order to fix this, we can think of this operation as decomposition. We're
taking `s`, and we decompose it into a pair of values of type `(a,c)`, where `c`
is "the rest of `s`", which we need in order put it together with `a` and obtain `s`
back. So, we've found a way to preserve all information about `s`'s structure
when going to `a,c`, and we can go back to `s` from there... that is an [isomorphism](https://en.wikipedia.org/wiki/Isomorphism). This quite literally preserves the structure to `s`.

{{ figure(
  src="iso-lens.png",
  position="center",
  style="width: 70%",
  caption_position="center",
  caption='Isomorphism between `s` and `a,c`',
  caption_style="background-color: transparent;"
) }}

This is a property that's for all the optics we'll build here. They are all proofs of
a conservation of structure or information between `s` and `a`, by preserving enough
information about the origin in order to rebuild it.
This type could very well be anything so as long as this isomorphic relation is
conserved, which is why these types of optics are called [existential](https://wiki.haskell.org/Existential_type).
Unfortunately OCaml doesn't really support existential types in the same way Haskell
does, so we need to make this `c` type explicit in our type signatures (this'll be a
pattern by the way, sorry).

So then, let's make a signature of our existential lens builder:

```ocaml
type ('s, 'a, 'c) lens = {
  get : 's -> 'c * 'a ;
  set : 'c * 'a -> 's ;
}
```

> Alternatively, this could be done with modules, and that's what I've tried at
> first, but at the same time it doesn't change much.

So yeah, our isomorphism requires a mapping from `s` to `a,c`, and another that
goes back. Hence, two functions. `set`, `over` and `view` are relatively simple
to build from here:

```ocaml
let view {get; _} s = s |> get |> snd
let over {get; set} s f = value |> get |> (fun (c, a) -> (c, f a)) |> set
let set lens s a = over lens s (Fun.const a)
```

Now, `Prism`s. In case we can extract `a` from some value of type `s`, we can trivially
map `s` to `a` and viceversa. However, in case the operation fails, we need to
conserve information (of type `'c`) that allows us to rebuild it later. In the
case of a sumtype, it could be the very value it came from, so that when it is
reverse mapped, we can return it directly.

```ocaml
type ('s, 'a, 'c) prism = {
  get : 's -> ('c, 'a) Either.t ;
  set : ('c, 'a) Either.t -> 's ;
}
```

In case we get to extract `'a`, the mapping is trivial. Otherwise, we need to
conserve some information of type `'c` in order to rebuild it later. That's it.
Here's `preview` and `review` in terms of this representation:

```ocaml
let preview {get; _} v = get v |> function Left _ -> None | Right v -> Some v
let review {set; _} v = Right v |> set
```

`preview`ing is just `get` that maps the result to an `Option`. `review`ing
matches a value of type `a` so that it gets reverse mapped successfully back to
`s`. Simple stuff.

Even more simple is `Iso`! Since an iso is quite literally an isomorphism, the
types `s` and `a` are expected to have a bijective relation between them. So,
there's no extra information to conserve and thus `c` would be of type `unit`,
thus conserving the previous model.

```ocaml
type ('s, 'a, 'c) iso = { get : 's -> 'a ; set : 'a -> 's }
```

In my implementation, I actually just eliminated the `'c` variable from the
signature, though. Also, the operations `from` and `to_` are trivial to
implement:

```ocaml
let from {get; _} = get
let to_ {set; _} = set
```

Last but not least, affines (or better called `AffineTraversal` or `Optional`).
An affine type has a structure like `c + b x a`. so, not only are we trying to
focus on a particular case of a sumtype, but *also* on part of a particular case.
One can acquire a trivial notion as to why the composition of lenses and prisms result
into an affine, because... that's literally what it would result into - a
combination of focusing into parts and cases.

Again, type representation breaks a bit in OCaml, because now we need to somehow
split a type variable into two parts... and that's just not possible. So instead
we pass three type parts into the constructor:

```ocaml
type ('s, 'a, 'b, 'c) affine = {
  get : 's -> ('c, 'b * 'a) Either.t ;
  set : ('c, 'b * 'a) Either.t -> 's;
}
```

> Do notice that these type signatures do not necessarily require the types of
> these optics to have `s` and `a` to be the same in the "forward" and "backward"
> mapping:
>
> ```ocaml
> type ('s, 't, 'a, 'b, 'c) lens' = {
>   get : 's -> ('c, 'a) ;
>   set : ('c, 'b) -> 't ;
> }
>
> type ('s, 'a, 'c) lens = ('s, 's, 'a, 'a, 'c) lens'
> ```
>
> This is perfectly fine (it does change some type signatures, notably the
> function passed to `over` changes from `a -> a` to `a -> b`). It also
> makes sense under our mental model of isomorphic optics - we are still
> conserving the same shape, we're just transforming the focused part into another
> type.

# Composition Nightmare

Of course, one of the beautiful things about optics is its high composability.
That means we don't need to make a very specific optic for every occasion, but
rather we make one out of smaller parts. Neat.

What's so neat is how this has to be implemented in OCaml. Again, no
typeclasses, no ad-hoc polymorphism. And in order to do composition in these types of
optics, you *still* need typeclasses to infer the morphsisms required to fit the optics
together. Therefore, we... quite literally need to make a function for every pair of
optic types, manually (which **must** be ordered because the order of composition
matters).

They vary from being trivial:

```ocaml
let iso_and_lens
  ({get=get1;set=set1} : ('a, 'b) iso)
  ({get=get2;set=set2} : ('b, 'c, 'd) lens)
  : ('a, 'c, 'd) lens
  =
  let get v = get1 v |> get2 in
  let set v = set2 v |> set1 in
  { get; set }
```

To:

```ocaml
let compose
  ({get; set} : ('s1, 's2, 'b1, 'c1) t)
  ({get=get'; set=set'} : ('s2, 'a2, 'b2, 'c2) t)
  : ('s1, 'a2, 'b1 * 'b2, ('c1, 'b1 * 'c2) Either.t) t
  =
  let open Either in
  let get s1 = match get s1 with
    | Right (b1, s2) ->
        begin match get' s2 with
        | Left c2 -> left (right (b1, c2))
        | Right (b2, a2) -> right ((b1, b2), a2)
        end
    | Left c1 ->
        c1 |> left |> left
  in
  let set = function
    | Right ((b1, b2), a2) ->
        let s2 = right (b2, a2) |> set'
        in (b1, s2) |> right |> set
    | Left (Right (b1, c2)) ->
        let s2 = left c2 |> set' in
        (b1, s2) |> right |> set
    | Left (Left c1) ->
        c1 |> left |> set
  in
  {get; set}
```

The whole process behind designing these was incredibly boring and repetitive.
Let's rather talk about the user API.

Again, no ad-hoc polymorphism, so we can't just use one single operator for optic
composition. As an alternative, a page was taken off reasonML and scala libraries,
where they just do method chaining that quite literally describe the composition
of which types are combined. We can simulate something similar, and also make it
relatively easy to type.

Essentially, every composition function that takes two optics of `a` and `b`, and
composes them in that order, has a signature of `a -> b -> c`. For each of those
functions, an alias of two letters of type `b -> a -> c` is made.
The inversion of the input is intentional - they're swapped in order to
facilitate composition through piping. Here's a sample:

```ocaml
let some_optic = some_lens |> la some_affine |> ap some_prism |> ai some_iso
```

And so the letter to the left of the shorthand indicates what must be piped, while the
letter to the right restricts the type of the optic to pipe into.

# The Twist Villain

Now, you might think we've escaped what probably was the worst part of this
whole project. No.

Maybe you want generic lenses. Say, lenses over the first component of a tuple
of two elements:

```ocaml
let _1 = Lens.make
  ~get:(fun (a, b) -> (b, a))
  ~set:(fun (b, a) -> (a, b))
```

This doesn't really work because of OCaml's value restriction semantics. Or at
least, it won't work when composed. It'll just shit on you for even trying to
use the same exact value for two different lens situations. So how would this
get fixed?

```ocaml
let _1 () = Lens.make
  ~get:(fun (a, b) -> (b, a))
  ~set:(fun (b, a) -> (a, b))
```

... just force the generic lens to be a function itself. Yep. Just instanciate
the lens whenever you need it.

```ocaml
let _1_1_1 () = _1 () |> ll (_1 ()) |> ll (_1 ())
```

So yeah. You may check out the (as of 24/09/22) half-documented project in my
github, [here](https://github.com/poki-musi/optics). Now, I shall leave this post with some ridiculous type combination after
playing around with generic optics:

<a href="https://www.youtube.com/watch?v=fJ0UQDWiS8o">
{{ figure(
  src="i-love-types.png",
  alt="Henlo",
  style="width: 70%",
  caption='*tiktok voice* I love optics!',
  caption_style="background-color: transparent;"
) }}
</a>
