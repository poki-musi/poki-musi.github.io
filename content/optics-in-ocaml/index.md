---
title: "Existential Optics and OCaml"
date: 2022-09-24
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

No optics 101 are to be explained here. I expect you at least have a vague
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
after in order to update the original value. Unfortuantely, as currently presented,
that's not possible. Since `a` is only part of `s`, we need "the rest of `s`" to
rebuild it off `a`.

In order to fix this, we can think of this operation as decomposition. We take `s`,
and we decompose it into a pair of values of type `(a,c)`, where `c` is "the rest of
`s`". Thus, we have a mapping from `s`'s structure to `a,c`, and a reverse mapping back to `s`... that's just an [isomorphism](https://en.wikipedia.org/wiki/Isomorphism).

{{ figure(
  src="iso-lens.png",
  position="center",
  style="width: 70%",
  caption_position="center",
  caption_style="background-color: transparent;"
) }}

An isomorphism just states that two structures are only different in labelling,
that is, we can map to each of these structures in both directions as they have the
same structure anyway. Since our lens holds the mappings, it becomes *proof* of this
relation.

Every optic we'll make here has to hold this property, where the original type and
the "focused" part alongside some extra information, are equivalent up to an
isomorphism. The type of this last value could very well be anything so as long as such
property holds anyway, which is why these types of optics are called [existential](https://wiki.haskell.org/Existential_type).

~~Unfortunately OCaml doesn't really support existential types~~ Actually yes
they do, with the usage of first-class modules and GADTs. Big failure from my
part. Here's the signature for our lens type:

```ocaml
type ('s, 'a) lens = Lens : {
  get : 's -> 'c * 'a ;
  set : 'c * 'a -> 's ;
} -> ('s, 'a) lens
```

So yeah, our isomorphism requires a mapping from `s` to `a,c`, and another that
goes back. Hence, two functions. `set`, `over` and `view` are relatively simple
to build from here:

```ocaml
(* Just get the value and extract 'a from it. *)
let view (Lens {get; _}) s = s |> get |> snd

(* Get the value and update the 'a component off ('a,'c), then go back to 's. *)
let over (Lens {get; set}) s f = value |> get |> (fun (c, a) -> (c, f a)) |> set

(* Just reuse over, where the modifying function just ignores its argument
   and returns the value we want to update 's with. *)
let set lens s a = over lens s (Fun.const a)
```

Now, `Prism`s. In case we can extract `a` from some value of type `s`, we can trivially
map `s` to `a` and viceversa. However, in case the operation fails, we need to
conserve information (of type `'c`) that allows us to rebuild it later. In the
case of a sumtype, it could be the very value it came from, so that when it is
reverse mapped, we can return it directly.

So all we really have to do is use an `Either` type. It'll hold `'a` when the
focus is successful, `'c` otherwise, and with any of those values were can
reconstruct how `'s` was like.

```ocaml
type ('s, 'a) prism = Prism : {
  get : 's -> ('c, 'a) Either.t ;
  set : ('c, 'a) Either.t -> 's ;
} -> ('s, 'a) prism
```

Here's `preview` and `review` in terms of this representation:

```ocaml
(* Get the value, and map it to an Option type. *)
let preview (Prism {get; _}) v = get v |> function Left _ -> None | Right v -> Some v

(* Reviewing just means to evaluate the "successful"
   branch in the set function. *)
let review (Prism {set; _}) v = Right v |> set
```

`Iso` is very trivial in this representation. It is, quite literally, an isomorphism.
The types `s` and `a` are already expected to be the same structurally. Thus, no
extra information to conserve, thus `'c`'s type here would be `unit`, or what
would be the same, such type would not even be included in our signature:

```ocaml
type ('s, 'a) iso = Iso : { get : 's -> 'a ; set : 'a -> 's } -> ('s, 'a) iso
```

Also, the operations `from` and `to_` are trivial to implement:

```ocaml
let from (Iso {get; _}) = get
let to_ (Iso {set; _}) = set
```

Last but not least, affines (or better called `AffineTraversal` or `Optional`).
An affine type its has a structure like `c + b × a`. so, not only are we trying to
focus on a particular case of a sumtype, but *also* on a piece of a particular case.
That's kinda why you can call them a combination of prisms and lenses, as you're
literally combining the means of focusing on both a case AND part of the type.

Again, type representation breaks a bit in OCaml, because now we need to somehow
split a type variable (the `a` from `b x a`) into two parts... and that's just not
possible. So instead we pass three type parts into the constructor - a `'b` type
that indicates the parts we need to "track of" in order to rebuild the original
type, and a type `'c` that indicates all the possible failed branches:

```ocaml
type ('s, 'a, 'b, 'c) affine = {
  get : 's -> ('c, 'b * 'a) Either.t ;
  set : ('c, 'b * 'a) Either.t -> 's;
}
```

And the affine traversal's `preview` and `over` functions would implemented as
follows.

```ocaml
let preview {get; _} v = get v |> Either.map ~left:(Fun.const None) ~right:snd
let over {get; set} f v =
  get v |> Either.map_right (fun (b, a) -> (b, f a)) |> set v
```

Again, it's kind of a combination of what we did with lenses and prisms. Of
course, just if you squint a bit.

> Do notice that these type signatures do not necessarily require the types of
> these optics to have `s` and `a` to be the same in the "forward" and "backward"
> mapping:
>
> ```ocaml
> type ('s, 't, 'a, 'b) lens' = Lens : {
>   get : 's -> ('c, 'a) ;
>   set : ('c, 'b) -> 't ;
> } -> ('s, 't, 'a, 'b) lens
>
> type ('s, 'a) lens = ('s, 's, 'a, 'a) lens'
> ```
>
> This is perfectly fine (it does change some type signatures, notably the
> function passed to `over` changes from `a -> a` to `a -> b`). It also
> makes sense under our mental model of isomorphic optics - we are still
> conserving the same shape, we're just transforming the focused part into another
> type. Currently we're working with the restricted version, as it is simply
> easier.

# Composition Nightmare

Of course, one of the beautiful things about optics is its high composability.
That means we don't need to make a very specific optic for every occasion, but
rather we make one out of smaller parts. Neat.

What's so neat is how this has to be implemented in OCaml. Again, no
typeclasses, no ad-hoc polymorphism. And in order to do composition in these types of
optics, you *still* need typeclasses to infer the morphsisms required to fit the optics
together. Therefore, we... quite literally need to make a function for every pair of
optic types, manually (which **must** be ordered because the order of composition
matters, adding more cases to handle).

They vary from being trivial:

```ocaml
let iso_and_lens
  (Iso {get=get1;set=set1})
  (Lens {get=get2;set=set2})
  =
  let get v = get1 v |> get2 in
  let set v = set2 v |> set1 in
  Lens { get; set }
```

To:

```ocaml
let compose
  (Affine {get=get1; set=set1})
  (Affine {get=get2; set=set2})
  =
  let open Either in
  let get s1 = match get1 s1 with
    | Right (b1, s2) ->
        begin match get2 s2 with
        | Left c2 -> left (right (b1, c2))
        | Right (b2, a2) -> right ((b1, b2), a2)
        end
    | Left c1 ->
        c1 |> left |> left
  in
  let set = function
    | Right ((b1, b2), a2) ->
        let s2 = right (b2, a2) |> set2
        in (b1, s2) |> right |> set1
    | Left (Right (b1, c2)) ->
        let s2 = left c2 |> set2 in
        (b1, s2) |> right |> set1
    | Left (Left c1) ->
        c1 |> left |> set1
  in Affine {get; set}
```

Yeah, the functions themselves are all about matching the types of the two composed
optics and the resulting composition. The implementation is boring, so instead
let us talk about how composition relates to our mental model of isomorphisms.

If our intent is to focus on the focus of another type, that's just making an optic
out of the relevant subjacent focuses/optics `('a, 'b) t` and `('b, 'c) t`.
At an user level, there isn't much to think about.

However, how do the residual types work here? It depends. For `Iso`s in relation to
any other optic `('s, 'a) t`, all we're doing is either mapping `'s` and `'a` to
another type (depending on if we compose from the left or the right, respectively), so
the residual type is the one contained in the other optic.

In the case of `Prism`s, we add a new possibility - or "branch" - to the
potential isomorphic result we get out of `'s`. For example, if we were to inspect
the type of the composition between two `Prism`s `p1 = ('s1, 'a1)` and
`p2 = ('a1, 'a2)`, we would get:

```ocaml
{ get : 's1 -> (('c1, 'c2) Either.t, 'a2) Either.t ;
  set : (('c1, 'c2) Either.t, 'a2) Either.t -> 's1 ; }
```

So, the resulting prism has a residual type `('c1, 'c2) Either.t`, which indicates
the existence of *two* failure cases:

* We could fail at getting `'a1`, so we would get `'c1` out of the first `Prism`.
* We could successfuly get `'a1`, but fail at getting `'a2`, thus we get `'c2`.

In the case of `Lens`es of type `('s, 'a) t`, we need to keep the inner, residual part
`'c`. If we compose the lens to the left of another optic `o`, we would have to keep
`'c` in every potential decomposition `o` has. Meanwhile, if the `Lens` is
decomposed to the right to another optic, we just separate the type inside the
"successful" decomposition case according to the lens.

Let's try to observe how the composition of a prism `('s1, 'a1) t` and
lens `('a1, 'a2) t` works (considering their residual types `'c1` and
`'c2` respectively):

```ocaml
{ get : 's1 -> ('c1, 'c2 * 'a2) Either.t ;
  set : ('c1, 'c2 * 'a2) Either.t -> 's2 ; }
```

Curious. It's the exact same signature as a generic affine. We get to add
a branch, in which the successful branch needs to be split. So the composition
of affines really can be reasoned as if we were composing a prism, then a lens. Neat.

Now, all of this doesn't really matter at an user level. Though they might care about the
explosion of combinations of pairings. Since we don't have ad-hoc polymorphism,
we can't just use one single operator for optic composition. As an alternative,
a page was taken off reasonML and scala libraries, where they just do method chaining
that quite literally describe the composition of which types are combined.
We can simulate something similar, and also make it relatively easy to type.

Essentially, every composition function that takes two optics of `a` and `b`, and
composes them in that order, has a signature of `a -> b -> c`. For each of those
functions, an alias of two letters of type `b -> a -> c` is made.
The inversion of the input is intentional - they're swapped in order to
facilitate composition through piping. Here's a sample:

```ocaml
let some_optic = some_lens |> la some_affine |> ap some_prism |> ai some_iso
```

And so the letter to the left of the shorthand indicates the optic taken from
the left, and the letter to the right, the optic we apply from the right.
Yeah. Simple enough.

# The Twist Villain

Now, you might think we've escaped what probably was the worst part of this
whole project. Well, we kinda did actually, but not without yet another curve
ball!

Maybe you want generic lenses. Say, lenses over the first component of a tuple
of two elements:

```ocaml
let _1 = Lens.make
  ~get:(fun (a, b) -> (b, a))
  ~set:(fun (b, a) -> (a, b))
```

This doesn't really work because of OCaml's [value restriction semantics](http://ocamlverse.net/content/weak_type_variables.html). In a nutshell and in this case,
partial application of polymorphic functions that only solve partially (that is, not
all the type vars get resolved in compile time) become closures with weak types.
Those get resolved by the compiler once the value gets used anywhere, and uses
the types that match that situation. Thus, this resulting type is not
polymorphic in any way, which isn't what we want. We *want* to make our lens
generic in the first place.

How do we solve this? Well, just force the lens to instantiate whenever you
wanna use a new one:

```ocaml
let _1 () = Lens.make
  ~get:(fun (a, b) -> (b, a))
  ~set:(fun (b, a) -> (a, b))
```

This is incredibly dumb but... I suppose, it's a fine solution. Here's the lens at
play:

```ocaml
let _1_1_1 () = _1 () |> ll (_1 ()) |> ll (_1 ())
val _1_1_1 : unit -> ((('a * 'b) * 'c) * 'd, 'a) lens'
```

So yeah. Interesting experience. You may check out the (as of 24/09/22) half-documented project in my
github, [here](https://github.com/poki-musi/optics). I'll eventually add it to
`opam`.

I leave you all with a rather daunting image of the type signature
of my previous optics implementation, which unfortunately exposed the internal
residual types to the user. [I love types :)](https://www.youtube.com/watch?v=fJ0UQDWiS8o)

{{ figure(
  src="i-love-types.png",
  alt="Henlo",
  style="width: 60%",
  caption="That's not the entire type, by the way.",
  caption_style="background-color: transparent;"
) }}
