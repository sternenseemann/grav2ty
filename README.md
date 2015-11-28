# grav1ty

grav1ty will probably be a two dimensional space game! It will be awesome!

grav1ty is written in Haskell using [frpnow][frpnow-paper] and [gloss][gloss].
Physics computations are mainly powered by Edward Kmett's [linear][linear].

Currently it only does gravity computations in a FRP-y way but it will
definately evolve!

[ki][naudiz] must also be mentioned for deriving the physical formulæ for me!

## Things left to do

* collisions
* manual acceleration / spaceship controls
* Nice models for object
* random universe generations
* moving viewport?

# Building

Like most haskell projects grav1ty can be build using cabal, I also provided an
`shell.nix` for dependency management.

[frpnow-paper]: http://www.cse.chalmers.se/~atze/papers/prprfrp.pdf
[gloss]: http://hackage.haskell.org/package/gloss
[linear]: https://hackage.haskell.org/package/linear
[naudiz]: https://github.com/KiNaudiz
