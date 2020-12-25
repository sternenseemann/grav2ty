# grav2ty :rocket:

![screenshot of grav2ty showing a spaceship, a planet, an asteroid orbiting and two projectiles](./doc/grav2ty-2019-05-24.png)

the most realistic asteroids-like game in existence.

## controls

* `up`/`down`: increase/decrease thruster power (ship acceleration)
* `left`/`right`: rotate ship (counter)-clockwise
* `c`: toggle centered view
* `+`/`-`: zoom in/out
* `space`: fire a projectile

## roadmap

- [x] simulation of gravity
- [x] controllable spaceship
- [x] collision detection
- [ ] make measurements more realistic
- [x] allow for zooming the viewport
- [ ] Free viewport (mouse moved)
- [x] time scaling / fast forward
- [x] rework HUD, log additional info to console
  - [ ] Add speed to HUD
- [x] projectiles
  - [ ] Limit firerate
  - [x] make projectiles self-destruct
  - [ ] reduce redundancy in projectile (cannon + object)
- [ ] performance improvements
  - [ ] simplify RelGraph (ordered tuples as keys)
  - [ ] don't calculate gravity to every little object
  - [ ] don't do collision detection at a safe distance
- [ ] multi player support
- [ ] Prevent library user from creating a broken state (by hiding lenses etc. if possible)
- [ ] cosmetics (improved models, stars, …)
- [ ] switch rendering engine
- [ ] orbit visualization / prediction

## history

* screenshot of [grav2ty at its first commit](./doc/grav2ty-first-commit.png)
* the previous attempt [grav1ty](https://github.com/sternenseemann/grav2ty/tree/grav1ty)
