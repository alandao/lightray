# LightRay
A raytracer written in Haskell. Intended to be a rendering engine aka
something similar to "Ogre3D".

# Output
![Output](http://i.imgur.com/22ejzef.png)

#Features
Planes and spheres

Perspective Camera

# Issues
None so far

# Work in Progress
Lighting (right now)

Reflection

Refraction

Optimizations

# Installation
Get stack at https://github.com/commercialhaskell/stack. Run "stack build" in
the Lightray project folder where "Lightray.cabal" is located. Then run
"stack exec -- Lightray". Currently, a file called "test.bmp" outputs in the
project folder. "test.bmp" should look like the output above.
