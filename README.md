# Agricola: All Creatures Big and Small


This is a Haskell implementation of `Agricola: All Creatures Big and Small`,
by `Uwe Rosenberg`. It uses a NCurses UI interface.

The instructions for the game itself can be found in the included repo file
`AgricolaAllCreaturesBigAndSmall.pdf`


It requires ncursesw to be installed on your system. On
`ubuntu`, it can be installed with

    sudo apt-get install libncursesw5-dev libncursesw5

To play the game itself, you can run

    $ make run

or

    $ cabal run

If you've already gotten the dependencies installed. If there are any problems,
`cabal` should  be able to point you in the right direction.

Which creates a `cabal` sandbox and installs into that the required dependencies.
This requires `cabal >= 1.22`.

To run the tests of which there admittedly very few of, you can run

    $ make test

or

    $ cabal test

