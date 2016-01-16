# Preliminaries

- OSX: Install [Gnuplot][1] using [Homebrew][2]

        brew install gnuplot --with-wxmac


# Setup

Install deps using `cabal` or `stack`:

    cabal install --only-dependencies

or

    stack build


# Try it

`cabal repl` or `stack ghci` then

    > plot [Fun (sin.(pi*)) "sin", Fun (cos.(pi*)) "cos"]

Default plot interval is [-1,1].

![example plot][3]

[1]: http://www.gnuplot.info
[2]: http://brew.sh
[3]: example.png
