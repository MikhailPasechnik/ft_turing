Install Haskell: https://www.haskell.org/platform/#linux-ubuntu

Build and run: `cabal new-build && ./dist-newstyle/build/x86_64-linux/ghc-8.6.5/ft-turing-0.1.0.0/x/ft-turing/build/ft-turing/ft-turing`

Machines:

- 05 Pseudo universal
        
    `./ft-turing ./machines/05_pseudo_universal.json "C&C{[+S>.][.H>.][1C>1]}S{[1P<+][.H<.]}P{[.C>1]}*11111+111111111111111111"`

Visialisation:

`d` MOve one step forward
`a` Move one step back

`./ft-turing ./machines/unary_add.json 111+11 2>&1 1>/dev/null | python3 vis.py`

`./ft-turing ./machines/05_pseudo_universal.json "C&C{[+S>.][.H>.][1C>1]}S{[1P<+][.H<.]}P{[.C>1]}*11111+111" 2>&1 1>/dev/null | python3 vis.py`
