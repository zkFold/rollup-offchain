# Cardano offchain code for zkFold's layer 2 solution


This repository provides the off‑chain Cardano code for [zkFold](https://zkfold.io/)'s layer‑2 solution.

## Tests


```
cabal install --package-env=$(pwd) --overwrite-policy=always cardano-cli cardano-node
cabal run rollup-offchain-cardano-api-tests -- -j1
```

Sometimes, node instances are still running even after completion of tests, execute `killall cardano-node` after running tests to kill node instances.