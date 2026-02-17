# zkFold's layer 2 offchain code

The repository houses following projects:

## [`rollup-offchain-cardano-api`](./rollup-offchain-cardano-api/)


[`rollup-offchain-cardano-api`](./rollup-offchain-cardano-api/) provides the off‑chain transaction building code for [zkFold](https://zkfold.io/)'s layer‑2 solution.

### Tests


```
cabal install --package-env=$(pwd) --overwrite-policy=always cardano-cli cardano-node
cabal run rollup-offchain-cardano-api-tests -- -j1
```

Sometimes, node instances are still running even after completion of tests, execute `killall cardano-node` after running tests to kill node instances.

## [`rollup-aggregator-server`](./rollup-aggregator-server/)

[`rollup-aggregator-server`](./rollup-aggregator-server/) provides the aggregator server for our layer‑2 solution.

Aggregation server serves the API as defined [here](./web/openapi/api.yaml). To start the server, you would need to provide configuration. Sample configuration is given below.

```yaml
coreProvider:
  maestroToken: YOUR_API_TOKEN
networkId: preprod
logging:
  - type: { tag: stderr }
    severity: Debug
    verbosity: V2
port: 8082
collateral:
  d5b1818211e09c2276a5b3c07deabb462e2fcba2a2f3abf572cebff2c85d76c3#1
  # Wallet that provides UTxO to be used as collateral.
wallet:
  tag: mnemonicWallet
  contents:
    mnemonic:
      - health
      - unable
      - dog
      - lend
      - artefact
      - arctic
      - dinner
      - energy
      - silent
      - wealth
      - shock
      - safe
      - glad
      - mail
      - gas
      - flag
      - beauty
      - penalty
      - mixed
      - garbage
      - erupt
      - wonder
      - magnet
      - around
    # Account index.
    accIx: 0
    # Payment address index.
    addrIx: 0
batchConfig:
  # Number of transactions to put in the batch.
  batchTransactions: 2
  # Periodic interval to process batch.
  batchIntervalSeconds: 300
apiKey: SERVER_API_KEY
# File path where to persist rollup-state. When starting from initial state, this file can be empty.
statePersistPath: rollup-state.json
maxBridgeIn: 1
maxBridgeOut: 1
maxOutputAssets: 2
rollupAddr: addr_test1wrygz6jr40426q8qtetj66f82euf3kjsnk5ykmure36yc3cxjteyz
rollupNFT: 298ab0e109c85d77152760e5dfcfb22aba60577aa95987cdce7c3288.7a6b466f6c642d726f6c6c75702d6e6674
rollupScriptRef: 48b23a0b01c080696a6fc97f0489a05fdc78125398fea1b21a226028a6caab19#0
rollupStakeScriptRef: 48b23a0b01c080696a6fc97f0489a05fdc78125398fea1b21a226028a6caab19#1
setupBytes:
  cmQc_bytes: 88b89ee7e7995473c79bef125333c481e19149f301038a1b384b45d955e1f2dcda52c66321d52fa6b9c46933792c05fd
  cmQk_bytes: 8f06fe0e6145eb2adf2dbe508ed78289fd9859639e207423dd7061b97239d8df74d044b8decd5def0d2c6a0613b1174d
  cmQl_bytes: a4e113eced3902705f14ebfbb11199c9cbb34e48488f805ca6e074bcd1494a33a4ca6f9f46104bdcde25bcf9b8faf8e2
  cmQm_bytes: ac6da07722f807c819312933afe51a9f89e11fffedf1f4eb624930770167c5d29df75b9a495ba6653385501ac52b1144
  cmQo_bytes: ac33d1a43aa2694786c4bcb3f865998db3de2fb0751c72814f61d1f0b14c307f72b7c5b411c8a5d68aca39f02055cdff
  cmQr_bytes: 874ba30546b19c76e551c01f75cd25e71860bf73b9881eec877986ae5d58fb04a1b3ae06be36eedcf7d2c7f5c7c0de96
  cmS1_bytes: ad4e8adf37f4b36313df95e850505f2c924de09a4ae51fc07e68fa9abeea87b0d82557bba3ea606f12686ea38f711c8e
  cmS2_bytes: b57ec0d139463b635a9aa613f44047b94666838c302bebefb44a063ce68a046cc3cf1428880f7d9c1f932d54c03d5afa
  cmS3_bytes: 82f12f52a6c320c0f79e3d18ca202f93699d1d46414d4eb1e1e3a43b334d0c1df6fae8e66c4a1375611cf1784a3c8dc8
  cmT1_bytes: 8c9df98353d60ac02a54ec3f4a91ea126861c259974321ef0dade17a9910fadb515b493e2f04d4769555a936b9d65130
  cmT2_bytes: 8c9df98353d60ac02a54ec3f4a91ea126861c259974321ef0dade17a9910fadb515b493e2f04d4769555a936b9d65130
  cmT3_bytes: 850e21e94f1eb5e19593b02b504cd66015f2685b0f036ff73e08a465d995d07b5df5d6773fa43a98657a6d80c2980ad9
  h1_bytes: 8063b799d2fd0db7bfa36f5ac7ab74fcfc1cfba97e8e0239cdcfba69df5ad32e0d18508eaa93c509875976f3fa7ae5ce1940e420eb29cc57b5983a8da117fbd6c59978cd4ae4134f8b881e3ca349bbd4a052fbe54b68d91ce975e294702edaed
  k1_int: 0
  k2_int: 42
  'n': 262144
  nPrv: 0
  omegaNPrv_int: 42951892408294048319804799042074961265671975460177021439280319919049700054024
  omega_int: 42951892408294048319804799042074961265671975460177021439280319919049700054024
  pow: 18
```

We provide a script to deploy rollup and this script would also generate relevant configuration parameters to be used in configuration file. Script can be executed like:

```
cabal run rollup-seed -- \
  --config secrets/maestro-config-preprod.json \
  --signing-key secrets/test-wallet.skey \
  --state-file secrets/rollup-state.json \
  --output secrets/out.yaml \
  --max-bridge-in 1 \
  --max-bridge-out 1 \
  --max-output-assets 2
```

Sample server execution command `cabal run rollup-aggregator-server:rollup-aggregator-server -- serve -c secrets/config-preprod.yaml`.

### Tests

We give an end-to-end test for verify behaviour of server endpoints, it is defined in [here](./rollup-aggregator-server/test/ZkFold/Cardano/Rollup/Aggregator/Test/EndToEnd.hs).

All the tests can be ran via: `cabal run rollup-aggregator-server-tests`. This test-suite involves spawning cardano testnets, so you may need to execute `killall cardano-node` after running tests.