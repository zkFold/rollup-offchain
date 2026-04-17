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
  2d87ad1997d546591f57235b62801c8e8ee8f487ab96892c6fc929df30101995#0
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
dbPath: rollup.db
# Access to socket path of running cardano node for chain sync event listener to track the accumulating onchain L2 state.
nodeSocketPath: path/to/node.socket
chainSyncStartPoint:
  slot: 120542468
  blockHash: 5c55360e637428f54e7fa891cd1fb29b094599090a0f7ffb09492cc033ab39b0
maxBridgeIn: 1
maxBridgeOut: 1
maxOutputAssets: 2
rollupAddr: addr_test1wzg2m2ju8eepm75fp73kc4lf2gme0l5ncz7fp0pwhg8058ccuyn43
rollupNFT: b069e397fe35dce167c2a94192803bc1324e591e2fdc5756a24d4ae1.7a6b466f6c642d726f6c6c75702d6e6674
rollupScriptRef: 818a6f3c0fdcbc5a8ec774c4f520dc55ec2dfd97063f3374c19bf776d59ea846#0
rollupStakeScriptRef: 818a6f3c0fdcbc5a8ec774c4f520dc55ec2dfd97063f3374c19bf776d59ea846#1
setupBytes:
  cmQc_bytes: 90e60fd0b07cd85dbabcea7946d8e0ce39539564c486a996014d3f0ed12249921afc41873881146b5117841c1d097168
  cmQk_bytes: 889cbdfb39698fe973050aaef12ff1234f46f8aa78bcc38a46f04281ab378cbd89bbf575a1cf89acbe7bc504e7341e7b
  cmQl_bytes: adbc2953a1f3afccea700eb8d9ce6b1ac555c57d106a604e943420de137eedb5104158f437635f492049017192b9062a
  cmQm_bytes: b8980b491b5411314dc30c2701e81af1756ac20c9ad21eb4649583e6909586a6b6eea2edcac1ee654abc936a339994ce
  cmQo_bytes: 8ffaaa35fcd33dd5dcef04ae74f53c8760c2ec902acaa9f25c3ef4747dc605404f07d51bd80785aaa22cfeb034e8fc2a
  cmQr_bytes: a68adc13122629f27343c7db3465bfc7ee5805aca7ee11fc2349910f3e0f058b859dcd96a296134121cf6441a35b8a46
  cmS1_bytes: b47bf42fc3dc51774bdda475d260d17204e5f6d2979016cc316896d3be8846e81070aaa275a679465dac134fccea57f9
  cmS2_bytes: 985e63ae6e0ccd45513fe40a30b991ba79e7f7cb3cd3fc704b95b2c3512fd1896830966c7c9ededb66d37d42f27422d0
  cmS3_bytes: a353c3cd207b0a81916c06109214108d492928295eb16c804f687b4aaeb766c90218ef0991d0572c6acc5c44187b82a4
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
  --state-file rollup.db \
  --output secrets/out.yaml \
  --max-bridge-in 1 \
  --max-bridge-out 1 \
  --max-output-assets 2
```

Sample server execution command `cabal run rollup-aggregator-server:rollup-aggregator-server -- serve -c secrets/config-preprod.yaml`.

Batcher is also clubbed with same executable and uses the same configuration, to run batcher, use command `cabal run rollup-aggregator-server:rollup-aggregator-server -- batch -c secrets/config-preprod.yaml`.

### Tests

We give an end-to-end test for verify behaviour of server endpoints, it is defined in [here](./rollup-aggregator-server/test/ZkFold/Cardano/Rollup/Aggregator/Test/EndToEnd.hs).

All the tests can be ran via: `cabal run rollup-aggregator-server-tests`. This test-suite involves spawning cardano testnets, so you may need to execute `killall cardano-node` after running tests.