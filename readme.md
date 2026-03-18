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
dbPath: rollup.db
maxBridgeIn: 1
maxBridgeOut: 1
maxOutputAssets: 2
rollupAddr: addr_test1wr4fsa8ka8nng76jshd5vwywtew54cty8qpqtzxkzhu3ncghc7ff9
rollupNFT: 27aa77d4661c5b17ad9d530aa42667f90c3407701d56469d8bf02c2b.7a6b466f6c642d726f6c6c75702d6e6674
rollupScriptRef: 6fd51e05a9c70edcb97b1be1101829a87312f5c2e5c043e0fb28b5f78676ea2e#0
rollupStakeScriptRef: 6fd51e05a9c70edcb97b1be1101829a87312f5c2e5c043e0fb28b5f78676ea2e#1
setupBytes:
  cmQc_bytes: a7ef89ad7fb7ab75fa408e7b9b7536067d518a65b02b5e397f97ae0306711931ad2198839918c63a6cf59fd9a1232ca7
  cmQk_bytes: 99180c758405a3f4ab294fb3f33dbb9b505d92eca17685a8838846f2cc41345df473279e8bd6d80a91706691761ade6b
  cmQl_bytes: b9e87077078535d9a628f52ef58dca6102224ab2d4e6ccc839c14783c7710a4d0c1fd8f74bb1c9a052570f3be1f39fd4
  cmQm_bytes: 8e72bc248b3a5c5bf0620e36832411e67c14ed8aef9878468d993e1e00462ec6a42a783c8328da56e99589c80d3df1eb
  cmQo_bytes: a08e0ff38a5d63b445afd95fdb206f2a2e084449fdc2548b96cb0612228c88f38f1eac8c87707962468646a1fd6a5bd4
  cmQr_bytes: b1970e5932669be886fe60c1b111a1b0c5d404a38d76d816e878ef090c0e244ac2e1f9d2d5dc920409ee9aa624e0d9b7
  cmS1_bytes: ae0c8803a0d2a0c1e4d01840375f2d1fb57d1fedf0c628e86309efdd3c46c1d0b8310c25cf0d41d4bc847aa51ed9835b
  cmS2_bytes: 8e2953c4449b5cfcada09d10b162ee9ad764b44678fd2feef970cab193750ebe9baaef2b778325ed028fd0274fb8bda6
  cmS3_bytes: 920f409b717652a2c5b3bc6def0d8cd4afaffbf6385b9448a466557df2ac2e39f5dde9fe9dfd064de448850a1f72eecb
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