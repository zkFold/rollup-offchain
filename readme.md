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
  maxTransactions: 2
  batchIntervalSeconds: 300
apiKey: SERVER_API_KEY
maxBridgeIn: 1
maxBridgeOut: 1
maxOutputAssets: 1
rollupAddr: addr_test1wphfguts64udjtpjx0k3y6jx6fany3cee4y795825th53cg8ztepg
rollupNFT: fb5020cb6beffcf410e911dc3fa2bfab45a2c5264864c8b9cd86b412.7a6b466f6c642d726f6c6c75702d6e6674
rollupScriptRef: 1d6822d2b6f55ae45378aeeea99f77b171f25244370cdc6a46a3bab62a0a2705#0
rollupStakeScriptRef: 1d6822d2b6f55ae45378aeeea99f77b171f25244370cdc6a46a3bab62a0a2705#1
setupBytes:
  cmQc_bytes: a4f81f5338a97c5c31781558a755907b3f8cbd4661566bc48d4cf906cbd86dcebf45e9fa99a56c6b914a1cef3b8e4d9b
  cmQk_bytes: b2ed45bf6d2838427ec2c3f4c32d764a28aea84023249593a796e9958df7b74e0a49bbc567233ab86a3a8a89999c13bc
  cmQl_bytes: a6901e96b8bf4d50042438fe97062137c4777384c916ad01bcb816e91c804e85510ddbbf2a6149cb277f3c66ae3a8bce
  cmQm_bytes: ac694bbdb5a010de4aac1ffae09782d12362d192a5290e173bcae7850620027581d524cb1d9c2612548a25646a68e9c6
  cmQo_bytes: ae563f1d7dc9d647435a7663e5b64f4d29ccfeec36134f6529fef409186d42ade21008419e6dce708bff53fb3091a00a
  cmQr_bytes: 8e7083e03326a78fc7412415739e8235592decc96f46b275b0ceab5b208b102887db8f121054b133e7196f4c37813eb0
  cmS1_bytes: 81eb48903377b28644d7afae8f47fe4eb3ec61aeba6d58a25bf80c6749f959191c1ab2acae48b5728f52b78a0988d9c0
  cmS2_bytes: a9c49c28c52d63441bdd5e6e3796f40936b6f8f97960c046acb5ebe3b0e70fc3ad03426a0971f6cabafafd40dd7474f8
  cmS3_bytes: 872b9972b570bae8cfcce3ff12641b983881d210056d0b53616e6d58ca9033e3c619224ecde4c822b4750b58f64f78b0
  cmT1_bytes: 8c9df98353d60ac02a54ec3f4a91ea126861c259974321ef0dade17a9910fadb515b493e2f04d4769555a936b9d65130
  cmT2_bytes: 8c9df98353d60ac02a54ec3f4a91ea126861c259974321ef0dade17a9910fadb515b493e2f04d4769555a936b9d65130
  cmT3_bytes: 850e21e94f1eb5e19593b02b504cd66015f2685b0f036ff73e08a465d995d07b5df5d6773fa43a98657a6d80c2980ad9
  h1_bytes: 8063b799d2fd0db7bfa36f5ac7ab74fcfc1cfba97e8e0239cdcfba69df5ad32e0d18508eaa93c509875976f3fa7ae5ce1940e420eb29cc57b5983a8da117fbd6c59978cd4ae4134f8b881e3ca349bbd4a052fbe54b68d91ce975e294702edaed
  k1_int: 0
  k2_int: 42
  "n": 262144
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
  --output secrets/out.yaml \
  --max-bridge-in 1 \
  --max-bridge-out 1 \
  --max-output-assets 2
```

### Tests

We give an end-to-end test for verify behaviour of server endpoints, it is defined in [here](./rollup-aggregator-server/test/ZkFold/Cardano/Rollup/Aggregator/Test/EndToEnd.hs).

All the tests can be ran via: `cabal run rollup-aggregator-server-tests`. This test-suite involves spawning cardano testnets, so you may need to execute `killall cardano-node` after running tests.