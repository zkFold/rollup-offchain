module ZkFold.Cardano.Rollup.Utils (
  setupToPlutus,
) where

import Data.Coerce (coerce)
import PlutusTx.Builtins qualified as PlutusTx
import ZkFold.Cardano.OnChain.BLS12_381.F (F (..))
import ZkFold.Cardano.OnChain.Plonkup.Data (SetupBytes (..))
import ZkFold.Symbolic.Examples.SmartWallet (ZKSetupBytes (..)) -- TODO: Update this import to not import from smartwallet module.

setupToPlutus ∷ ZKSetupBytes → SetupBytes
setupToPlutus ZKSetupBytes {..} =
  SetupBytes
    { n = n
    , nPrv = nPrv
    , pow = pow
    , omega = coerce omega_int
    , omegaNPrv = coerce omegaNPrv_int
    , k1 = coerce k1_int
    , k2 = coerce k2_int
    , h1_bytes = PlutusTx.toBuiltin h1_bytes
    , cmQm_bytes = PlutusTx.toBuiltin cmQm_bytes
    , cmQl_bytes = PlutusTx.toBuiltin cmQl_bytes
    , cmQr_bytes = PlutusTx.toBuiltin cmQr_bytes
    , cmQo_bytes = PlutusTx.toBuiltin cmQo_bytes
    , cmQc_bytes = PlutusTx.toBuiltin cmQc_bytes
    , cmQk_bytes = PlutusTx.toBuiltin cmQk_bytes
    , cmS1_bytes = PlutusTx.toBuiltin cmS1_bytes
    , cmS2_bytes = PlutusTx.toBuiltin cmS2_bytes
    , cmS3_bytes = PlutusTx.toBuiltin cmS3_bytes
    , cmT1_bytes = PlutusTx.toBuiltin cmT1_bytes
    , cmT2_bytes = PlutusTx.toBuiltin cmT2_bytes
    , cmT3_bytes = PlutusTx.toBuiltin cmT3_bytes
    }