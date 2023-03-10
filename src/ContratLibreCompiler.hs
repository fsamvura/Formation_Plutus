{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ContratLibreCompiler where

import              Prelude (FilePath, IO)
import              Cardano.Api
import              Cardano.Api.Shelley             ( PlutusScript (..), PlutusScriptV2 )
import              Codec.Serialise                 (serialise)
import              Data.Aeson
import qualified    Data.ByteString.Lazy            as LBS
import qualified    Data.ByteString.Short           as SBS
import qualified    Plutus.V1.Ledger.Scripts
import qualified    Plutus.V1.Ledger.Value
import qualified    Plutus.V2.Ledger.Api
import qualified    Plutus.V2.Ledger.Contexts
import qualified    PlutusTx
import              PlutusTx.Prelude


import qualified ContratLibreValidator

-- If we do not import Ledger, then
-- how to replace Ledger.Validator?

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeContratLibreScript :: IO (Either (FileError ()) ())
writeContratLibreScript = writeValidator "output/plutus-red-egal-dat.plutus" ContratLibreValidator.validator
