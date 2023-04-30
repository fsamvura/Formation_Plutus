{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ImportQualifiedPost #-}

module AlwaysSucceedValidator where

import Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Plutus.V2.Ledger.Api qualified as Plutus
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Prelude qualified as PlutusPrelude


{-# INLINABLE mkValidator #-}
alwaysSucceeds :: BuiltinData -> BuiltinData -> BuiltinData -> ()
alwaysSucceeds _ _ _ = ()

  

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript
   $$(PlutusTx.compile [|| wrap ||])
 where
   wrap = Scripts.mkUntypedValidator alwaysSucceeds
