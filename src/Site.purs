module Site where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Traversable (traverse)
import Foreign (F, Foreign, readArray, readString)
import Foreign.Index ((!))

type Isolate
  = { name :: String
    , kl :: String
    , st :: String
    , ocl :: String
    }

foreign import isolatesImpl :: Foreign

readIsolate :: Foreign -> F Isolate
readIsolate value = do
  name <- value ! "name" >>= readString
  kl <- value ! "kl" >>= readString
  st <- value ! "st" >>= readString
  ocl <- value ! "ocl" >>= readString
  pure { name, kl, st, ocl }

isolates :: Array Isolate
isolates = either (const []) identity $ runExcept $ readArray isolatesImpl >>= traverse readIsolate

foreign import baseURL :: String
