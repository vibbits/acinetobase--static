module Site where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (either)
import Data.Traversable (traverse)
import Foreign (F, Foreign, readArray, readString)
import Foreign.Index ((!))

type Isolate =
  { name :: String
  , kl :: String
  , stPas :: String
  , stOx :: String
  , ocl :: String
  , mt :: String
  }

foreign import isolatesImpl :: Foreign

readIsolate :: Foreign -> F Isolate
readIsolate value = do
  name <- value ! "name" >>= readString
  kl <- value ! "kl" >>= readString
  stPas <- value ! "st_pas" >>= readString
  stOx <- value ! "st_ox" >>= readString
  ocl <- value ! "ocl" >>= readString
  mt <- value ! "mt" >>= readString
  pure { name, kl, stPas, stOx, ocl, mt }

isolates :: Array Isolate
isolates = either (const []) identity $ runExcept $ readArray isolatesImpl >>= traverse readIsolate

foreign import baseURL :: String
