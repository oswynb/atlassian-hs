module Atlassian.Internal.JSON where

import           Data.Aeson.Casing
import           Data.Aeson.Types

defaultSnake :: Options
defaultSnake = (aesonDrop 0 snakeCase){omitNothingFields = True}

defaultCamel :: Options
defaultCamel = (aesonDrop 0 camelCase){omitNothingFields = True}
