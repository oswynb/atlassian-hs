{-# LANGUAGE RecordWildCards #-}

module Atlassian.Bitbucket.Server.Util where

import qualified Data.Vector                      as V
import           Prelude                          hiding (id)

import           Atlassian.Bitbucket.Server.Types

numApprovals :: PR -> Int
numApprovals PR{..} = V.length $ V.filter approved reviewers
