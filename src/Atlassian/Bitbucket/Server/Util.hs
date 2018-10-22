{-# LANGUAGE RecordWildCards #-}

module Atlassian.Bitbucket.Server.Util where

import           Prelude                          hiding (id)

import           Atlassian.Bitbucket.Server.Types

numApprovals :: PR -> Int
numApprovals PR{..} = length $ filter (\x -> status x == Approved) reviewers
