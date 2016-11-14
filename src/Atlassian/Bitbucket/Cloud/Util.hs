{-# LANGUAGE RecordWildCards #-}

module Atlassian.Bitbucket.Cloud.Util where

import           Data.Maybe

import           Atlassian.Bitbucket.Cloud.Types


numApprovals :: PR -> Int
numApprovals PR{..} = length $ filter approved (fromMaybe [] participants)
