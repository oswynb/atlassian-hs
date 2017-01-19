{-# LANGUAGE RecordWildCards #-}

module Atlassian.Bitbucket.Cloud.Util where

import           Data.Maybe
import           Prelude                         hiding (id)

import           Atlassian.Bitbucket.Cloud.Types


numApprovals :: PR -> Int
numApprovals PR{..} = length $ filter approved (fromMaybe [] participants)

getSimplePipelineState :: GetPipelinesResponse -> SimplePipelineState
getSimplePipelineState GetPipelinesResponse{state=PipelineState{..}} =
  case _type of
    PipelineStatePending -> Pending
    PipelineStateCompleted -> let (Just (PipelineResult completedType)) = _result in case completedType of
      PipelineStateCompletedSuccessful -> Successful
      PipelineStateCompletedFailed     -> Failed
      PipelineStateCompletedError      -> Error
