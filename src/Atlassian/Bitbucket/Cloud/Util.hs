{-# LANGUAGE RecordWildCards #-}

module Atlassian.Bitbucket.Cloud.Util where

import           Data.Maybe
import           Data.Proxy
import           Data.Text                       (Text)
import           Network.URI
import           Prelude                         hiding (id)
import           Servant.Utils.Links

import           Atlassian.Bitbucket.Cloud.API
import           Atlassian.Bitbucket.Cloud.Types


numApprovals :: PR -> Int
numApprovals PR{..} = length $ filter approved (fromMaybe [] participants)

prBranch :: PR -> Maybe Text
prBranch PR{..} = case branch source of
  Nothing -> Nothing
  Just (SourceBranch b) -> Just b

pipelineLink :: Owner -> Slug -> GetPipelinesResponse -> URI
pipelineLink owner slug response =
  let baseLink = safeLink restAPI (Proxy :: Proxy PipelineResults) owner slug (uuid response)
  in baseLink{uriAuthority = Just (URIAuth "" "bitbucket.org/" "")}

getSimplePipelineState :: GetPipelinesResponse -> SimplePipelineState
getSimplePipelineState GetPipelinesResponse{state=PipelineState{..}} =
  case _type of
    PipelineStatePending -> Pending
    PipelineStateCompleted -> let (Just (PipelineResult completedType)) = _result in case completedType of
      PipelineStateCompletedSuccessful -> Successful
      PipelineStateCompletedFailed     -> Failed
      PipelineStateCompletedError      -> Error
