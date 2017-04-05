module Atlassian.Bitbucket.Cloud.Client where

import           Servant
import           Servant.Client

import           Atlassian.Bitbucket.Cloud.API
import           Atlassian.Bitbucket.Cloud.Types

restClient :: Client RestAPI
restClient = client restAPI

getPR                  :: Owner -> Slug -> Int -> BasicAuthData -> ClientM PR
getPRs                 :: Owner -> Slug -> Maybe PRState -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse PR)
getRepositoriesForTeam :: Owner -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse GetRepositoriesResponse)
getPipelines           :: Owner -> Slug -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse GetPipelinesResponse)
getPR :<|> getPRs  :<|> getRepositoriesForTeam :<|> getPipelines :<|> pipelineResults = restClient
