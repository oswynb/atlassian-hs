module Atlassian.Bitbucket.Cloud.Client where

import           Network.HTTP.Client             hiding (Proxy)
import           Servant
import           Servant.Client

import           Atlassian.Bitbucket.Cloud.API
import           Atlassian.Bitbucket.Cloud.Types

restClient :: Client RestAPI
restClient = client restAPI

getPR                  :: Owner -> Slug -> Int -> BasicAuthData -> Manager -> BaseUrl -> ClientM PR
getPRs                 :: Owner -> Slug -> Maybe PRState -> Maybe Int -> BasicAuthData -> Manager -> BaseUrl -> ClientM (PagedResponse PR)
getRepositoriesForTeam :: Owner -> Maybe Int -> BasicAuthData -> Manager -> BaseUrl -> ClientM (PagedResponse GetRepositoriesResponse)
getPR :<|> getPRs  :<|> getRepositoriesForTeam = restClient
