module Atlassian.Bitbucket.Server.Client where

import           Data.Text                        (Text)
import           Servant
import           Servant.Client

import           Atlassian.Bitbucket.Server.API
import           Atlassian.Bitbucket.Server.Types

restClient :: Client RestAPI
restClient = client restAPI

getRepos     :: Project ->                          Maybe Int -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse GetReposResponse)
getCommits   :: Project -> Slug -> Maybe Text    -> Maybe Int -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse CommitInfo)
getPRs       :: Project -> Slug -> Maybe PRState -> Maybe Int -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse PR)
getTaskCount :: Project -> Slug -> Int           ->                           BasicAuthData -> ClientM TaskCount
buildStatus  :: Text                             -> Maybe Int -> Maybe Int -> BasicAuthData -> ClientM (PagedResponse CommitBuildStatus)

getRepos :<|> getCommits :<|> getPRs :<|> getTaskCount :<|> buildStatus = restClient
