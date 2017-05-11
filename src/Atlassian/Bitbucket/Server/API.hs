{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Atlassian.Bitbucket.Server.API where

import           Data.Text                        (Text)
import           Servant

import           Atlassian.Bitbucket.Server.Types

type PagedAPI a =
  QueryParam "start" Int :> QueryParam "limit" Int :> BasicAuth "stash" Int :> Get '[JSON] (PagedResponse a)

type RestAPI  = GetRepos :<|> Commits :<|> GetPRs :<|> GetTaskCount :<|> BuildStatus

restAPI :: Proxy RestAPI
restAPI = Proxy

-- | Core API
type GetRepos     = "api" :> "1.0" :> "projects" :> Capture "project" Project :> "repos" :> PagedAPI GetReposResponse
type Commits      = "api" :> "1.0" :> "projects" :> Capture "project" Project :> "repos" :> Capture "slug" Slug :> "commits"       :> QueryParam "path" Text      :> PagedAPI CommitInfo
type GetPRs       = "api" :> "1.0" :> "projects" :> Capture "project" Project :> "repos" :> Capture "slug" Slug :> "pull-requests" :> QueryParam "state" PRState  :> PagedAPI PR
type GetTaskCount = "api" :> "1.0" :> "projects" :> Capture "project" Project :> "repos" :> Capture "slug" Slug :> "pull-requests" :> Capture "pullRequestId" Int :> "tasks" :> "count" :> BasicAuth "stash" Int :> Get '[JSON] TaskCount

-- | Build Status Extensions
type BuildStatus = "build-status" :> "1.0" :> "commits" :> Capture "commitId" Text :> PagedAPI CommitBuildStatus
