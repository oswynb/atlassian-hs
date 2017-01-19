{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Atlassian.Bitbucket.Cloud.API where

import           Data.Text                       (Text)
import           Servant

import           Atlassian.Bitbucket.Cloud.Types


type PagedAPI a =
  QueryParam "page" Int :> BasicAuth "bitbucket" Int :> Get '[JSON] (PagedResponse a)

type RestAPI = GetPR :<|> GetPRs :<|> GetRepositoriesForTeam :<|> GetPipelines :<|> PipelineResults

restAPI :: Proxy RestAPI
restAPI = Proxy

type GetPR                  = "repositories" :> Capture "owner"    Owner :> Capture "repo_slug" Slug :> "pullrequests" :> Capture "id" Int :> BasicAuth "bitbucket" Int :> Get '[JSON] PR
type GetPRs                 = "repositories" :> Capture "username" Owner :> Capture "repo_slug" Slug :> "pullrequests" :> QueryParam "state" PRState :> PagedAPI PR
type GetRepositoriesForTeam = "repositories" :> Capture "teamname" Owner :> PagedAPI GetRepositoriesResponse
-- TODO: Pipelines wot ⚓️
type GetPipelines           = "repositories" :> Capture "username" Owner :> Capture "repo_slug" Slug :> "pipelines/" :> PagedAPI GetPipelinesResponse
-- TODO: Even more wot 🍟
type PipelineResults        = Capture "username" Owner :> Capture "repo_slug" Slug :> "addon" :> "pipelines" :> "home#!" :> "results" :> Capture "uuid" Text :> Raw
