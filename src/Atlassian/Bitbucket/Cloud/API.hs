{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Atlassian.Bitbucket.Cloud.API where

import           Servant

import           Atlassian.Bitbucket.Cloud.Types


type PagedAPI a =
  QueryParam "page" Int :> BasicAuth "bitbucket" Int :> Get '[JSON] (PagedResponse a)

type RestAPI = GetPR :<|> GetPRs :<|> GetRepositoriesForTeam

restAPI :: Proxy RestAPI
restAPI = Proxy

type GetPR                  = "repositories" :> Capture "owner"    Owner :> Capture "repo_slug" Slug :> "pullrequests" :> Capture "id" Int :> BasicAuth "bitbucket" Int :> Get '[JSON] PR
type GetPRs                 = "repositories" :> Capture "username" Owner :> Capture "repo_slug" Slug :> "pullrequests" :> QueryParam "state" PRState :> PagedAPI PR
type GetRepositoriesForTeam = "repositories" :> Capture "teamname" Owner :> PagedAPI GetRepositoriesResponse
