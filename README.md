# vosonTCN
Twitter Conversation Networks and Analysis.

### Installation

``` r
# To install from a private repo, use auth_token with a token
# from https://github.com/settings/tokens. You only need the
# repo scope.

library(remotes)

install_github("vosonlab/vosonTCN", auth_token = "xxxxxxxx")

```

### Usage

#### Twitter OAuth authentication

Authentication requires the `consumer key` and `consumer secret` for a Developer Project App that has been approved to use the API v2 Early Access endpoints. This is application based authentication and uses an OAuth2 bearer token as opposed to user based authentication that uses an OAuth1a token.

Under the Standard Product track this allows a cap of 500,000 tweets to be collected per month per project.

#### Twitter API v2 rate limits

The API `Recent search` endpoint where the conversation tweets are retrieved from has a rate-limit of 450 requests per 15 min (per app). A single request can retrieve 100 tweets, meaning a maximum limit of 45,000 tweets per 15 mins. This package is not currently using user access which would have smaller per user rate-limits.

#### Example

``` r
library(vosonTCN)

# -- create bearer token

token <- get_token(consumer_key = "xxxxxxxx",
                   consumer_secret = "xxxxxxxx")
saveRDS(token, "~/.tcn_token")

# -- collect some conversation tweets

token <- readRDS("~/.tcn_token")

# choose a twitter conversation
# e.g https://twitter.com/Warcraft/status/1366810588039573505
# can use any tweet id that is part of the conversation thread
tweets <- get_convo(token, tweet_id = "1366810588039573505")

# saveRDS(tweets, "./tcn_tweets.rds")

# -- generate network

network <- net_convo(tweets)

> nrow(network$nodes)
[1] 27

> print(head(network$nodes, n = 3))
# # A tibble: 3 x 5
#   tweet_id     user_id      source      created_at     text                           
#   <chr>        <chr>        <chr>       <chr>          <chr>                          
# 1 13670463885~ 13540157795~ Twitter fo~ 2021-03-03T09~ "@Warcraft I am a professional~
# 2 13669645882~ 13526866149~ Twitter We~ 2021-03-03T04~ "@Warcraft I'd watch but I don~
# 3 13669332907~ 85150515685~ Twitter fo~ 2021-03-03T02~ "@Warcraft Been gone for a bit~

> nrow(network$edges)
[1] 27

> print(head(network$edges, n = 3))
# # A tibble: 3 x 3
#   from                to                  type      
#   <chr>               <chr>               <chr>     
# 1 1367046388530339844 1366810588039573505 replied_to
# 2 1366964588240121859 1366810588039573505 replied_to
# 3 1366933290704322562 1366810588039573505 replied_to

# saveRDS(network, "./tcn_network.rds")
```

#### Network Graph

```R
library(igraph)

# network <- readRDS("./tcn_network.rds")

g <- graph_from_data_frame(network$edges, vertices = network$nodes)
```
