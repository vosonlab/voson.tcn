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

### Example

#### Get Access Token
``` r
library(vosonTCN)

# -- create bearer token

token <- get_token(consumer_key = "xxxxxxxx",
                   consumer_secret = "xxxxxxxx")

# if you save the token to file this only needs to be done once
saveRDS(token, "~/.tcn_token")
```

#### Collect Tweets
```R
# -- collect some conversation tweets

# read token from file
token <- readRDS("~/.tcn_token")

# choose a twitter conversation
# e.g https://twitter.com/Warcraft/status/1366810588039573505

# can use any tweet id that is part of the conversation thread
tweets <- get_convo(token, tweet_id = "1366810588039573505")

# save collected data
saveRDS(tweets, "./tcn_tweets.rds")
```

#### Generate Networks
```R
# -- generate networks

# - activity

activity_net <- net_convo(tweets)

> nrow(activity_net$nodes)
[1] 27

> print(head(activity_net$nodes, n = 3))
# # A tibble: 3 x 5
#   tweet_id     user_id      source      created_at     text                           
#   <chr>        <chr>        <chr>       <chr>          <chr>                          
# 1 13670463885~ 13540157795~ Twitter fo~ 2021-03-03T09~ "@Warcraft I am a professional~
# 2 13669645882~ 13526866149~ Twitter We~ 2021-03-03T04~ "@Warcraft I'd watch but I don~
# 3 13669332907~ 85150515685~ Twitter fo~ 2021-03-03T02~ "@Warcraft Been gone for a bit~

> nrow(activity_net$edges)
[1] 27

> print(head(activity_net$edges, n = 3))
# # A tibble: 3 x 3
#   from                to                  type      
#   <chr>               <chr>               <chr>     
# 1 1367046388530339844 1366810588039573505 replied_to
# 2 1366964588240121859 1366810588039573505 replied_to
# 3 1366933290704322562 1366810588039573505 replied_to

# save network to file
saveRDS(activity_net, "./tcn_activity_net.rds")

# - actor

actor_net <- net_convo(tweets, "actor")

> nrow(actor_net$nodes)
[1] 24

> print(head(actor_net$nodes, n = 3))
# # A tibble: 3 x 2
#   user_id             source             
#   <chr>               <chr>              
# 1 1354015779528744960 Twitter for iPhone 
# 2 1352686614909214721 Twitter Web App    
# 3 851505156856455168  Twitter for Android

> nrow(actor_net$edges)
[1] 27

> print(head(actor_net$edges, n = 3))
# # A tibble: 3 x 6
#   from         to      type     tweet_id       created_at       text                                                             
#   <chr>        <chr>   <chr>    <chr>          <chr>            <chr>                                                            
# 1 13540157795~ 610331~ replied~ 1367046388530~ 2021-03-03T09:3~ "@Warcraft I am a professional designer, with 4 years experience~
# 2 13526866149~ 610331~ replied~ 1366964588240~ 2021-03-03T04:1~ "@Warcraft I'd watch but I don't care to see Hpal/Fire Mage/x fo~
# 3 85150515685~ 610331~ replied~ 1366933290704~ 2021-03-03T02:0~ "@Warcraft Been gone for a bit whats with this free for all buff~
# >

# save network to file
saveRDS(actor_net, "./tcn_actor_net.rds")
```

#### Network Graphs

```R
library(igraph)

network <- readRDS("./tcn_actor_net.rds")

g <- graph_from_data_frame(network$edges, vertices = network$nodes)

plot(g)
```
