# vosonTCN

Twitter Conversation Networks and Analysis. This package uses the Twitter API v2 [Early Access](https://developer.twitter.com/en/products/twitter-api/early-access) endpoints to collect tweets and generate networks for threaded conversations identified using the new [conversation identifier](https://developer.twitter.com/en/docs/twitter-api/conversation-id).

#### Twitter OAuth Authentication

This package uses the app based authentication approach with an `OAuth2` bearer token that only has read-only access, as opposed to a user based one that requires an `OAuth1a` token and has broader permissions. To retrieve a bearer token the `consumer key` and `consumer secret` for a Developer `Standard Project` or `Academic Research Project` app that has been approved to use the Twitter API v2 endpoints are required. These can be found on the Twitter Developer Portals [Projects & Apps](https://developer.twitter.com/en/portal/projects-and-apps) page.

#### Search Endpoint

By default the `recent` search endpoint is used that makes available for collection only tweets that were made within the last 7 days. If the user has an `Academic Research Project` they can use the `tcn_threads` parameter `type = "all"` to collect all `historical` conversation tweets.

#### Twitter API v2 Rate-limits

The API `Recent search` endpoint where the conversation tweets are retrieved from has a rate-limit of 450 requests per 15 min (per app). A single request can retrieve 100 tweets, translating to an upper limit of 45,000 tweets per 15 mins. This package is not currently using user based access which would have smaller per user rate-limits.

There is currently a cap of 500,000 tweets that be collected per month per project under the Twitter API v2 `Standard` product track and 10,000,000 for the `Academic Research` track.

#### Limitations

- Not currently managing quotas or rate-limits. Doesn't check or wait until reset time when the rate-limit has been reached.

### Installation

```R
library(remotes)

install_github("vosonlab/vosonTCN")
```

### Usage

#### Get Access Token

Retrieve and save an app bearer token using its consumer keys.
```R
library(vosonTCN)

# -- get bearer token

token <- tcn_token(consumer_key = "xxxxxxxx",
                   consumer_secret = "xxxxxxxx")

# if you save the token to file this step only needs to be done once
saveRDS(token, "~/.tcn_token")
```

#### Collect Tweets

Using tweet urls collect conversation tweets and enough metadata to generate networks.
```R
# -- collect some conversation tweets

# read token from file
token <- readRDS("~/.tcn_token")

# choose a twitter conversation or multiple
# e.g https://twitter.com/Warcraft/status/1366810588039573505
#     https://twitter.com/Warcraft/status/1367583684770074625

# can use any tweet id that is part of the conversation thread and a list of ids, urls or both
tweet_ids <- c("https://twitter.com/Warcraft/status/1366810588039573505",
               "1367583684770074625")
               
tweets <- tcn_threads(tweet_ids, token)

# save collected data
saveRDS(tweets, "./tcn_tweets.rds")
```

#### Generate Networks

Two types of networks can be generated from the tweets collected. An `activity` network in which tweets are the nodes and an `actor` network where Twitter users are the nodes. Edges are the relationships between nodes, in both networks these are either a `reply` or a `quote`, signifying for example that a tweet is a reply-to another tweet or that a user has replied to another user.
```R
# -- generate networks

tweets <- readRDS("./tcn_tweets.rds")

# - activity

activity_net <- tcn_network(tweets)

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
saveRDS(activity_net, "./tcn_activity.rds")

# - actor

actor_net <- tcn_network(tweets, "actor")

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
# 1 13540157795~ 610331~ replied~ 1367046388530~ 2021-03-03T09:3~ "@Warcraft I am a professional designer,~
# 2 13526866149~ 610331~ replied~ 1366964588240~ 2021-03-03T04:1~ "@Warcraft I'd watch but I don't care to~
# 3 85150515685~ 610331~ replied~ 1366933290704~ 2021-03-03T02:0~ "@Warcraft Been gone for a bit whats wit~
# >

# save network to file
saveRDS(actor_net, "./tcn_actor.rds")
```

#### Network Graphs

```R
library(igraph)

network <- readRDS("./tcn_actor.rds")

g <- graph_from_data_frame(network$edges, vertices = network$nodes)

plot(g)
```
