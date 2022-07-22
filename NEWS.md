# voson.tcn 0.4.1

## Major Changes
- Package now requires R version `4.1` or greater.

## Minor Changes
- Changed `tcn_tweets` to be able to accept any number of tweet ids or urls. Previously was set to a maximum of 100.
- Added `referenced_tweets` parameter to `tcn_tweets` function so as to now have the option to specify if referenced tweets should also be retrieved. The default value is `FALSE`.
- Added retry on rate limit `retry_on_limit` parameter to `tcn_tweets` and `tcn_counts` functions.
- Changed `warning` messages to use the `message` function instead.
- Removed `magrittr` import and replaced pipes with native R version.
- `tcn_counts` will now also accept tweet urls.
- Changed extraction of tweet ids from urls to use a more robust regex.

## Bug Fixes
- Added a 1 second delay to initial full-archive search requests as 15 min API rate-limit timeout was being triggered when
multiple low number results returned too quickly.

# voson.tcn 0.3.3

## Bug Fixes
- Fixed an error produced by arranging users by tweet count in `tcn_threads`.

# voson.tcn 0.3.2

## Minor Changes
- Updated standard package documentation, added citation and README.Rmd.

# voson.tcn 0.3.1

## Major Changes
- Added the `tcn_counts` function for retrieving tweet counts over time for conversation ids. This uses the API v2 `tweets/counts` endpoint and does not contribute to the monthly tweet cap. It could be used prior to collecting conversation threads to identify conversations to target or conversations to add to a skip list.

## Minor Changes
- Full-archive endpoint searches with the optional API v2 `max_results` querystring parameter have started to return response status code
400 for values over 100.
- The `max_results` parameter for function `tcn_threads` has been changed from the maximum total results, and now refers to and allows the API parameter of the same name above to be set. The default value has been set to 100. If left at default academic projects using the full-archive search endpoint will only collect 30,000 tweets per 15 minute rate-limit.
- Previous `max_results` parameter for the function `tcn_threads` has been renamed to `max_total`.
- Added `retry_on_limit` parameter to `tcn_threads` to allow waiting for API rate-limit to reset before continuing, rather than exiting upon reaching the limit.
- Updated standard package documentation.

# voson.tcn 0.2.4

## Major Changes
- Added `tcn_tweets` function. This function accepts tweet URL's or ID's and collects specific tweet data using the API v2 `tweets` endpoint. Currently supports only bearer access tokens.

## Minor Changes
- Changed `httr` request user-agent string via header field rather than options.
- Added `converstion_id` as node and edge attribute to networks.
- Thread collection now returns a list of four dataframes, `tweets`, `users`, `errors` and `meta`. The `errors` dataframe contains partial errors such as those caused when trying to retrieve tweets that are not publicly available. The `meta` dataframe contains search results metadata such as tweet id range, number of results and pagination token.
- Added unnesting of `public_metrics` during the JSON to dataframe process.
- Added unnesting of referenced tweets to network creation.
- Outputs a warning if rate-limit reached.
- Renamed `end-point` parameter to `endpoint` to be consistent with twitter documentation.
- Added `max_results` parameter to coarsely limit how many tweets are collected in a `tcn_threads` operation. This is to assist with managing the monthly tweet cap placed on projects using the API search endpoints.
- Expanded tweet and user fields collected.

## Bug Fixes
- Fixed an issue with partial errors preventing `get_tweets` from returning requested tweets.

# voson.tcn 0.1.10

## Bug Fixes
- Fixed `object 'df_convo' not found` message when endpoint related error occurs.

## Minor Changes
- Added `start_time` and `end_time` parameters for academic track historical search endpoint (`end_point = "all"`). These are UTC datetime strings in ISO 8601 format. If unused the API uses a default UTC start time of 30 days ago and a default end time of the current time minus 30 seconds.

# voson.tcn 0.1.8

- CRAN version.

## Bug Fixes
- Removed extraneous self-loop from conversation starter node in activity networks.
- Added a warning for threads that return no tweets and fixed `anti_join` on `tweet_id` error.

## Minor Changes
- Added public metrics, referenced tweet objects and user profile fields to tweet expansion and object fields collected.
- The `tcn_threads` function now produces a named list of dataframes: `tweets` and `users` for tweet data and user metadata.
- Added `bearer` parameter to the `tcn_token` function to assign a bearer token directly rather than retrieving it with app keys.

# voson.tcn 0.1.3

## Initial Version
- Retrieve a Twitter bearer token using app consumer keys with the `tcn_token` function.
- Option to collect on multiple conversations by passing multiple tweet URLs or IDs in a list to `tcn_threads` using the `tweet_ids` parameter.
- Option to use historical rather than a recent search API endpoint by using `tcn_threads` with the `end_point = "all"` parameter. 
- Generate either an `actor` or `activity` network using the `tcn_network` function with the `type = "activity"` parameter.
- Created a package, package documentation and pkgdown site.
