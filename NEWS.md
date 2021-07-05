# voson.tcn 0.1.9.9000

## Bug Fixes
- Fixed `object 'df_convo' not found` message when end-point related error occurs.

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
- Option to use historical rather than a recent search API end-point by using `tcn_threads` with the `end_point = "all"` parameter. 
- Generate either an `actor` or `activity` network using the `tcn_network` function with the `type = "activity"` parameter.
- Created a package, package documentation and pkgdown site.
