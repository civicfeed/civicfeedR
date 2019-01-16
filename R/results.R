#' Check number of news results matching a request
#'
#' Find out the number of results matching a CivicFeed News API request. Note: \code{results}
#' itself sends a request to the CivicFeed News API, so specify a valid API key
#' or have one stored as environment variable "CF_KEY".
#'
#' @usage results(q = NULL, sources = NULL, from = NULL, to = NULL, key =
#'   NULL)
#'
#' @param q String identifying a query to be searched through specified data
#'   sources.
#' @param sources Numeric vector of IDs of data source to search through. Use
#'   \code{lookup_id} to retrieve IDs.
#' @param from String identifying the start date (e.g. 2018-10-01) of content to
#'   search. Date is in UTC.
#' @param to String identifying the end date (e.g. 2018-10-01) of content to
#'   search. Date is in UTC.
#' @param key String specifying a valid CivicFeed API key. If \code{NULL},
#'   \code{get_news} will look for a relevant API key stored as environmental
#'   variable "CF_KEY".
#'
#' @return An integer specifying the number of results corresponding to the
#'   request.
#'
#' @references \url{https://developers.civicfeed.com/documentation}
#'
#' @examples
#' \dontrun{
#' results(q = "congress", from = "2018-10-01", to = "2018-11-01")
#' ## finds out how many articles about congress were posted between 2018-10-01 and 2018-11-01
#' }
#' @export
results = function(
  q = NULL,
  sources = NULL,
  from = NULL,
  to = NULL,
  key = NULL
) {
  news = get_news(
    q = q,
    sources = sources,
    from = from,
    to = to,
    key = key
  )
  return(news$results)
}
