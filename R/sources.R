#' Lookup IDs of news sources
#'
#' Find out the IDs of news sources by querying a source name. Use the IDs returned in
#' \code{\link{get_news}} requests.
#'
#' @usage lookup_id(q = NULL, key = NULL)
#'
#' @param q String identifying a query to be searched through news source names.
#' @param key String specifying a valid CivicFeed API key. If \code{NULL},
#'   \code{get_news} will look for a relevant API key stored as environmental
#'   variable "CF_KEY".
#'
#' @return A named list of sources relevant to the query and their IDs. A maximum of 10
#'   relevant sources are returned.
#'
#' @references \url{https://developers.civicfeed.com/documentation}
#'
#' @examples
#' \dontrun{
#' # Good
#' sources = lookup_id("new york times")
#' ## NY times is the 7th element of "sources"
#'
#' # Bad
#' lookup_id("times")
#' ## too broad a query, source ID of NY times may not show up in 10 results returned}
#' @export
lookup_id = function(
  q = NULL,
  key = NULL
) {
  print.cf_api_list = function(x) {
    utils::str(list(results = x$results, content = x$content))
  }
  if (is.null(key)) {
    api_key = Sys.getenv("CF_KEY")
    if (identical(api_key, "")) {
      stop("Please provide argument 'key' or set env var CF_KEY to your CivicFeed API key")
    }
  } else {api_key = key}

  url = httr::modify_url("https://api-beta.civicfeed.com", path = "news/sources")

  query_params = list(q = q)

  cf_resp = suppressWarnings(
    tryCatch(
      httr::GET(
        url,
        httr::add_headers("x-api-key" = api_key),
        query = query_params
      ),
      error = function(e) {
        stop(sprintf("Invalid request\nCheck parameters and API key"), call. = FALSE)
      }
    )
  )
  if (httr::http_type(cf_resp) != "application/json") {
    stop("CivicFeed API did not return json", call. = FALSE)
  }
  if (stringr::str_detect(toString(httr::status_code(cf_resp)), "5.{2}")) {
    message("Retrying...")
    cf_resp = suppressWarnings(
      tryCatch(
        httr::RETRY(
          "httr::GET",
          url = url,
          httr::add_headers("x-api-key" = api_key),
          query = query_params,
          body = FALSE
        ),
        error = function(e) {
          stop(sprintf("Invalid request\nCheck parameters and API key"), call. = FALSE)
        }
      )
    )
  }
  if (httr::status_code(cf_resp) != 200) {
    stop(
      sprintf(
        "CivicFeed API request failed [%s]\n%s",
        httr::status_code(cf_resp),
        httr::http_status(cf_resp)$reason
      )
    )
  }
  parsed = suppressMessages(
    tryCatch(
      jsonlite::fromJSON(httr::content(cf_resp, "text"),
                         simplifyVector = FALSE),
      error = function(e) {
        stop(sprintf("Invalid response from CivicFeed API:\n%s", cf_resp), call. = FALSE)
      }
    )
  )
  if (!is.null(parsed$error)) {
    stop(parsed$error, call. = FALSE)
  }
  if (parsed$status != "ok") {
    stop(
      sprintf(
        "'%s' status received from CivicFeed API\nCheck your request",
        parsed$status
      ),
      call. = FALSE
    )
  }
  y = list()
  name = list()
  for (i in seq_along(parsed$sources)) {
    y[[i]] = parsed$sources[[i]]$id
    name = c(name, parsed$sources[[i]]$title)
  }
  names(y) = name
  y
}
