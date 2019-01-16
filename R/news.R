#' Get news from CivicFeed
#'
#' Send out requests to the CivicFeed News API. The response will be parsed and
#' returned as a nested list.
#'
#' @usage get_news(q = NULL, sources = NULL, from = NULL, to = NULL, pages = 1,
#'   results = 10, all = FALSE, sort = NULL, key = NULL)
#'
#' @param q String identifying a query to be searched through specified data
#'   sources.
#' @param sources Numeric vector of IDs of data source to search through. Use
#'   \code{\link{lookup_id}} to retrieve IDs.
#' @param from String identifying the start date (e.g. 2018-10-01) of content to
#'   search. Date is in UTC.
#' @param to String identifying the end date (e.g. 2018-10-01) of content to
#'   search. Date is in UTC.
#' @param pages Integer specifying number of pages of results to return.
#'   Defaults to 1. If number of pages specified exceeds number of pages
#'   available, maximum number of pages available will be returned. Note:
#'   \code{get_news} makes a new call to API for every additional page.
#' @param results Integer specifying number of results per page returned. Max
#'   30. Defaults to 30.
#' @param all Logical indicating whether to return all results. Defaults to
#'   \code{FALSE}. If \code{TRUE}, argument \code{pages} will be ignored. Use
#'   \code{\link{results}} to find out how many results match your request.
#' @param sort String specifying how results should be ordered. Available
#'   options are: \code{"created"}, \code{"socialcount_last"} and
#'   \code{"_score"}.
#' @param key String specifying a valid CivicFeed API key. If \code{NULL},
#'   \code{get_news} will look for a relevant API key stored as environment
#'   variable "CF_KEY".
#'
#' @return S3 object (class: \code{"cf_api_list"}) with 2 elements:
#'   \code{results}, which indicates the total number of articles corresponding
#'   to the request, and \code{content}, which contains all available features
#'   of the retrieved articles.
#'
#' @seealso \code{\link{titles_from}} to restructure retrieved data
#'
#' @references \url{https://developers.civicfeed.com/documentation}
#'
#' @examples
#' \dontrun{
#' get_news(q = "congress", pages = 2, sort = "_score")
#' ## gets all features from 60 articles most relevant to query "congress"
#'
#' get_news(sources = lookup_id("bbc news")[[1]], results = 10, from = "2018-10-01", sort = "socialcount_last")
#' ## gets all features from 10 most popular articles posted by bbc news after 2018-10-01}
#' @export
get_news = function(
  q = NULL,
  sources = NULL,
  from = NULL,
  to = NULL,
  pages = 1,
  results = 30,
  all = FALSE,
  sort = NULL,
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
  }
  if (pages <= 0 | !isTRUE(all.equal(pages, as.integer(pages)))) {
    stop("argument 'pages' must be a positive integer")
  }
  sources = purrr::pmap(as.list(sources), paste, sep = ",")
  get_page = function(page) {
    query_params = list(
      q = q,
      sources = sources,
      from = from,
      to = to,
      page = page,
      results = results,
      sort = sort
    )
    if (is.null(key)) {
      api_key = Sys.getenv("CF_KEY")
    } else {
      api_key = key
    }

    cf_resp = suppressWarnings(
      tryCatch(
        httr::GET(
          "https://api-beta.civicfeed.com/news/search",
          httr::add_headers("x-api-key" = api_key),
          query = query_params,
          httr::user_agent("http://github.com/gsantoshi/civicfeedR")
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
            url = "https://api-beta.civicfeed.com/news/search",
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
        ),
        call. = FALSE
      )
    }
    p = tryCatch(
      suppressMessages(
        jsonlite::fromJSON(httr::content(cf_resp, "text"),
                           simplifyVector = FALSE)
        ),
      error = function(e) {
        stop(sprintf("Invalid response from CivicFeed API:\n%s", cf_resp), call. = FALSE)
      }
    )
    if (!is.null(p$error)) {
      stop(p$error, call. = FALSE)
    }
    if (p$status != "ok") {
      stop(
        sprintf(
          "'%s' status received from CivicFeed API\nCheck your request",
          p$status
        ),
        call. = FALSE
      )
    }
    p
  }
  parsed = get_page(page = 1)
  articles = parsed$articles
  total_results = parsed$results
  if (all == FALSE & (pages == 1 | ceiling(total_results/results) == 1)) {
    articles = articles
  } else if (all == FALSE & pages <= ceiling(total_results/results)) {
    for (i in 2:pages) {
      articles = c(articles, get_page(i)$articles)
      Sys.sleep(stats::runif(1, 1, 7))
    }
  } else {
    for (i in 2:ceiling(total_results/30)) {
      articles = c(articles, get_page(i)$articles)
      Sys.sleep(stats::runif(1, 1, 7))
    }
  }

  structure(
    list(
      results = total_results,
      content = articles
    ),
    class = "cf_api_list"
  )
}
