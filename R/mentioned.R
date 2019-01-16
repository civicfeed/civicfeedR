#' Filter news mentioning specific entities
#'
#' From article results retrieved using \code{\link{get_news}}, extract a subset
#' that mention entities of interest and create a tibble of the features of that
#' subset.
#'
#' @usage mentioned(..., entities, titles = TRUE, published = TRUE, sources =
#'   TRUE, snippets = FALSE, highlights = FALSE, shares = FALSE)
#'
#' @param ... Objects of class "cf_api_list" retrieved using
#'   \code{\link{get_news}}.
#' @param entities Numeric vector of entity IDs OR character vector of entity
#'   names. Character vector is case-insensitive. Advisable to check \code{term}
#'   entries in output returned by \code{\link{entities_from}} to avoid
#'   misnaming.
#' @param titles Logical indicating whether to include titles of articles in
#'   tibble. Defaults to \code{TRUE}.
#' @param published Logical indicating whether to include unix timestamps for
#'   dates articles were published in tibble. Defaults to \code{TRUE}.
#' @param sources Logical indicating whether to include sources of articles in
#'   tibble. Defaults to \code{TRUE}.
#' @param snippets Logical indicating whether to include snippets of articles in
#'   tibble. Defaults to \code{FALSE}.
#' @param highlights Logical indicating whether  to include highlights of
#'   articles in tibble. Defaults to \code{FALSE}. If \code{TRUE}, note:
#'   articles may have more than one highlight. Each highlight will occupy
#'   separate row in the tibble.
#' @param shares Logical indicating whether to include information about social
#'   media shares of articles in tibble. Defaults to \code{FALSE}.
#'
#' @return A tibble displaying features of articles mentioning specified
#'   entitites.
#'
#' @examples
#' \dontrun{
#' news1 = get_news("congress")
#' news2 = get_news("white house")
#' # Good
#' mentioned(news1, news2, entities = c("hillary clinton", "president donald trump"), snippets = TRUE)
#' ## creates a tibble of titles, dates published, sources and snippets of articles about congress
#' or white house mentioning h. clinton or trump
#'
#' # Bad
#' mentioned(news1, news2, entities = c("hilary clinton", "donald trump"), snippets = TRUE)
#' ## no matches due to spelling error and naming disparity}
#' @export
mentioned = function(
  ...,
  entities,
  titles = TRUE,
  published = TRUE,
  sources = TRUE,
  snippets = FALSE,
  highlights = FALSE,
  shares = FALSE
  ) {
  `%>%` = magrittr::`%>%`
  x = list(...)
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
  }
  df = entities_from(...) %>%
    dplyr::filter(
      entity_id == entities[1]|
        stringr::str_to_lower(term) == stringr::str_to_lower(entities[1])
      )
  if (length(entities) > 1) {
    for (i in 2:length(entities)) {
      df = rbind(df, entities_from(...) %>%
        dplyr::filter(
          entity_id == entities[i]|
            stringr::str_to_lower(term) == stringr::str_to_lower(entities[i])
          )
        )
    }
  }
  a = list(titles, published, sources, highlights, snippets, shares)
  b = list(titles_from, published_from, sources_from, highlights_from, snippets_from, shares_from)
  for (i in seq_along(a)) {
    if (a[[i]]) {
      df = dplyr::left_join(df, b[[i]](...), by = "article_id")
    }
  }
  df
}
