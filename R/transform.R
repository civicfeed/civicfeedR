#' Restructure retrieved news
#'
#' Transform features of articles retrieved from CivicFeed News API into tidy
#' tibbles or lists
#'
#' @usage titles_from(..., format = c("tibble", "list"))
#' @usage links_from(..., format = c("tibble", "list"))
#' @usage published_from(..., format = c("tibble", "list"))
#' @usage sources_from(..., format = c("tibble", "list"))
#' @usage shares_from(..., format = c("tibble", "list"))
#' @usage entities_from(..., format = c("tibble", "list"))
#' @usage highlights_from(..., format = c("tibble", "list"))
#' @usage snippets_from(..., format = c("tibble", "list"))
#'
#' @param ... Objects of class "cf_api_list" retrieved using
#'   \code{\link{get_news}}
#' @param format String specifying whether to return data as a "tibble" or
#'   "list". Defaults to "tibble".
#'
#' @details Each function extracts a different feature of the articles
#'   retrieved. The functions can be used in combination to create your desired
#'   dataset.
#'
#' @seealso \code{\link{get_news}} to retrieve news from CivicFeed
#'
#' @examples
#' \dontrun{
#' news1 = get_news("congress")
#' news2 = get_news("white house")
#' # Good
#' entities_from(news1, news2)
#' ## creates tibble of entities found in articles about congress and the white house
#'
#' sources_from(news1, news2, format = "list")
#' ## creates list of sources of articles about congress and the white house
#'
#' # Bad
#' sources_from(news1, news2, "list")
#' ## format argument must be labelled}
#' @describeIn from Extracts titles from retrieved articles
#' @export
titles_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  df = list()
  z = 0
  title_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      title = list()
      for (i in seq_along(x[[n]]$content)) {
        title = c(title, toNA(x[[n]]$content[[i]]$title))
        article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
      }
      df[[n]] = dplyr::tibble(article_id = unlist(article_id, use.names = FALSE),
                              title = unlist(title, use.names = FALSE))
    } else {
      for (i in seq_along(x[[n]]$content)) {
        title_list[[z + i]] = list()
        title_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
        title_list[[z + i]][[2]] = toNA(x[[n]]$content[[i]]$title)
        names(title_list[[z + i]]) = c("article_id", "title")
      }
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(title_list)
  }
}

#' @describeIn from Extracts url, thumbnail and screenshot links from retrieved
#'   articles
#' @export
links_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  link_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      url = list()
      thumbnail = list()
      screenshot = list()
      for (i in seq_along(x[[n]]$content)) {
        url = c(url, toNA(x[[n]]$content[[i]]$url))
        thumbnail = c(thumbnail, toNA(x[[n]]$content[[i]]$thumbnail))
        screenshot = c(screenshot, toNA(x[[n]]$content[[i]]$screenshot))
        article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
      }
      df[[n]] = dplyr::tibble(
        article_id = unlist(article_id, use.names = FALSE),
        url = unlist(url, use.names = FALSE),
        thumbnail = unlist(thumbnail, use.names = FALSE),
        screenshot = unlist(screenshot, use.names = FALSE)
      )
    } else {
      for (i in seq_along(x[[n]]$content)) {
        link_list[[z + i]] = list()
        link_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
        link_list[[z + i]][[2]] = toNA(x[[n]]$content[[i]]$url)
        link_list[[z + i]][[3]] = toNA(x[[n]]$content[[i]]$thumbnail)
        link_list[[z + i]][[4]] = toNA(x[[n]]$content[[i]]$screenshot)
        names(link_list[[z + i]]) = c("article_id", "url", "thumbnail", "screenshot")
      }
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(link_list)
  }
}

#' @describeIn from Extracts unix timestamps for dates on which retrieved
#'   articles were published
#' @export
published_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  published_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      published = list()
      for (i in seq_along(x[[n]]$content)) {
        published = c(published, toNA(x[[n]]$content[[i]]$published))
        article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
      }
      df[[n]] = dplyr::tibble(article_id = unlist(article_id, use.names = FALSE),
                              published = unlist(published, use.names = FALSE))
    } else {
      for (i in seq_along(x[[n]]$content)) {
        published_list[[z + i]] = list()
        published_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
        published_list[[z + i]][[2]] = toNA(x[[n]]$content[[i]]$published)
        names(published_list[[z + i]]) = c("article_id", "published")
      }
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(published_list)
  }
}

#' @describeIn from Extracts source IDs and names from retrieved articles
#' @export
sources_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  source_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      source_id = list()
      source_name = list()
      for (i in seq_along(x[[n]]$content)) {
        source_id = c(source_id, toNA(x[[n]]$content[[i]]$source$id))
        source_name = c(source_name, toNA(x[[n]]$content[[i]]$source$name))
        article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
      }
      df[[n]] = dplyr::tibble(
        article_id = unlist(article_id, use.names = FALSE),
        source_id = unlist(source_id, use.names = FALSE),
        source_name = unlist(source_name, use.names = FALSE)
      )
    } else {
      suppressWarnings(
        for (i in seq_along(x[[n]]$content)) {
          source_list[[z + i]] = list()
          source_list[[z + i]][[1]] = x[[n]]$content[[i]]$id
          source_list[[z + i]][[2]] = x[[n]]$content[[i]]$source
          names(source_list[[z + i]]) = c("article_id", "source")
        }
      )
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(source_list)
  }
}

#' @describeIn from Extracts timestamped social media shares, reactions and
#'   comments from retrieved articles
#' @export
shares_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  shares_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      fb_shares = list()
      fb_reactions = list()
      fb_cm = list()
      timestamp = list()
      for (i in seq_along(x[[n]]$content)) {
        for (m in seq_along(x[[n]]$content[[i]]$shares)) {
          fb_shares = c(fb_shares, toNA(x[[n]]$content[[i]]$shares[[m]]$fb_share))
          fb_reactions = c(fb_reactions, toNA(x[[n]]$content[[i]]$shares[[m]]$fb_reactions))
          fb_cm = c(fb_cm, toNA(x[[n]]$content[[i]]$shares[[m]]$fb_cm))
          timestamp = c(timestamp, toNA(x[[n]]$content[[i]]$shares[[m]]$timestamp))
          article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
        }
      }
      df[[n]] = dplyr::tibble(
        article_id = unlist(article_id, use.names = FALSE),
        fb_shares = unlist(fb_shares, use.names = FALSE),
        fb_reactions = unlist(fb_reactions, use.names = FALSE),
        fb_cm = unlist(fb_cm, use.names = FALSE),
        timestamp = unlist(timestamp, use.names = FALSE)
      )
    } else {
      suppressWarnings(
        for (i in seq_along(x[[n]]$content)) {
          shares_list[[z + i]] = list()
          shares_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
          shares_list[[z + i]][[2]] = list()
          names(shares_list[[z + i]]) = c("article_id", "shares")
          for (m in seq_along(x[[n]]$content[[i]]$shares)) {
            shares_list[[z + i]][[2]][[m]] = x[[n]]$content[[i]]$shares[[m]]
          }
        }
      )
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(shares_list)
  }
}

#' @describeIn from Extracts entity IDs, names, types etc. from retrieved articles
#' @export
entities_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  entity_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      entity_id = list()
      term = list()
      type = list()
      state = list()
      for (i in seq_along(x[[n]]$content)) {
        if (!identical(x[[n]]$content[[i]]$entities, FALSE)) {
          for (m in seq_along(x[[n]]$content[[i]]$entities)) {
            entity_id = c(entity_id, toNA(x[[n]]$content[[i]]$entities[[m]]$civicfeed_id))
            type = c(type, toNA(x[[n]]$content[[i]]$entities[[m]]$type))
            term = c(term, toNA(x[[n]]$content[[i]]$entities[[m]]$term))
            article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
            state = c(state, toNA(x[[n]]$content[[i]]$entities[[m]]$state))
          }
        }
      }
      df[[n]] = dplyr::tibble(
        article_id = unlist(article_id, use.names = FALSE),
        entity_id = unlist(entity_id, use.names = FALSE),
        term = unlist(term, use.names = FALSE),
        type = unlist(type, use.names = FALSE),
        state = unlist(state, use.names = FALSE)
      )
    } else {
      suppressWarnings(
        for (i in seq_along(x[[n]]$content)) {
          entity_list[[z + i]] = list()
          entity_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
          entity_list[[z + i]][[2]] = list()
          names(entity_list[[z + i]]) = c("article_id", "entity")

          for (m in seq_along(x[[n]]$content[[i]]$entities)) {
            entity_list[[z + i]][[2]][[m]] = x[[n]]$content[[i]]$entities[[m]]
          }
        }
      )
      z = sum(z, length(x[[n]]$content))
    }
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(entity_list)
  }
}


#' @describeIn from Extracts highlights from retrieved articles
#' @export
highlights_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  highlight_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      highlight = list()
      for (i in seq_along(x[[n]]$content)) {
        for (m in seq_along(x[[n]]$content[[i]]$highlight)) {
          highlight = c(highlight, x[[n]]$content[[i]]$highlight[[m]])
          article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
        }
      }
      df[[n]] = dplyr::tibble(
        article_id = unlist(article_id, use.names = FALSE),
        highlight = unlist(highlight, use.names = FALSE)
      )
    } else {
      suppressWarnings(
        for (i in seq_along(x[[n]]$content)) {
          highlight_list[[z + i]] = list()
          highlight_list[[z + i]][[1]] = toNA(x[[n]]$content[[i]]$id)
          highlight_list[[z + i]][[2]] = list()
          names(highlight_list[[z + i]]) = c("article_id", "highlight")

          for (m in seq_along(x[[n]]$content[[i]]$highlight)) {
            highlight_list[[z + i]][[2]][[m]] = x[[n]]$content[[i]]$highlight[[m]]
          }
        }
      )
      z = sum(z, length(x[[n]]$content))
    }
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(highlight_list)
  }
}

#' @describeIn from Extracts snippets from retrieved articles
#' @export
snippets_from = function(
  ...,
  format = c("tibble", "list")
) {
  format = match.arg(format)
  x = list(...)
  z = 0
  df = list()
  snippet_list = list()
  for (n in seq_along(x)) {
    if (class(x[[n]]) != "cf_api_list") {
      stop("All arguments in '...' must have class 'cf_api_list'")
    }
    if (identical(x[[n]]$content, list())) {
      warning(sprintf("Argument %s in '...' has no content", n))
      next
    }
    if (format == "tibble") {
      article_id = list()
      snippet = list()
      for (i in seq_along(x[[n]]$content)) {
        snippet = c(snippet, toNA(x[[n]]$content[[i]]$snippet))
        article_id = c(article_id, toNA(x[[n]]$content[[i]]$id))
      }
      df[[n]] = dplyr::tibble(article_id = unlist(article_id, use.names = FALSE),
                              snippet = unlist(snippet, use.names = FALSE))
    } else {
      for (i in seq_along(x[[n]]$content)) {
        snippet_list[[z + i]] = list()
        snippet_list[[z + i]][[1]] = x[[n]]$content[[i]]$id
        snippet_list[[z + i]][[2]] = x[[n]]$content[[i]]$snippet
        names(snippet_list[[z + i]]) = c("article_id", "snippet")
      }
    }
    z = sum(z, length(x[[n]]$content))
  }
  if (format == "tibble") {
    y = df[[1]]
    if (length(df) > 1) {
      for (i in 2:length(df)) {
        y = rbind(y, df[[i]])
      }
    }
    unique(y)
  } else {
    unique(snippet_list)
  }
}
