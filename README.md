# civicfeedR

R client package for the CivicFeed News API. `civicfeedR` functions simplify the retrieval and manipulation of news data gathered from across the web by CivicFeed. More about CivicFeed [here](https://www.civicfeed.com/). 

## Installation

Install and load the `civicfeedR` in a few simple steps:

```r

## install devtools if it's not already
install.packages("devtools")
## install civicfeedR from github
devtools::install_github("gsantoshi/civicfeedR")
## load civicfeedR
library(civicfeedR)

```
## Getting started

Before you can use the `civicfeedR` package, you must choose a [pricing scheme](https://developers.civicfeed.com/pricing) to get a CivicFeed API key. Store your API key in .Renviron as the variable CF_KEY. More on how to do this [here](https://community.rstudio.com/t/how-to-set-a-variable-in-renviron/5029).

## Package features

### Look up source IDs

If you're interested in exploring news from specific sources, use `lookup_id` to find source IDs.

```r

## look for ids of news sources matching the query "new york times"
lookup_id("new york times")

```

### Get the news

To retrieve news data, make API requests using `get_news`.

```r

## get 60 most popular nyt articles posted after 2018-12-01 and corresponding to the query "congress" 
news = get_news(
  q = "congress", 
  sources = 175, 
  from = "2018-12-01", 
  sort = "socialcount_last",
  pages = 2
  )

```
Before calling `get_news`, it might be useful to check how many articles will match your `get_news` request, particularly if you intend to set the `all` argument of `get_news` to TRUE (i.e. you want to retrieve all articles matching your request). This ensures that you don't overrun the request limit corresponding to your CivicFeed pricing scheme, as each page of results returned counts as a separate request.

```r

## check the number of nyt articles posted after 2018-12-01 and corresponding to the query "congress" 
results(
  q = "congress", 
  sources = 175, 
  from = "2018-12-01"
  )
  
```

### Manipulate retrieved data

Distil useful information from the retrieved news data.

```r

## create a tibble of the titles of the articles retrieved
titles_from(news)
## create a list of snippets from the articles retrieved
snippets_from(news, format = "list")
## create a tibble of the entities mentioned in the articles retrieved
entities_from(news)

```

Filter articles that mention specific entities using `mentioned`.

```r

## get titles, dates published and social media shares from retrieved articles mentioning nancy pelosi 
mentioned(news, entities = "nancy pelosi", shares = TRUE)

```







