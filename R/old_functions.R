#' Get AHREF rank
#'
#' Depreciated: will be removed soon
#'
#' @param url URL we want the rank for
#' @param api_key API key (also called token)
#'
#' @return A single number
#' @export
#'
#' @examples\dontrun{
#' get_rank('google.com', ahrefs_key)
#' }
get_rank <- function(url, api_key){
  parameters <- list(
    token  = api_key,
    target = url,
    mode   = 'exact',
    output = 'json',
    from   = 'ahrefs_rank')

  response <- httr::GET('http://apiv2.ahrefs.com', query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  return(parsed$pages[[1]]$ahrefs_rank)
}


#' Get Domain Rating
#'
#' Depreciated: will be removed soon
#'
#' @param url URL we want the rank for
#' @param api_key API key (also called token)
#'
#' @return A single number
#' @export
#'
#' @examples\dontrun{
#' get_domain_rating('google.com', ahrefs_key)
#' }
get_domain_rating <- function(url, api_key){

  parameters <- list(
    token  = api_key,
    target = url,
    mode   = 'domain',
    output = 'json',
    from   = 'domain_rating')

  response <- httr::GET('http://apiv2.ahrefs.com', query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  return(parsed$domain$domain_rating)
}

#' Get Refering Domains
#'
#' Depreciated: will be removed soon
#'
#' @param url URL we want the domains for
#' @param api_key API key (also called token)
#'
#' @return A list: first element has info on the refering domains, second on overall statistics
#' @export
#'
#' @examples\dontrun{
#' get_refering_domains('google.com', ahrefs_key)
#'}
get_refering_domains <- function(url, api_key, limit = 10){

  parameters <- list(
    token  = api_key,
    target = url,
    mode   = 'domain',
    output = 'json',
    from   = 'refdomains',
    limit  = limit)

  response <- httr::GET('http://apiv2.ahrefs.com', query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  return(parsed)
}

library(httr)

#' Get URL metrics
#'
#' Depreciated: will be removed soon
#'
#' @param url URL we metrics for
#' @param api_key API key (also called token)
#'
#' @return A list
#' @export
#'
#' @examples\dontrun{
#' get_metrics('google.com', ahrefs_key)
#'}
get_metrics <- function(url, api_key){

  parameters <- list(
    token =  api_key,
    target = url,
    limit = 1000,
    output = 'json',
    from = 'metrics_extended',
    mode = 'prefix')

  get_url = 'http://apiv2.ahrefs.com/'

  response <- httr::GET(get_url, query = parameters)

  if (httr::http_type(response) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  httr::stop_for_status(response)

  content <- httr::content(response, type = 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  return(parsed)
}
