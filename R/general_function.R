#' Get Data From the AHREF's
#'
#'Get data from the AHREF's API.
#'For more information see the online documentation: https://ahrefs.com/api/documentation.
#'
#' @param target Aim of a request: a domain, a directory or a URL
#' @param token Authentication token
#' @param from Table to select data from
#' @param mode Mode of operation: exact, domain, subdomains or prefix
#' @param select A character vector of columns to select in 'from' table.
#' @param where "Where" condition to satisfy (see documentation for how to build where conditions)
#' @param having "Having" condition to satisfy (see documentation for how to build a having conditions)
#' @param order_by List of columns to sort on (see documentation for how to build order_by conitions)
#' @param offset Position from which to start returning data
#' @param limit Number of results to return
#'
#' @return An object of class 'ahrefs_api'
#' @export
#'
#' @examples \dontrun{
#' ahrefs_key <- '123456789'
#' get_ahrefs_data('mhairihmcneill.com', ahrefs_key, from = 'ahrefs_rank', mode = 'domain')
#' get_ahrefs_data('mhairihmcneill.com/blog', ahrefs_key, from = 'metrics_extended', mode = 'exact')
#' get_ahrefs_data('mhairihmcneill.com/blog', ahrefs_key, from = 'refdomains', mode = 'prefix', limit = 10)
#' }
get_ahrefs_data <- function(target,
                            token,
                            from,
                            mode,
                            select   = NULL,
                            where    = NULL,
                            having   = NULL,
                            order_by = NULL,
                            offset   = 0,
                            limit    = 1000){

  mode_values <- c('exact', 'domain', 'subdomains', 'prefix')

  # Check parameters
  if (!(from %in% from_values)) stop("Invalid table chosen for 'from' parameter.")
  if (!(mode %in% mode_values)) stop('Invalid mode.')
  if (offset %% 1 != 0) stop('Offset must be an integer')
  if (limit  %% 1 != 0) stop('Limit must be an integer')

  # Change select to format wanted by API
  if (!is.null(select)) select <- paste0(select, ',')

  parameters <- list(
    target    = target,
    token     = token,
    from      = from,
    mode      = mode,
    select    = select,
    where     = where,
    having    = having,
    order_by  = order_by,
    offset    = offset,
    limit     = limit,
    output    = 'json'
  )

  # Remove null parameters
  parameters <- Filter(function(x) !is.null(x), parameters)

  # Set user agent
  ua <- user_agent("http://github.com/mhairi/ahrefs")

  # Make Request
  url <- httr::modify_url('http://apiv2.ahrefs.com', query = parameters)

  response <- httr::GET(url, ua)

  # Check for errors
  httr::stop_for_status(response)

  if (httr::http_type(response) != 'application/json') {
    stop('API did not return json', call. = FALSE)
  }

  # Parse content
  content <- httr::content(response, 'text', encoding = 'utf-8')
  parsed  <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  # More error checking
  if (!is.null(parsed$error)) stop('API returned error: ', content$error)

  return(structure(
    list(
      content = parsed,
      response = response,
      parameters = parameters
    ),
    class = 'ahrefs_api'
  ))

}
