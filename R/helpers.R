#' @export
print.ahrefs_api <- function(x, ...){
  cat('Status: ', x$response$status_code, '\n', sep = '')
  cat('Date: ',   format(x$response$date, "%Y-%m-%d %H:%M"), '\n', sep = '')
  cat('Target: ', x$parameters$target, '\n', sep = '')
  cat('From: ', x$parameters$from, '\n', sep = '')
  cat('Mode: ', x$parameters$mode, '\n', sep = '')

  contents <- try(Map(length, x$content))
  if (!inherits(contents, 'try-error')){
    cat('Content Lengths - \n')
    print(contents)
  }
}


#' Make a condition.
#'
#' Make a contition to use as 'having' or 'where' paramater.
#'
#' @param col List of columns to operate on
#' @param operator List of operators to use. Possible operators are: '=', '<>', '<', '<=', '>', '>=', 'subdomain', 'word', 'substring'.
#' @param value List of values for operator.
#'
#' @return A single element character vector.
#' @export
#'
#' @examples
#' make_condition('title', 'word', 'the')
#' make_condition(c('date', 'date'), c('=', '='), c('2016-10-01', '2016-10-02'))
#'
make_condition <- function(col, operator, value){

  # For a single value
  to_condition <- function(col, operator, value){

    valid_operators <- c('=', '<>', '<', '<=', '>', '>=', 'subdomain', 'word', 'substring')

    if(!(operator %in% valid_operators)) stop('Invalid Operator: ', operator,'. Possible operators: ', valid_operators)

    if (inherits(value, 'Date') | is.character(value)) value <- paste0('"', value, '"')

    if (operator %in% c('subdomain', 'word', 'substring')){
      return(paste0(operator, '(', col, ',', value, ')'))
    }

    return(paste0(col, operator, value))
  }

  conditions <- Map(to_condition, col, operator, value)
  conditions <- paste(unlist(conditions), collapse = ',')

  return(conditions)
}

#' Make an order.
#'
#' Make an order, to use in an order parameter.
#'
#' @param col Columns for ordering
#' @param direction Character vector of either 'asc' for accending, or 'desc' for decending.
#'
#' @return A single element character vector.
#' @export
#'
#' @examples
#'
#' make_order('date')
#' make_order(c('year', 'month'), c('desc', 'desc'))
make_order <- function(col, direction = 'asc'){

  # For a single value
  to_order <- function(col, direction){
    if (!(direction %in% c('desc', 'asc'))) stop ('Invalid direction: ', direction, '. Possible values are: desc, asc')
    paste0(col, ':', direction)
  }

  orders <- Map(to_order, col, direction)

  orders <- paste(unlist(orders), collapse = ',')

  return(orders)
}


