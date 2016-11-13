#' @example
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
