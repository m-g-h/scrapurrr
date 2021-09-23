#' Collect all local objects and return them as named list.
#'
#' Makes it easier to return a named list. Recommended us is inside
#' an extraction function that is passed to \code{\link{do_scrape}}.
#'
#' @return \code{named list} of all objects in the function that don't
#' start with a dot
#' @export
#'
#' @examples
#'
#' # Function that uses return_named_list()
#' func <- function(a, b) {
#'   # objects prefixed with a dot get ignored
#'   .not_include = b
#'   x = a
#'   y = NULL
#'   return_named_list()
#' }
#'
#' func(1, 2)

return_named_list <- function() {

  # Get parent environment
  parent_env = parent.frame()

  # Get parent function arguments (we don't want those)
  args = formals(sys.function(sys.parent(1))) %>%
    names()
  # Get objects and return them as named list. Filter out args
  objs = objects(name = parent_env,
                 all.names = F)

  list = objs[!(objs %in% args)] %>%
    mget(envir = parent_env)

  # Replace NULL values by NA
  list[sapply(list, is.null) %>%
    which()] = NA

  list


}
