
#' Prints a progress message to the console as \code{YYYY-MM-DD HH:MM:SS n/N object}
#'
#' @param n \code{numeric scalar} number of current object
#' @param N \code{numeric scalar} total number of objects
#' @param object \code{character scalar or convertable to character} the object to
#' print in the tail of the message
#'
#' @return Prints a message to the console
#' @export
#'
#' @importFrom cli console_width
#' @importFrom crayon blue white
#'
#' @examples
#'
#' messagefun(1, 1000, "object")
#'
messagefun = function(n, N, object){

  # Calculate print length for object
  len_n = nchar(as.character(n))
  len_N = nchar(as.character(N))

  print_len = console_width() - 21 - len_n - len_N

  # Format object to string andprint length
  object_formatted = as.character(object)[[1]] %>%
    strtrim(print_len)

  # Print message
  message(blue(Sys.time()), " ",
          n, "/", N, ": ",
          white(object_formatted)
  )
}


#' Executes a function and gives a try-error if it fails or times out
#'
#' @param func \code{function} Function to execute
#' @param timeout \code{numeric scalar} Seconds until function is considered to
#' be timed out
#' @param ... \code{further arguments to function}
#'
#' @return Returns either a \code{try-error} or the function result
#' @export
#'
#' @importFrom R.utils withTimeout
#' @importFrom crayon green
#'
#' @examples
#'
#' # Works
#' try_and_timeout(mean, 1:100)
#'
#' # Returns try-error because of timeout
#' try_and_timeout(Sys.sleep, 0.2, timeout = 0.1)
#'
#' # Returns error because of wrong argument
#' try_and_timeout(mean, "error")

try_and_timeout <- function(func, ...,
                            timeout = 15){
  # Setup while loop
  res <- character()
  class(res) <- "try-error"
  n_try <- 0

  while ("try-error" %in% class(res)) {
    n_try <- n_try + 1
    # Message if there are retry attempts
    if(n_try > 1 ){
      message("Retry, attempt ", n_try)
    }
    # Try attempt
    res <- try({
      withTimeout({
        func(...)
      },timeout = timeout,
      onTimeout = "warning")
    })

    # Success message
    if(!("try-error" %in% class(res)) & n_try>1){
      message(green("Success!"))
    }

    return(res)
  }# End of while loop
}
