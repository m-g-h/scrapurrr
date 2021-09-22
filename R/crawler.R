
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

