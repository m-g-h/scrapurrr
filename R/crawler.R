
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
#' @param attempts \code{numeric scalar} Number of attempts to try the function
#' @param ... \code{further arguments to function}
#' @param print_status_message \code{Logical scalar} indicating whether status
#' messages should be printed
#'
#' @return Returns either a \code{try-error} or the function result
#' @export
#'
#' @importFrom R.utils withTimeout
#' @importFrom crayon green red
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
                            timeout = 15,
                            attempts = 3,
                            print_status_message = T){
  # Setup while loop
  res <- character()
  class(res) <- "try-error"
  n_try <- 0

  while ("try-error" %in% class(res) & n_try <= attempts) {
    n_try <- n_try + 1
    # Message if there are retry attempts
    if(n_try > 1 & print_status_message){
      message("Retry, attempt ", n_try)
    }
    # Try attempt
    res <- try({
      withTimeout({
        func(...)
      },timeout = timeout,
      onTimeout = "silent")
    },silent = T)

    # Return silently if successful
    if(!("try-error" %in% class(res)) & n_try == 1){
      return(res)
      # Return with success message if there were retry attempts
    } else if(!("try-error" %in% class(res)) & n_try>1){
      if(print_status_message){message(green("Success!"))}
      return(res)
      # Return error if retries failed
    } else if (("try-error" %in% class(res)) & n_try == attempts){
      if(print_status_message){message(red(paste0(" Extraction failed. Returned error message in results")))}
      return(list("error" = as.character(res)))
    }
  }# End of while loop
}

#' Scrape content of objects using a specified function. Supports timeouts and
#' error handling.
#'
#' @param .func \code{function} A function that returns a list with named
#' entries
#' @param ... arguments to the function, e.g. a list of links to scrape from
#' @inheritParams try_and_timeout
#'
#' @return Returns a \code{tibble}
#' @export
#'
#' @importFrom crayon green
#' @importFrom purrr pmap_dfr
#' @import dplyr
#'
#' @examples
#'
#' library(magrittr)
#' library(rvest)
#' library(webscraping)
#'
#' # List of pages to scrape
#' links = list("https://de.wikipedia.org/wiki/Bayern",
#'              "https://de.wikipedia.org/wiki/Berlin",
#'              "https://de.wikipedia.org/wiki/Brandenburg")
#'
#' # Web scraping function
#' scrapefun = function(link){
#'   title = read_html(link) %>%
#'     html_elements("h1") %>%
#'     html_text()
#'  list("title" = title)
#' }
#'
#' do_scrape(scrapefun,links)

do_scrape = function(.func,
                     ...,
                     timeout = 15,
                     attempts = 3,
                     print_status_message = T){

  # Function that is used inside map to loop over all objects in ...
  mapfun <- function(..., n, N){
    inner_dots = list(...)
    # Status message
    if(print_status_message){
      messagefun(n, N, inner_dots[[1]])
    }
    # Try to evaluate the function with timeout
    res <- try_and_timeout(.func, ...,
                           timeout = timeout,
                           attempts = attempts,
                           print_status_message = print_status_message)

    # Throw error if no named list is returned
    if(!is.list(res) & is.null(names(res))){
      funcname = deparse(substitute(.func))
      stop(paste0("The scraping function`",
                  funcname,
                  "`did not return a named list."))
    }

    if(n == N & print_status_message){
      message(green("Finished"))
    }
    # Return result
    res$id = n
    res
  }

  # Capture args
  dots = list(...)
  N = length(dots[[1]])
  dots$n = 1:length(dots[[1]])

  # Evaluate all objects
  pmap_dfr(.l = dots,
           mapfun,
           N = N)

}
