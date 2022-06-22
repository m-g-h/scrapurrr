#' Wraps around a function which is returned from \code{\link[purrr]{safely}}.
#' Makes it return the result or the error message
#'
#' @param .function A \code{function} to be wrapped
#'
#' @return Returns a \code{function}.
#' @export

return_result_or_message <- function(.function) {

  force(.function)

  function(...){
    res = .function(...)
    if(is.null(res$result)){
      list("error" = c("message" = res$error$message))
    } else {
      res$result
    }
  }
}

#' Add progress tick \code{pb$tick()} to a function
#'
#' @param .function .function A \code{function} to which the progress should be
#' added
#' @param pb A progress bar as created by \code{\link[progress]{progress_bar}}
#'
#' @return Returns a \code{function}.
#' @export

with_progress = function(.function, pb){
  force(.function)

  function(...){
    pb$tick()
    .function(...)
  }
}


#' Makes a function slow (delay before it's called) insistent (retries
#' execution on fail) and safe (return error message instead of stopping)
#'
#' @param .function A \code{function} to be made slow, insistent and safe
#' @param delay \code{numeric scalar} giving the number of seconds to wait
#' before the function is called
#' @param attempts \code{numeric scalar} giving the number of retry attempts
#' if the function fails
#'
#' @importFrom purrr slowly rate_delay insistently rate_backoff safely
#'
#' @return Returns a \code{function}
#' @export

make_function_safe_slow_insistent <- function(.function,
                                              delay = 0,
                                              attempts = 3) {

  # Return function
  .function %>%
    ifelse(delay > 0,
           slowly(., rate = rate_delay(delay)),
           .) %>%
    ifelse(attempts > 0,
           insistently(., rate = rate_backoff(pause_base = delay,
                                           pause_cap = delay,
                                           pause_min = delay,
                                           max_times = attempts)),
           .) %>%
    safely() %>%
    return_result_or_message()

}

scfun = function(a){
  print(a)
  as.numeric(a)
}

#' Scrape a list of links or html files using a scrapefun
#'
#' @param ... arguments passed to the \code{scrapefun}.
#' @param scrapefun A \code{function} that defines the scraping logic. See
#' \code{vignette(scrapefuns_and_helpers)} for examples.
#' @param map_fun A map_... \code{function} from \code{purrr} ord \code{furrr}.
#' @param display_progress_bar \code{logical scalar} indicating whether to display
#' a progress bar
#' @inheritParams make_function_safe_slow_insistent
#'
#' @return Returns a \code{list}.
#' @export
#'
#' @import progress

scrapurrr = function(..., scrapefun, map_fun,
                     delay = 0, attempts = 3,
                     display_progress_bar = F){

  scrapefun = scrapefun %>%
    make_function_safe_slow_insistent(delay, attempts)

  if(display_progress_bar){

    args = list(...)

    pb = progress_bar$new(total = length(args[[1]]),
                          format = " Scraping :current/:total [:bar] :percent eta: :eta",
                          force = T,
                          stream = stdout())

    scrapefun = with_progress(scrapefun, pb)

  }

  map_fun(..., .f = scrapefun)

}
