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


#' Makes a function slow (delay before it's called) insistent (retries
#' execution on fail) and safe (return error message instead of stopping)
#'
#' @param .function A \code{function} to be made slow, insisten and safe
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
  .function %>%
    slowly(rate = rate_delay(delay)) %>%
    insistently(rate = rate_backoff(pause_base = delay,
                                    pause_cap = delay,
                                    pause_min = delay,
                                    max_times = attempts)) %>%
    safely() %>%
    return_result_or_message()

}
