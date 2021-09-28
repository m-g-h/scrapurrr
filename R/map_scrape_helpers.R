#' Add sleeping time to a function.
#'
#' This is a functional operator. It adds a call to \code{\link{Sys.sleep}
#' before calling the original function.
#'
#' @param .func A \code{function} which is wrapped with the
#' @param sleep_time \code{numeric scalar} giving the sleeping time in seconds
#'
#' @return Returns a \code{function}
#' @export
#'
#' @examples
#'
add_sleep_time = function(.func, sleep_time = 0){
  # Make sure `.func` and `sleep_time` actually get evaluated
  force(sleep_time)
  force(.func)

  # Return function that now includes a waiting time
  function(...){
    Sys.sleep(sleep_time)
    .func(...)
  }
}
