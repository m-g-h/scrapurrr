# map_scrape + future + dfr -----------------------------------------------


#' Scrape a list of links or html files using a scrapefun
#'
#' @describeIn map_scrape standard version implemented via \code{\link[purrr]{map}}
#'
#' @param scrapefun A \code{function} that defines the scraping logic. See
#' \code{vignette(scrapefuns_and_helpers)} for examples.
#' @param x A \code{list} or \code{atomic vector}.
#'
#' @return Returns a \code{list}.
#' @export
#'
#' @importFrom purrr map
#'

map_scrape = function(x, scrapefun){

  scrapefun %>%
    make_function_safe_slow_insistent(delay = 0,
                                      attempts = 3) %>%
    map(.x = x, .f = .)

}

#' @describeIn map_scrape parallelised version using \code{\link[furrr]{future_map}}
#' @export
#'
#' @importFrom furrr future_map furrr_options

future_map_scrape = function(x, scrapefun){

  scrapefun %>%
    make_function_safe_slow_insistent(delay = 0,
                                      attempts = 3) %>%
    future_map(.x = x, .f = .,
               .options = furrr_options(seed = 1337))

}

#' @describeIn map_scrape merges results rowwise to a tibble similar to
#' \code{\link[purrr]{map_dfr}}
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom dplyr bind_rows
#'
map_scrape_dfr = function(x, scrapefun){
  map_scrape(x, scrapefun) %>%
    reduce(bind_rows)
}

#' @describeIn map_scrape parallelised version using \code{\link[furrr]{future_map_dfr}}
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom dplyr bind_rows

future_map_scrape_dfr = function(x, scrapefun){
  future_map_scrape(x, scrapefun) %>%
    reduce(bind_rows)
}

# map2_scrape + future + dfr -----------------------------------------------


#' Scrape a list of links or html files using a scrapefun using multiple inputs.
#'
#' @describeIn map2_scrape standard version implemented via \code{\link[purrr]{map2}}
#'
#' @param scrapefun A \code{function} that defines the scraping logic. See
#' \code{vignette(scrapefuns_and_helpers)} for examples.
#' @param x A \code{list} or \code{atomic vector}.
#' @param y A \code{list} or \code{atomic vector}.
#'
#' @importFrom purrr map2

map2_scrape = function(x, y, scrapefun){

  scrapefun %>%
    make_function_safe_slow_insistent(delay = 0,
                                      attempts = 3) %>%
    map2(.x = x, .y = y, .f = .)

}

#' @describeIn map2_scrape parallelised version using \code{\link[furrr]{future_map2}}
#' @export
#'
#' @importFrom furrr future_map2 furrr_options

future_map2_scrape = function(x, y, scrapefun){

  scrapefun %>%
    make_function_safe_slow_insistent(delay = 0,
                                      attempts = 3) %>%
    future_map2(.x = x, .y = y, .f = .,
               .options = furrr_options(seed = 1337))

}

#' @describeIn map2_scrape merges results rowwise to a tibble similar to
#' \code{\link[purrr]{map2_dfr}}
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom dplyr bind_rows

map2_scrape_dfr = function(x, y, scrapefun){
  map2_scrape(x, y, scrapefun) %>%
    reduce(bind_rows)
}

#' @describeIn map2_scrape parallelised version using \code{\link[furrr]{future_map_dfr}}
#' @export
#'
#' @importFrom purrr reduce
#' @importFrom dplyr bind_rows

future_map2_scrape_dfr = function(x, y, scrapefun){
  future_map2_scrape(x, y, scrapefun) %>%
    reduce(bind_rows)
}
