
# return_result_or_message() ----------------------------------------------

test_that("return_result_or_message() returns function",{

  result = mean %>%
    return_result_or_message()

  expect_true(is.function(result))

})

test_that("return_result_or_message() function returns result or message",{

  returnfun = function(x){
    if(x == 2){
      ret = NULL
      error = list("message" = "error")
    } else {
      ret = x
      error = NULL
    }

    return(list("result" = ret,
                "error" = error))
  }

  testfun = returnfun %>%
    return_result_or_message()

  expect_equal(testfun(1),
               1)

  expect_equal(testfun(2),
               list("error" = c("message" = "error")))

})


# map_scrape() ------------------------------------------------------------

test_that("map_scrape() returns result and error list",{

  return_x = function(x){
    x
  }

  result = map_scrape(1:3, return_x)

  expect_equal(object = result,
               expected = as.list(1:3)
  )

})

test_that("map_scrape() returns error message as string",{

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      x
    }
  }

  result = map_scrape(1:3, error_on_2)

  expect_equal(result[[2]],
               list("error" = c("message" = "Request failed after 3 attempts"))
  )
})

test_that("map_scrape() output can be reduced to tibble",{


    library(purrr, warn.conflicts = F)
    library(tibble)

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      list("res" = x)
    }
  }

  result = map_scrape(1:3, error_on_2) %>%
    reduce(.f = bind_rows)

  expected = tibble(res = c(1, NA, 3),
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})


# future_map_scrape() -----------------------------------------------------

test_that("future_map_scrape() returns result and error list",{

  library(furrr, quietly = T)

  if("Windows" %in% Sys.info()){
    strat = "multisession"
  } else {
    strat = "multiprocess"
  }

  plan(strategy = strat, workers = 2)

  return_x = function(x){
    x
  }

  result = future_map_scrape(1:3, return_x)

  expect_equal(object = result,
               expected = as.list(1:3)
  )

})





# map_scrape_dfr() --------------------------------------------------------

test_that("map_scrape_dfr() returns a tibble",{

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      list("res" = x)
    }
  }

  result = map_scrape_dfr(1:3, error_on_2)

  expected = tibble(res = c(1, NA, 3),
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})

# future_map_scrape_dfr() --------------------------------------------------------

test_that("future_map_scrape_dfr() returns a tibble",{

  library(furrr, quietly = T)

  if("Windows" %in% Sys.info()){
    strat = "multisession"
  } else {
    strat = "multiprocess"
  }

  plan(strategy = strat, workers = 2)

  error_on_2 = function(x){
    if(x == 2){
      stop()
    } else {
      list("res" = x)
    }
  }

  result = future_map_scrape_dfr(1:3, error_on_2)

  expected = tibble(res = c(1, NA, 3),
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})


# map2_scrape() ------------------------------------------------------------

test_that("map_scrape() returns result and error list",{

  return_xy = function(x, y){
    list("x" = x, "y" = y)
  }

  result = map2_scrape(1:3, 1:3, return_xy)

  expect_equal(object = result,
               expected = list(list("x" = 1,
                                    "y" = 1),
                               list("x" = 2,
                                    "y" = 2),
                               list("x" = 3,
                                    "y" = 3))
  )

})

test_that("map_scrape() returns error message as string",{

  error_on_2_xy = function(x, y){
    if(x == 2){
      stop()
    } else {
      list("x" = x, "y" = y)
    }
  }

  result = map2_scrape(1:3, 1:3, error_on_2_xy)

  expect_equal(result[[2]],
               list("error" = c("message" = "Request failed after 3 attempts"))
  )
})

test_that("map_scrape() output can be reduced to tibble",{


  library(purrr, warn.conflicts = F)
  library(tibble)

  error_on_2_xy = function(x, y){
    if(x == 2){
      stop()
    } else {
      list("res" = x,
           "y" = y)
    }
  }

  result = map2_scrape(1:3, 1:3, error_on_2_xy) %>%
    reduce(.f = bind_rows)

  expected = tibble(res = c(1, NA, 3),
                    y = res,
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})


# future_map2_scrape() -----------------------------------------------------

test_that("future_map_scrape() returns result and error list",{

  library(furrr, quietly = T)

  if("Windows" %in% Sys.info()){
    strat = "multisession"
  } else {
    strat = "multiprocess"
  }

  plan(strategy = strat, workers = 2)

  return_xy = function(x, y){
    list(x, y)
  }

  result = future_map2_scrape(1:3, 1:3, return_xy)

  expect_equal(object = result,
               expected = list(list(1,
                                    1),
                               list(2,
                                    2),
                               list(3,
                                    3))
  )

})

# map2_scrape_dfr() --------------------------------------------------------

test_that("map_scrape_dfr() returns a tibble",{

  error_on_2_xy = function(x, y){
    if(x == 2){
      stop()
    } else {
      list("res" = x,
           "y" = y)
    }
  }

  result = map2_scrape_dfr(1:3, 1:3, error_on_2_xy)

  expected = tibble(res = c(1, NA, 3),
                    y = res,
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})

# future_map2_scrape_dfr() --------------------------------------------------------

test_that("future_map_scrape_dfr() returns a tibble",{

  library(furrr, quietly = T)

  if("Windows" %in% Sys.info()){
    strat = "multisession"
  } else {
    strat = "multiprocess"
  }

  plan(strategy = strat, workers = 2)

  error_on_2_xy = function(x, y){
    if(x == 2){
      stop()
    } else {
      list("res" = x,
           "y" = y)
    }
  }

  result = future_map2_scrape_dfr(1:3, 1:3, error_on_2_xy)

  expected = tibble(res = c(1, NA, 3),
                    y = res,
                    error = c(NA, "message" = "Request failed after 3 attempts", NA))

  expect_equal(result,
               expected)

})
