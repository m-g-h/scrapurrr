
# messagefun() ------------------------------------------------------------


test_that("messagefun() works", {
  expect_message(messagefun(1, 1000, "object"),
                 regexp = "object")
})

# try_and_timeout() -------------------------------------------------------


test_that("try_and_timeout() works",{

  expect_equal(try_and_timeout(mean, 1:100),
               50.5)

  errorfun = function(x){
    stop("error")
  }

  # Error error
  expect_message(try_and_timeout(errorfun, attempts = 3),
                 "Retry, attempt 2") %>%
    expect_message("Retry, attempt 3") %>%
    expect_message("Extraction failed. Returned error message in results")

})

# do_scrape() -------------------------------------------------------------


test_that("do_scrape() works",{

  files = list.files("do_scrape_files",
                     full.names = T)




  # Scraping using a function and a list of files
  scrapefun = function(file){
    title = rvest::read_html(file) %>%
      rvest::html_elements("h1") %>%
      rvest::html_text() %>%
      .[1]

    list("title" = title)

  }

  expect_equal(do_scrape(scrapefun, files, print_status_messages = F),
               tibble::tibble(title = c("Berlin",
                                        "Baden-Württemberg",
                                        "Bayern")))

  # Messages on
  expect_message(do_scrape(scrapefun, files),
                 regexp = "1/3") %>%
    expect_message(regexp = "2/3") %>%
    expect_message(regexp = "3/3") %>%
    expect_message(regexp = "Finished")

  # Scraping using a function and a list of files + a list of indices
  scrapefun2 = function(file,index){
    title = rvest::read_html(file) %>%
      rvest::html_elements("h1") %>%
      rvest::html_text() %>%
      .[1]

    list("title" = title,
         "index" = index)

  }

  expect_equal(do_scrape(scrapefun2, files, 1:3, print_status_messages = F),
               tibble::tibble(title = c("Berlin",
                                        "Baden-Württemberg",
                                        "Bayern"),
                              index = 1:3))

  # What if scrapefun returns an error
  scrapefun3 = function(x){

    if(x == "two"){
      stop("error")
    } else {
      res = x
    }

    list("res" = res)

  }

  expect_equal(do_scrape(scrapefun3, c(1, "two", 3), print_status_messages = F),
               tibble::tibble(res = c("1", NA, "3"),
                              error = c(NA, "Error in func(...) : error\n" , NA)))

})
