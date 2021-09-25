
# messagefun() ------------------------------------------------------------

# Should print simple status message
test_that("messagefun() works", {
  expect_message(messagefun(1, 1000, "object"),
                 regexp = "1/1000. object")
})

# try_and_timeout() -------------------------------------------------------


test_that("try_and_timeout() works",{

  meanfun = function(x){
    list("mean" = mean(x))
  }

  # This should work without error
  expect_equal(try_and_timeout(meanfun, 1:100),
               list("mean" = 50.5))

  # This should trigger 3 retry attempts and then give an error message
  errorfun = function(x){
    stop("error message")
  }

  # Error Messages
  expect_message(try_and_timeout(errorfun, attempts = 3),
                 "Retry, attempt 2") %>%
    expect_message("Retry, attempt 3") %>%
    expect_message("Extraction failed. Returned error message in results")

  # Expect that it returns the error message
  expect_equal(try_and_timeout(errorfun, attempts = 3,print_status_message = F),
               list("error" = "Error in .f(...) : error message\n"))

})

# map_scrape() -------------------------------------------------------------


test_that("map_scrape() works",{

  files = list.files("test-crawler_files",
                     full.names = T)

  # Scraping using a function and a list of files
  scrapefun = function(file){
    title = rvest::read_html(file) %>%
      rvest::html_elements("h1") %>%
      rvest::html_text() %>%
      .[1]

    list("title" = title)

  }

  expect_equal(map_scrape(files, .f = scrapefun,
                         print_status_message = F),
               tibble::tibble(title = c("Berlin",
                                        "Baden-Württemberg",
                                        "Bayern"),
                              id = 1:3))

  # Expect status messages
  expect_message(map_scrape(files, .f = scrapefun),
                 regexp = "1/3") %>%
    expect_message(regexp = "2/3") %>%
    expect_message(regexp = "3/3") %>%
    expect_message(regexp = "Finished")

  # Scraping using a function that takes two list arguments
  scrapefun2 = function(file,index){
    title = rvest::read_html(file) %>%
      rvest::html_elements("h1") %>%
      rvest::html_text() %>%
      .[1]

    list("title" = title,
         "index" = index)

  }

  expect_equal(map_scrape(files, 1:3, .f = scrapefun2,
                         print_status_message = F),
               tibble::tibble(title = c("Berlin",
                                        "Baden-Württemberg",
                                        "Bayern"),
                              index = 1:3,
                              id = 1:3))

  # What if scrapefun returns an error
  scrapefun3 = function(x){

    if(x == "two"){
      stop("error")
    } else {
      res = x
    }

    list("res" = res)

  }
  # Expect status + error messages
  expect_message(map_scrape(c(1, "two", 3), .f = scrapefun3,
                           print_status_message = T),
                 "1/3. 1") %>%
    expect_message("2/3. two") %>%
    expect_message("Retry, attempt 2") %>%
    expect_message("Retry, attempt 3") %>%
    expect_message("Extraction failed. Returned error message in results") %>%
    expect_message("3/3. 3") %>%
    expect_message("Finished")

  # Expect the error message in the output
  expect_equal(map_scrape(c(1, "two", 3), .f = scrapefun3,
                         print_status_message = F),
               tibble::tibble(res = c("1", NA, "3"),
                              id = 1:3,
                              error = c(NA, "Error in .f(...) : error\n" , NA)))


})

test_that("map_scrape() works online", {
  skip_if_offline()

  links = list("https://de.wikipedia.org/wiki/Bayern",
               "https://de.wikipedia.org/wiki/Berlin",
               "https://de.wikipedia.org/wiki/Brandenburg")

  # Web scraping function
  scrapefun = function(link){
    title = rvest::read_html(link) %>%
      rvest::html_elements("h1") %>%
      rvest::html_text()
   list("title" = title)
  }

  expect_equal(map_scrape(links, .f = scrapefun,
                         print_status_message = F),
               tibble(title = c("Bayern",
                                "Berlin",
                                "Brandenburg"),
                      id = 1:3))
})
