
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Webscraping in `R` with `scrapurrr` <img src="man/figures/hex_small.svg" align="right" />

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/m-g-h/scrapurrr/branch/master/graph/badge.svg)](https://codecov.io/gh/m-g-h/scrapurrr?branch=master)
[![R-CMD-check](https://github.com/m-g-h/scrapurrr/workflows/R-CMD-check/badge.svg)](https://github.com/m-g-h/scrapurrr/actions)
<!-- badges: end -->

### Overview

This package offers a functional programming style framework for
webscraping and useful tools and tutorials to overcome common
webscraping challenges. Check out the vignettes for webscraping
workflows and infos on how to write the webscraping logic for specific
webscraping challenges

Install the development version via

``` r
remotes::install_github("https://github.com/m-g-h/scrapurrr",
                        build_vignettes = T)
```

### Solve common webscraping tasks in a `scrapefun()`

![](vignettes/scrapefun.svg)

Write a taylor-made `scrapefun()` that solves the challenges of your
specific webscraping task:

-   Check if you need a webdriver or not using `view_html()`
-   Use `rvest` or implement a webdriver like like `phantomjs` or
    `Selenium` to download the webpage content
-   Use `rvest` for extracting information. The helpers `node_which()`
    and `html_find()` add `regex` selection
-   Use a VPN to rotate proxies (currently NordVPN via
    `NordVPN_quick_connect()`)
-   Use `V8` to execute `javascript`, e.g. to de-obfusciate emails.
-   Return output via `return_named_list()` conveniently as named list

See `vignette("scrapefuns_and_helpers")` for more info.

### Automate repetitive scraping tasks

![](vignettes/automation.svg)

Step 1: define the list of `url`s or `html` files you want to scrape

Step 2: define a flexible `scrapefun()` taylor-made to the specific
`scrapurrr` challenge at hand

Step 3: use `scrapurrr()` to webscrape the entire list.

See `vignette("workflows")` for more info.

## Example Workflow

Generate list of links to scrape. In this example we scrape the
Wikipedia pages of all German federal states:

``` r
library(scrapurrr)
library(rvest)

state_links = read_html("https://en.wikipedia.org/wiki/States_of_Germany") %>% 
  html_element(".wikitable") %>% 
  html_elements("tr> td:nth-child(3)") %>% 
  html_elements("a") %>% 
  html_attr("href") %>% 
  paste0("https://en.wikipedia.org", .)

head(state_links)
#> [1] "https://en.wikipedia.org/wiki/Baden-W%C3%BCrttemberg"
#> [2] "https://en.wikipedia.org/wiki/Bavaria"               
#> [3] "https://en.wikipedia.org/wiki/Berlin"                
#> [4] "https://en.wikipedia.org/wiki/Brandenburg"           
#> [5] "https://en.wikipedia.org/wiki/Bremen_(state)"        
#> [6] "https://en.wikipedia.org/wiki/Hamburg"
```

Before defining the `scrapefun()` I assess whether I need a webdriver or
not. With `view_html()` I can see what my computer receives with
`read_html()` from `rvest`/`xml2` in the viewer:
![](vignettes/viewer_screenshot.PNG)

Looks like we have a static html page and the capital is easily
accessible. If the content would be loaded dynamically or if it was
obfuscated I would have needed a webdriver like `phantomjs` or
`Selenium`.

Now we define a scraping function that extracts the state capital from
each page. I use `rvest` and `tidyverse` to power the function.

``` r
library(tidyverse)

get_state_capital = function(link){
  # Download html
  .page = read_html(link)
  
  # Extract name of country
  State = .page %>% 
    html_element("h1") %>% 
    html_text()
  
  # Extract capital city
  Capital = .page %>% 
    html_element("table.infobox") %>% 
    html_table(header = F) %>% 
    filter(str_detect(X1, "Capital")) %>% 
    pull("X2")
  
  # Return results as named list. Objects prefixed with a dot get ignored
  return_named_list()
}
```

And then we can use `scrapurrr()` and `purrr::map_dfr` to execute the
task:

``` r
results = scrapurrr(state_links,
                    scrapefun = get_state_capital,
                    map_fun = purrr::map_dfr)

head(results)
#> # A tibble: 6 x 2
#>   Capital                              State            
#>   <chr>                                <chr>            
#> 1 Stuttgart                            Baden-Württemberg
#> 2 Munich                               Bavaria          
#> 3 Capital city, state and municipality Berlin           
#> 4 Potsdam                              Brandenburg      
#> 5 Bremen                               Bremen (state)   
#> 6 <NA>                                 Hamburg
```

# Further Information and Advanced `scrapefun()` features

See the vignettes for further information:

-   `vignette("workflows")` for tutorials on automating common
    webscraping tasks
-   `vignette("scrapefuns_and_helpers")` for features to implement in
    your `scrapefun()`, e.g. javascript execution
