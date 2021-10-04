test_that("return_named_list() works", {

  # Function that uses return_named_list()
  func <- function(a, b=1) {
    # objects prefixed with a dot get ignored
    .not_include = b
    x = a
    y = NULL
    return_named_list()
  }

  expect_equal(func(1, 2),
               list("x" = 1,
                    "y" = NA))

  # Should also work within map_scrape()
  expect_equal(map_scrape_dfr(x = 1:3, scrapefun = func),
               tibble(x = 1:3,
                      y = NA))

})

test_that("node_which() works",{

  # Lets suppose we want to know the owner of "Alfreds Futterkiste":
  html = "<table>
  <tr>
    <th>Company</th>
    <th>Contact</th>
    <th>Country</th>
  </tr>
  <tr>
    <td>Alfreds Futterkiste</td>
    <td>Maria Anders</td>
    <td>Germany</td>
  </tr>
  <tr>
    <td>Centro comercial Moctezuma</td>
    <td>Francisco Chang</td>
    <td>Mexico</td>
  </tr>
</table>" %>%
    xml2::read_html()

  # Searching for `td` elements returns a list:
  rvest::html_elements(x = html, "td")
  # Of course we could match by position, but it may not be fixed if we have
  # many tables. Let's use `node_which()`. since the "owner" is always two rows
  # behind the "company" we increment by 2:
  rvest::html_elements(x = html, "td") %>%
    node_which("Alfreds Futterkiste", inc = 2)

  # Test that position equals 3
  expect_equal(rvest::html_elements(x = html, "td") %>%
                 node_which("Alfreds Futterkiste", inc = 2),
               3)
}
)

test_that("html_find() works",{
  # Lets suppose we want to know the owner of "Alfreds Futterkiste":
  html = "<table>
  <tr>
    <th>Company</th>
    <th>Contact</th>
    <th>Country</th>
  </tr>
  <tr>
    <td>Alfreds Futterkiste</td>
    <td>Maria Anders</td>
    <td>Germany</td>
  </tr>
  <tr>
    <td>Centro comercial Moctezuma</td>
    <td>Francisco Chang</td>
    <td>Mexico</td>
  </tr>
</table>" %>%
    xml2::read_html()

  # Searching for `td` elements returns a list:
  rvest::html_elements(x = html, "td")
  # Of course we could match by position, but it may not be fixed if we have
  # many tables. Let's use `node_which()`. since the "owner" is always two rows
  # behind the "company" we increment by 1:
  rvest::html_elements(x = html, "td") %>%
    html_find("Alfreds Futterkiste", inc = 1)

  result = rvest::html_elements(x = html, "td") %>%
    .[2]

  # Test that position equals 3
  expect_equal(rvest::html_elements(x = html, "td") %>%
                 html_find("Alfreds Futterkiste", inc = 1),
               result
  )
}
)
