#' Collect all local objects and return them as named list.
#'
#' Makes it easier to return a named list. Recommended us is inside
#' an extraction function that is passed to \code{\link{do_scrape}}.
#'
#' @return \code{named list} of all objects in the function that don't
#' start with a dot
#' @export
#'
#' @examples
#'
#' # Function that uses return_named_list()
#' func <- function(a, b) {
#'   # objects prefixed with a dot get ignored
#'   .not_include = b
#'   x = a
#'   y = NULL
#'   return_named_list()
#' }
#'
#' func(1, 2)

return_named_list <- function() {

  # Get parent environment
  parent_env = parent.frame()

  # Get parent function arguments (we don't want those)
  args = formals(sys.function(sys.parent(1))) %>%
    names()
  # Get objects and return them as named list. Filter out args
  objs = objects(name = parent_env,
                 all.names = F)

  list = objs[!(objs %in% args)] %>%
    mget(envir = parent_env)

  # Replace NULL values by NA
  list[sapply(list, is.null) %>%
         which()] = NA

  list


}

#' Find the positions of nodes in a nodelist that matche a regex pattern
#'
#' @param nodelist \code{xml_nodeset}, e.g. returned from \code{rvest}s
#' \code{link{html_elements}}
#' @param regex \code{string scalar} giving the regex to search for. See the
#' stringr cheatsheet on \url{https://www.rstudio.com/resources/cheatsheets/}
#' @param inc \code{numeric scalar} Increment added to the returned index. See
#' examples for a usecase.
#'
#' @return Returns a \code{numeric scalar or vector}
#' @export
#'
#' @importFrom stringr str_which
#' @importFrom rvest html_text
#'
#' @examples
#' library(rvest)
#' library(webscraping)
#'
#' # Lets suppose we want to know the owner of "Alfreds Futterkiste":
#' html = "<table>
#'   <tr>
#'     <th>Company</th>
#'     <th>Contact</th>
#'     <th>Country</th>
#'   </tr>
#'   <tr>
#'     <td>Alfreds Futterkiste</td>
#'     <td>Maria Anders</td>
#'     <td>Germany</td>
#'   </tr>
#'   <tr>
#'     <td>Centro comercial Moctezuma</td>
#'     <td>Francisco Chang</td>
#'     <td>Mexico</td>
#'   </tr>
#' </table>" %>%
#'   read_html()
#'
#' # Searching for `td` elements returns a list:
#' html_elements(x = html, "td")
#' # Of course we could match by position, but it may not be fixed if we have
#' # many tables. Let's use `node_which()`. since the "owner" is always two rows
#' # behind the "company" we increment by 2:
#' html_elements(x = html, "td") %>%
#'   node_which("Alfreds Futterkiste", inc = 2)

node_which <- function(nodelist, regex, inc = 0) {
  str_which(html_text(nodelist), regex) + inc
}

#' Find and return the node(s) in a nodelist that matche a regex pattern
#'
#' @inheritParams node_which
#'
#' @return Returns a \code{xml_nodeset}
#' @export
#'
#' @examples
#' library(rvest)
#' library(webscraping)
#'
#' # Lets suppose we want to know the owner of "Alfreds Futterkiste":
#' html = "<table>
#'   <tr>
#'     <th>Company</th>
#'     <th>Contact</th>
#'     <th>Country</th>
#'   </tr>
#'   <tr>
#'     <td>Alfreds Futterkiste</td>
#'     <td>Maria Anders</td>
#'     <td>Germany</td>
#'   </tr>
#'   <tr>
#'     <td>Centro comercial Moctezuma</td>
#'     <td>Francisco Chang</td>
#'     <td>Mexico</td>
#'   </tr>
#' </table>" %>%
#'   read_html()
#'
#' # Searching for `td` elements returns a list:
#' html_elements(x = html, "td")
#' # Of course we could match by position, but it may not be fixed if we have
#' # many tables. Let's use `node_which()`. since the "owner" is always two rows
#' # behind the "company" we increment by 1:
#' html_elements(x = html, "td") %>%
#'   html_find("Alfreds Futterkiste", inc = 1)

html_find <- function(nodelist, regex, inc = 0) {
  nodelist[node_which(nodelist, regex, inc)]
}
