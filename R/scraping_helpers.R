#' Collect all local objects and return them as named list.
#'
#' Makes it easier to return a named list from a function.
#'
#' @return \code{named list} of all objects in the function that don't
#' start with a dot.
#' @export
#'
#' @examples
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

  is_empty = function(x){
    length(x) == 0 | is.null(x)
  }

  # Replace empty values by NA
  list[sapply(list, is_empty) %>%
         which()] = NA

  list


}

#' Find the positions of nodes in a \code{xml_nodeset} that match a regex pattern
#'
#' @param nodelist \code{xml_nodeset}, as e.g. returned from
#' \code{\link[rvest]{html_elements}}.
#' @param regex \code{string scalar} giving the regular expression to search for.
#' See the stringr cheatsheet on \url{https://www.rstudio.com/resources/cheatsheets/}
#' @param inc \code{numeric scalar}. Increment added to the returned index. See
#' examples for a use case.
#'
#' @return Returns a \code{numeric scalar or vector}.
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

#' Find and return the node(s) in a \code{xml_nodeset} that match a regex pattern
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

#' Download html and/or display it in the RStudio Viewer / Browser
#'
#' @param link \code{string scalar}(optional). Link to a webpage. It will be
#' downloaded via \code{\link[xml2]{read_html}}.
#' @param html \code{string scalar or html} containing the html to be displayed.
#'
#' @return Opens the RStudio viewer and displays the webpage or html.
#' @export
#'
#' @importFrom xml2 read_html
#' @importFrom rstudioapi viewer
#' @importFrom utils browseURL
#'
#' @examples
#' # Using link to webpage:
#' view_html("https://www.google.de")
#'
#' # Using downloaded html
#' google = xml2::read_html("https://www.google.de")
#' view_html(html = google)
#'
view_html = function(link = NULL, html = NULL){

  # Get html using xml2 read_html if none is provided
  if(is.null(html)){
    html = read_html(link)
  }

  html = as.character(html)

  # Create local temp file with html content
  tempDir <- tempfile()
  dir.create(tempDir)
  htmlFile <- file.path(tempDir, "index.html")
  writeLines(text = html,
             con = htmlFile)

  # Use RStudio Viewer to access file
  viewer <- getOption("viewer", default = utils::browseURL)
  viewer(htmlFile)
}

#' Invoke the "Quick Connect" command of NordVPN
#'
#' @param nordvpn_exe \code{string scalar} giving the path to the NordVPN.exe.
#'
#' @return Invokes the `-c` command of NordVPN
#' @export
#'
#' @importFrom crayon green white
#' @importFrom httr GET
#'

NordVPN_quick_connect = function(
  nordvpn_exe = "C:\\Program Files\\NordVPN\\NordVPN.exe"){

  # System command to connect to new proxy
  NordVPN_connect = function(){
    message(green("Executing NordVPN Quick Connect ..."))

    system(paste0('"', nordvpn_exe, '" -c'),
           wait = T)
  }

  # GET request to popular page
  get_testpage = function(){
    try(httr::GET("https://www.google.com"), silent = T)
  }

  # First try at connection
  NordVPN_connect()
  Sys.sleep(4)
  testpage = get_testpage()

  # While loop that reconnects until the testpage could be reached
  start = Sys.time()
  duration = difftime(Sys.time(), start,
                      units = "secs")

  while ("try-error" %in% class(testpage)) {
    Sys.sleep(4)
    duration = difftime(Sys.time(), start,
                        units = "secs")
    message("Waiting on proxy, time elapsed: ",
            white(round(duration)),
            " seconds")

    if(duration > 20){
      NordVPN_connect()
      start = Sys.time()
    }

    testpage = get_testpage()
  }
  message(green("Connected"))
}
