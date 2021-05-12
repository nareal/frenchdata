#' @importFrom magrittr %>%
#' @importFrom rlang .data

# Create the package environment
frenchdata <- new.env()

#' `french_data_list` S3 class
#'
#' @description
#' The `french_data_list` exists to hold the results of reading the files lists of Kenneth's French data library.
#'
#' It provides a method to print the objects of this class.
#'
#' @section Properties of `french_data_list`:
#'
#' Objects of class `french_data_list` have:
#' * A `class` attribute of `french_data_list`.
#' * A base type of `"list"` with the following elements:
#'    * `info` - holds the information about when and were the information was retrieved.
#'    * `files_list` a tibble with with a list of files that can be downloaded, the tibble contains a `name`, `file_url` and `details_url` column.
#'
#' @section Behavior of `french_data_list`:
#'
#' * Pretty prints the object.
#'
#' @name french_data_list-class
NULL



#' Generic print method for objects of class `french_data_list`
#'
#' Prints an object of class `french_data_list`
#'
#' @param x an object of class `french_data_list`
#'
#' @param ... other arguments passed to `print()`
#'
#' @method print french_data_list
#' @export
#'
#' @examples
#'
#' \dontrun{
#'    files_list <- get_french_data_list()
#'    files_list
#' }
#' @rdname print
print.french_data_list <- function(x, ...) {
  cli::cli_h3("Kenneth's French data library")
  cli::cli_alert_info(x$info)
  cli::cli_h3("Files list")
  print(x$files_list, ...)
}

#' Browse Kenneth's French data library website
#'
#' Opens the data library website on the default browser
#' \url{https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html}
#'
#' @examples
#'
#' \dontrun{
#'    browse_french_site()
#' }
#'
#' @export
browse_french_site <- function() {
  utils::browseURL("https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html")
}

#' Get list of files available on Kenneth's French data library website
#'
#' Extract the list of files along with a description and links to them and additional information from the data library page of Prof. Kenneth French website.
#'
#' @param max_tries a number defining the maximum number of tries to perform when downloading the page.
#'
#' @param refresh logical. If TRUE re-downloads the page and overwrites the cached information. Otherwise use the cached data if a download has been done in the current session.
#'
#' @return An object of class `french_data_list` with the following elements:
#'    * `info` - holds the information about when and were the information was retrieved.
#'    * `files_list` a tibble with with a list of files that can be downloaded, the tibble contains a `name`, `file_url` and `details_url` column.
#'
#' @examples
#'
#' \dontrun{
#'    files_list <- get_french_data_list()
#'    files_list
#' }
#'
#' @export
get_french_data_list <- function(max_tries = 3,
                                 refresh = FALSE) {
  assertthat::assert_that(is.numeric(max_tries),
                          length(max_tries) == 1)
  assertthat::assert_that(assertthat::is.flag(refresh))

  if ((refresh == TRUE) || (!exists("french_data_files_list",
                                    envir = frenchdata)))
  {
    base_url <-
      "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html"
    trial <- 1
    success <- FALSE
    while (trial <= as.integer(max_tries)) {
      request <-
        httr::GET(base_url)
      if (httr::status_code(request) == 200) {
        success <- TRUE
        page <- httr::content(request, encoding = "UTF-8")
        links <- get_info(page)

        break()
      } else {
        trial <- trial + 1
        cli::cli_h3("Error reading the page")
        cli::cli_alert_danger(httr::http_status(request)$message)
        cli::cli_alert_info("Trying again in 5 seconds. Please wait...")
        Sys.sleep(5)
      }
    }

    if (success == FALSE) {
      cli::cli_h3("Unable to get the file list")
      cli::cli_alert_danger("Max trials reached!")
      cli::cli_alert(
        "Check your internet connection; please check if you can visit the site <https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html> on a browser."
      )
      cli::cli_alert(
        "Try again in a couple of minutes and if the problem persists please open a ticket on the package github site."
      )
      files_list <- NULL
    } else {
      files_list <-
        structure(list(
          info = paste(
            "Information collected from:",
            base_url,
            "on",
            format(Sys.time(), "%a %b %d %H:%M:%S %Y")
          ),
          files_list = links
        ),
        class = "french_data_list")
      assign("french_data_files_list", files_list, envir = frenchdata)
    }
  } else {
    files_list <- get("french_data_files_list", envir = frenchdata)
  }

  return(files_list)
}


get_info <- function(page) {
  links <- tibble::tibble(file_url = page %>%
                            rvest::html_elements("a") %>%
                            rvest::html_attr("href")) %>%
    dplyr::filter(stringr::str_detect(.data$file_url, "_CSV.zip")) %>%
    dplyr::mutate(
      name = purrr::map(.data$file_url,
                        find_file_description,
                        page),
      details_url = purrr::map(.data$file_url,
                               find_details,
                               page)
    ) %>%
    dplyr::select(.data$name, .data$file_url, .data$details_url) %>%
    tidyr::unnest(cols = c(.data$name, .data$details_url))

  return(links)
}



find_file_description <- function(url, page) {
  page %>%
    rvest::html_elements(xpath =
                           paste0("//a[contains(@href,'", url,
                                  "')]/preceding::b[2]")) %>%
    rvest::html_text()
}


find_details <- function(url, page) {
  page %>%
    rvest::html_elements(xpath =
                           paste0("//a[contains(@href,'", url,
                                  "')]/following::a[1]")) %>%
    rvest::html_attr("href")
}
