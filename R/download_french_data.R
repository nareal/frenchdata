#' `french_dataset` S3 class
#'
#' @description
#' The `french_dataset` exists to hold the results of reading the files lists of Kenneth's French data library.
#'
#' It provides a method to print the objects of this class.
#'
#' @section Properties of `french_dataset`:
#'
#' Objects of class `french_dataset` have:
#' * A `class` attribute of `french_dataset`.
#' * A base type of `"list"` with the following elements:
#'    * `info` - holds the information about when and were the information was retrieved.
#'    * `details_url` - url for the webpage with details on data set.
#'    * `subsets` a tibble with with the subsets contained in the downloaded file, the tibble contains a `name` and `data` column.
#'
#' @section Behavior of `french_dataset`:
#'
#' * Pretty prints the object.
#'
#' @name french_dataset-class
NULL


#' Generic print method for objects of class `french_dataset`
#'
#' Prints an object of class [`french_dataset`][french_dataset-class]
#'
#' @param x an object of class [`french_dataset`][french_dataset-class]
#'
#' @param ... other arguments passed to `print()`
#'
#' @method print french_dataset
#'
#' @return Return a `tbl_df`/`data.frame` class object from the `subsets` element of `x`.
#'
#' @export
#' @examples
#' \donttest{
#' ff_3f <- download_french_data('Fama/French 3 Factors')
#' print(ff_3f)
#' ff_3f
#' }
print.french_dataset <- function(x, ...) {
  cli::cli_h3("Kenneth's French data set")
  cli::cli_alert_info(x$info)
  cli::cli_text("")
  cli::cli_alert_info(paste("For details on the data set call the function `browse_details_page()` on this object"))
  cli::cli_h3("Subsets in the file:")
  print(x$subsets, ...)
}



#' Browse the details webpage of a Kenneth's French data set
#'
#' Opens the details webpage of a data set on the default browser.
#'
#' @param fds an object of class [`french_dataset`][french_dataset-class]
#'
#' @return Does not return a value. Opens the details webpage of a data set on the default browser window.
#'
#' @examples
#' \donttest{
#' if(interactive()){
#'   ff_3f <- download_french_data('Fama/French 3 Factors')
#'   browse_details_page(ff_3f)
#' }
#' }
#'
#' @export
browse_details_page <- function(fds) {

  assertthat::assert_that(inherits(fds, "french_dataset"))
  utils::browseURL(fds$details_url)

}



read_info <- function(skip, csv_file){
  info <- readr::read_lines(I(csv_file), n_max = 1, skip = skip,
                            progress = FALSE)
  if (length(info) == 0) {
    info <-  ""
  } else {
    info <- stringr::str_trim(info, side = "both")
  }

  return(info)
}

read_data <- function(skip, n_max, csv_file){
  csv_data <- suppressMessages(suppressWarnings(readr::read_csv(
    I(csv_file),
    skip = skip,
    n_max = n_max,
    guess_max = n_max,
    col_names = TRUE,
    show_col_types = FALSE,
    progress = FALSE
  )))

  csv_data <- csv_data %>%
    rlang::set_names(c("date", names(csv_data)[-1]))

  return(csv_data)
}



#' Download the data set
#'
#' @param dataset_name string with the data set name. Use `get_french_data_list()` to get the list of data sets available to download.
#'
#' @param dir character. Should be a valid directory path where to save the compressed downloaded file.
#'
#' @param dest_file character. Should be a valid file name to save the compressed downloaded file. If `dir` is defined and `dest_file` is left empty, the original file name will be used.
#'
#' @param overwrite boolean. Overwrite an existing file?
#'
#' @param max_tries numeric. Number of file download trials.
#'
#' @return An objects of class [`french_dataset`][french_dataset-class] with the following elements:
#'    * `info` - holds the information about when and were the information was retrieved.
#'    * `details_url` - url for the webpage with details on data set.
#'    * `subsets` a tibble with with the subsets contained in the downloaded file, the tibble contains a `name` and `data` column.
#'
#' @export
#'
#' @examples
#' \donttest{
#' ff_3f <- download_french_data('Fama/French 3 Factors')
#' ff_3f
#' }
download_french_data <- function(dataset_name,
                                 dir = NULL,
                                 dest_file = NULL,
                                 overwrite = FALSE,
                                 max_tries = 3){

  assertthat::assert_that(assertthat::not_empty(dataset_name))
  assertthat::assert_that(is.character(dataset_name),
                          length(dataset_name) == 1)

  if (!is.null(dir)) {
    assertthat::assert_that(is.character(dir),
                            length(dir) == 1)
    assertthat::assert_that(assertthat::is.dir(fs::path_dir(dir)))
    assertthat::assert_that(assertthat::is.writeable(fs::path_dir(dir)))
    if (!is.null(dest_file)) {
      assertthat::assert_that(is.character(dest_file),
                              length(dest_file) == 1)
      if (fs::file_exists(file.path(dir, dest_file))) {
        assertthat::assert_that(overwrite == TRUE,
                                msg = "File exists and overwrite is set to FALSE!")
      }
    }
  }

  assertthat::assert_that(assertthat::is.flag(overwrite))
  assertthat::assert_that(is.numeric(max_tries),
                          length(max_tries) == 1)

  base_url <-
    "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/"

  if (exists("french_data_files_list",
             envir = frenchdata)) {
    files_list <- get("french_data_files_list", envir = frenchdata)
  } else {
    files_list <- get_french_data_list()
  }

  url_info <- files_list$files_list %>%
    dplyr::filter(.data$name == dataset_name)

  assertthat::assert_that(NROW(url_info) != 0,
              msg = "There no matches to data set name, please verify the contents of the parameter `dataset_name`")
  assertthat::assert_that(NROW(url_info) == 1,
                          msg = "There are more than one match to the  data set name, please verify the contents of the parameter `dataset_name`")

  if (!is.null(dir) &&
      (is.null(dest_file) || !assertthat::not_empty(dest_file))) {
    dest_file <- fs::path_file(url_info$file_url[1])
    if (fs::file_exists(file.path(dir, dest_file))) {
        assertthat::assert_that(overwrite == TRUE,
                                msg = "File exists and overwrite is set to FALSE!")
    }
  }

  link_to_file <- paste0(base_url, url_info$file_url[1])

  temp_file_name <- fs::file_temp(
    pattern =
      fs::path_file(url_info$file_url[1]) %>%
        fs::path_ext_remove(),
    tmp_dir = tempdir(),
    ext = "zip"
  )

  trial <- 1
  success <- FALSE
  while (trial <= as.integer(max_tries)) {
    request <- httr::GET(url = link_to_file,
                         httr::write_disk(temp_file_name))

    if (httr::status_code(request) == 200) {
      success <- TRUE
      probable_encodings <-
        readr::guess_encoding(temp_file_name)
      file_content <- readr::read_lines(temp_file_name, progress = FALSE) %>%
        stringr::str_conv(probable_encodings$encoding[1])

      subsets <- tibble::tibble(text = file_content) %>%
        dplyr::mutate(
          line = dplyr::row_number(),
          aux = stringr::str_detect(file_content, "^\\d") |
            stringr::str_detect(file_content, "^\\s*(\\d)")
        ) %>%
        dplyr::filter(.data$aux == TRUE) %>%
        dplyr::mutate(
          consecutive = .data$line - dplyr::lag(.data$line),
          group = dplyr::if_else(.data$consecutive == 1, 0, 1,
                                 missing = 1) %>%
            base::cumsum()
        ) %>%
        dplyr::group_by(.data$group) %>%
        dplyr::summarise(start = dplyr::first(.data$line) - 1,
                         end = dplyr::last(.data$line))

      header_info <-
        readr::read_lines(I(file_content),
                          n_max = subsets$start[1] - 2,
                          progress = FALSE) %>%
        stringr::str_trim(side = "both") %>%
        paste(collapse = " ")

      subsets <- subsets %>%
        dplyr::mutate(
          name = purrr::map_chr(.data$start - 2, read_info,
                                file_content),
          data = purrr::map2(.data$start - 1,
                             .data$end - .data$start,
                             read_data,
                             file_content)) %>%
        dplyr::select(.data$name, .data$data)

      if (!is.null(dir)){
        fs::file_copy(path = temp_file_name,
                      new_path = file.path(dir, dest_file),
                      overwrite = overwrite)
        cli::cli_alert_info(paste("File writen to:",
                            file.path(dir, dest_file)))
      }

      fs::file_delete(temp_file_name)

      break()
    } else {
      trial <- trial + 1
      cli::cli_h3("Error downloading the file")
      cli::cli_alert_danger(httr::http_status(request)$message)
      cli::cli_alert_info("Trying again in 5 seconds. Please wait...")
      if (fs::file_exists(temp_file_name)){
        fs::file_delete(temp_file_name)
      }
      Sys.sleep(5)
    }
  }

  if (success == FALSE) {
    cli::cli_h3("Unable to download and read the file")
    cli::cli_alert_danger("Max trials reached!")
    cli::cli_alert(
      paste(
        "Check your internet connection; please check if you can download the file manually <",
        link_to_file, "> using a browser."))
    cli::cli_alert(
      "Try again in a couple of minutes and if the problem persists please open a ticket on the package github site.")
    results <- NULL
  } else {
    results <-
      structure(list(
        info = paste(header_info, "\n\n",
          "Information collected from:",
          link_to_file,
          "on",
          format(Sys.time(), "%a %b %d %H:%M:%S %Y")
        ),
        details_url = paste0(base_url, url_info$details_url[1]),
        subsets = subsets
      ),
      class = "french_dataset")
  }

  return(results)
}




