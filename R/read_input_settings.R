#' Read input files for either training the classifier or for advert classification
#'
#' @param fn_scorecard         a list of csv filenames to read and join together, expected to have same column names.
#' @param sw_read_write     "read" for infiles to train or classify, "write" for classification results (includes a column of classifications)
#' @param sw_log_file       see AT_ScoreCard_SigTest()
#' @param out_log           defined inside AT_ScoreCard_SigTest()
#'
#' @return dat a single dataframe
#' @importFrom log4r create.logger
#' @importFrom log4r logfile
#' @importFrom log4r level
#' @importFrom log4r debug
#' @importFrom log4r info
#' @importFrom log4r warn
#' @importFrom log4r error
#' @importFrom readr read_csv
#' @importFrom jsonlite read_json
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr last
#' @export
read_input_settings <-
  function(
    fn_scorecard  = NULL
  , sw_read_write = c("read", "write")
  , out_dat       = NULL
  , sw_log_file   = NULL
  , out_log       = NULL
  ) {

  # # read files
  # if (sw_read_write == "read") {
  #   # initialize input data list
  #   dat_list <- NULL
  # }
  # for (i_fn in 1:length(fn_scorecard)) {

    # determine the file extension
    #library(stringr)
    file_ext <-
      fn_scorecard %>%
      stringr::str_split(stringr::fixed(".")) %>%
      unlist() %>%
      dplyr::last()

    if (!(file_ext %in% c("csv", "json"))) {
      if (sw_log_file) {
        # library(log4r)
        log4r::error(out_log, paste0("fn_scorecard file type not recognized, must be .csv or .json: ", fn_scorecard))
      }
      stop(paste0("fn_scorecard file type not recognized, must be .csv or .json: ", fn_scorecard))
    }

    # read files
    if (sw_read_write == "read") {
      if (file_ext == "csv") {
        #library(readr)
        dat <- readr::read_csv(fn_scorecard)
      }

      if (file_ext == "json") {
        #library(jsonlite)
        dat <- jsonlite::read_json(fn_scorecard, simplifyVector = FALSE)
      }
    }

    # write files
    if (sw_read_write == "write") {
      if (file_ext == "csv") {
        #library(readr)
        readr::write_csv(out_dat, fn_scorecard)
      }

      if (file_ext == "json") {
        #library(jsonlite)
        jsonlite::write_json(
          out_dat
        , fn_scorecard
        , pretty = TRUE
        , auto_unbox = TRUE
        , digits = 6
        , null = "null"
        )
      }
    }
  # }

  # read files
  if (sw_read_write == "read") {
    # # library(dplyr)
    # #dat <- rbind.fill(dat_list)
    # #dat <- do.call(rbind, dat_list)
    # dat <- dplyr::bind_rows(dat_list)
    #
    # # sort the data by ad and frame number
    # #library(dplyr)
    # dat <- dplyr::arrange(dat, ad_id, frame_number)
    if (sw_log_file) {
      # library(log4r)
      log4r::info(out_log, paste0("infile, number of rows: ", length(dat)))
    }

    return(dat)
  }



  # write files
  if (sw_read_write == "write") {
    invisible(NULL)
  }
}
