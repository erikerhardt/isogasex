#' Read template parameter file
#'
#' @param param_fn A yaml or Excel parameter file
#'
#' @return PARAM_RAW a list of parameter values
#' @importFrom yaml read_yaml
#' @importFrom readxl read_xls
#' @importFrom stringr str_split
#' @importFrom stringr fixed
#' @importFrom dplyr last
#' @importFrom magrittr %>%
read_template_param <-
  function(
    param_fn = NULL
  ) {

  ## DEBUG
  # path <- "C:/Dropbox/StatAcumen/consult/Authorship/2009_DavidHanson_Isotopes/package_testing/template_text"
  # fn <- "template4_text.yaml"
  # fn <- "Templaste4_John_tdllicor_2019-05-17_CCM_test.xls"
  # param_fn = paste(path, fn, sep="/")
  #
  # all.equal(PARAM_RAW, PARAM_RAW2)
  # for (i in 1:length(names(PARAM_RAW))) {
  #   print(i)
  #   print( (PARAM_RAW[[i]] == PARAM_RAW2[[i]]) )
  #   print( (names(PARAM_RAW)[[i]] == names(PARAM_RAW2)[[i]]) )
  # }

  # determine the parameter file extension
  file_ext <-
    param_fn %>%
    stringr::str_split(stringr::fixed(".")) %>%
    unlist() %>%
    dplyr::last()

  if (!(file_ext %in% c("xls", "xlsx", "yaml"))) {
    p_o <- paste0("param_fn file type not recognized, must be .yaml, .xls, or .xslx: ", param_fn); write_out(p_o)
    stop(paste0("param_fn file type not recognized, must be .yaml, .xls, or .xslx: ", param_fn))
  }

  # yaml text parameter file
  if (file_ext == "yaml") {
    PARAM_RAW <- yaml::read_yaml(file = param_fn)

    # replace NULL with NA
    for (i_list in seq_along(PARAM_RAW)) {
      if (is.null(PARAM_RAW[[i_list]])) {
        PARAM_RAW[[i_list]] <- NA
      }
    }

  }

  # Excel parameter file
  if (file_ext %in% c("xls", "xlsx")) {
    PARAM_TABLE <-
      as.matrix( readxl::read_xls(param_fn))

    PARAM_RAW <-
      read_template_param_table_to_raw(PARAM_TABLE)
  }

  return(PARAM_RAW)
}
