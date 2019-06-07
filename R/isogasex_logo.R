#' The isogasex logo, using ASCII or Unicode characters
#'
#' Use [crayon::strip_style()] to get rid of the colors.
#'
#' @param sw_unicode Whether to use Unicode symbols. Default is `TRUE`
#'   on UTF-8 platforms.
#'
#' @md
#' @export
#' @examples
#' isogasex_logo()
isogasex_logo <- function (sw_unicode = l10n_info()$`UTF-8`) {
  logo <-
    c(
      ""
    , " 0 _      1     2         9            3   4 "
    , "  (_)___  ___    ___  ___   ___   ___ __  __ "
    , " / /(_-< / . )  / . )/ . ) (_-<  / -_)\\ \\/ / "
    , "/_//___/(___/  (_, /(__,_|/___/  \\__/ /_/\\_\\ "
    , "     5   6    /___/      7      8         9    "
    , ""
    )

  hexa <- c("*", ".", "o", "*", ".", "*", ".", "o", ".", "*")

  if (sw_unicode) {
    hexa <- c("*" = "\u2b22", "o" = "\u2b21", "." = ".")[hexa]
  }

  cols <-
    c("red", "yellow", "green", "magenta", "cyan", "yellow",
      "green", "white", "magenta", "cyan")

  col_hexa <-
    purrr::map2(hexa, cols, ~ crayon::make_style(.y)(.x))

  for (i in 0:9) {
    pat <- paste0("\\b", i, "\\b")
    logo <- sub(pat, col_hexa[[i + 1]], logo)
  }

  #structure(crayon::blue(logo), class = "tidyverse_logo")
  structure(crayon::blue(logo), class = "isogasex_logo")

}

#' @export

print.isogasex_logo <- function(x, ...) {
  cat(x, ..., sep = "\n")
  invisible(x)
}
