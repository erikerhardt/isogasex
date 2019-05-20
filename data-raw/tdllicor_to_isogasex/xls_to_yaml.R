setwd("C:/Dropbox/StatAcumen/consult/Authorship/2009_DavidHanson_Isotopes/R-package/isogasex/data-raw/tdllicor_to_isogasex")
library(tidyverse)

write_yaml(as.yaml(t(VARIABLES)), "isogasex_template4.yaml")

VAR2 <-
  "isogasex_template4_byhand.yaml" %>%
  read_yaml() %>%
  yaml.load()



# period list to unique

list_exclude <-
  c(
    "Sys.timezone"
  , "as.POSIXct"
  , "as.list"
  , "as.numeric"
  , "as.character"
  , "capture.output"
  , "create.logger"
  , "dev.control"
  , "dev.set"
  , "dir.create"
  , "do.call"
  , "e.g."
  , "error.message"
  , "file.copy"
  , "file.exists"
  , "file.remove"
  , "file.rename"
  , "flush.console"
  , "graphics.off"
  , "i.e."
  , "install.packages"
  , "is.finite"
  , "is.na"
  , "is.null"
  , "is.numeric"
  , "na.rm"
  , "new.env"
  , "pkgs.had.co.nz"
  , "rbind.fill"
  , "read.csv"
  , "read.delim"
  , "read.xls"
  , "set.seed"
  , "smooth.spline"
  , "write.csv"
  , "X.U.S."
  )

dat <-
  readLines("periods_dups.txt") %>%
  unique() %>%
  sort() %>%
  .[str_detect(., pattern = fixed("."))] %>%
  .[!(. %in% list_exclude)]

# sort by string length desc, so subsets aren't replaced
dat <-
  dat[order(-str_length(dat))]

dat <-
  dat %>%
  str_replace_all(fixed("."), fixed("[.]"))

dat %>%
  writeLines(con = "periods_unique.txt")

