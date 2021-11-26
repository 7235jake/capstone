library(tidyverse)

x <- intersect(colnames(common_09_10), colnames(selected_df))

y <- c("CBD111", "DBD381", "DBD411", "DBD895", "RIDAGEMN", "RIDAGEYR", 
       "RIDEXAGM", "SDMVPSU", "SDMVSTRA")

qs_to_include <- x[!(x %in% y)]
qs_to_include


save(qs_to_include, file = "Diabetes_ROV_qs.RData")
