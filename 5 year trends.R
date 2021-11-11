library(tidyverse)

load("NHANESCleanFactors.RData")
load("TranslationsFull.RData")

load("NHANES_Clean_2009_2010.RData")
# renaming 2009-2010 for consistency
df_09_10 <- df_full2009

load("NHANES_Clean_2011_2012.RData")
load("NHANES_Clean_2013_2014.RData")
load("NHANES_Clean_2015_2016.RData")
load("NHANES_Clean_2017_2018.RData")

# Getting list of all column names for each year

colname_09_10 <- colnames(df_09_10)
colname_11_12 <- colnames(df_11_12)
colname_13_14 <- colnames(df_13_14)
colname_15_16 <- colnames(df_15_16)
colname_17_18 <- colnames(df_17_18)
colname_full <- colnames(df_full)


# All five years
common_cols <- Reduce(intersect, list(colname_09_10, colname_11_12, colname_13_14, colname_15_16, colname_17_18, colname_full))

ROV_questions <- list()

for(i in 1:505){
  x <- translate_full[[i]]
  if(!(is.double(translate_full[[i]]))){
    for(j in 1:ncol(x)){
      if(x[j,2] == "Range of Values"){
        ROV_questions <- append(ROV_questions, translate_full[i])
      }
    }
  }
}
common_ROVq_5year <- intersect(names(ROV_questions),common_cols)

# common questions for each year
common_09_10 <- df_09_10[which(common_ROVq_5year %in% colname_09_10)]
common_11_12 <- df_11_12[which(common_ROVq_5year %in% colname_11_12)]
common_13_14 <- df_13_14[which(common_ROVq_5year %in% colname_13_14)]
common_15_16 <- df_15_16[which(common_ROVq_5year %in% colname_15_16)]
common_17_18 <- df_17_18[which(common_ROVq_5year %in% colname_17_18)]


