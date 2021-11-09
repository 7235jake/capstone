library(nhanesA)
library(tidyverse)
library(plyr)

# ------- Load in data ---------

load("NHANES_Clean_2009_2010.RData")
load("NHANES_Clean_2011_2012.RData")
load("NHANES_Clean_2013_2014.RData")
load("NHANES_Clean_2015_2016.RData")
load("NHANES_Clean_2017_2018.RData")

# ------- Make Names Consistent ---------

df_17_18 <- as.data.frame(df_full)
df_15_16 <- as.data.frame(df_full1516)
df_13_14 <- as.data.frame(df_full1314)
df_11_12 <- as.data.frame(df_11_12)
df_09_10 <- as.data.frame(df_full2009)

# ------- Getting Column names for each year ---------

a <- colnames(df_17_18)
b <- colnames(df_15_16)
c <- colnames(df_13_14)
d <- colnames(df_11_12)
e <- colnames(df_09_10)

# ------- Finding questions that arent in 2017-2018

names_15_16 <- a[pmatch(b, a)]
names_15_16 <- names_15_16[!is.na(names_15_16)]

names_13_14 <- a[pmatch(c, a)]
names_13_14 <- names_13_14[!is.na(names_13_14)]

names_11_12 <- a[pmatch(d, a)]
names_11_12 <- names_11_12[!is.na(names_11_12)]

names_09_10 <- a[pmatch(e, a)]
names_09_10 <- names_09_10[!is.na(names_09_10)]

# ------- Getting Rid of questions that aren't in 2017-2018

df_15_16 <- df_15_16[c(names_15_16)]
df_13_14 <- df_13_14[c(names_13_14)]
df_11_12 <- df_11_12[c(names_11_12)]
df_09_10 <- df_09_10[c(names_09_10)]

a <- colnames(df_17_18)
b <- colnames(df_15_16)
c <- colnames(df_13_14)
d <- colnames(df_11_12)
e <- colnames(df_09_10)

# ------- Names 2017 - 2018 not in Specified Year -------
name_1718_1516 <- colnames(df_17_18[, -which(a %in% b)])
name_1718_1314 <- colnames(df_17_18[, -which(a %in% c)])
name_1718_1112 <- colnames(df_17_18[, -which(a %in% d)])
name_1718_0910 <- colnames(df_17_18[, -which(a %in% e)])


# ------- Adding Cols Not in Each year and filling them with NA -------

null_15_16 <- as.data.frame(matrix(data = NA, nrow = nrow(df_15_16), ncol = length(name_1718_1516)))
colnames(null_15_16) <- name_1718_1516

null_13_14 <- as.data.frame(matrix(data = NA, nrow = nrow(df_13_14), ncol = length(name_1718_1314)))
colnames(null_13_14) <- name_1718_1314

null_11_12 <- as.data.frame(matrix(data = NA, nrow = nrow(df_11_12), ncol = length(name_1718_1112)))
colnames(null_11_12) <- name_1718_1112

null_09_10 <- as.data.frame(matrix(data = NA, nrow = nrow(df_09_10), ncol = length(name_1718_0910)))
colnames(null_09_10) <- name_1718_0910

# ------- Combining them with their respective years -------

df_15_16 <- cbind(df_15_16, null_15_16)
df_13_14 <- cbind(df_13_14, null_13_14)
df_11_12 <- cbind(df_11_12, null_11_12)
df_09_10 <- cbind(df_09_10, null_09_10)

# ------- Order names and combine all years into one Data frame -------

a <- colnames(df_17_18)
b <- colnames(df_15_16)
c <- colnames(df_13_14)
d <- colnames(df_11_12)
e <- colnames(df_09_10)

df_17_18 <- df_17_18[ , order(names(df_17_18))]
df_15_16 <- df_15_16[ , order(names(df_15_16))]
df_13_14 <- df_13_14[ , order(names(df_13_14))]
df_11_12 <- df_11_12[ , order(names(df_11_12))]
df_09_10 <- df_09_10[ , order(names(df_09_10))]


setdiff(e, a)

#df_full <- rbind(df_17_18, df_15_16)
#df_full <- rbind(df_full, df_13_14)
df_full <- rbind(df_17_18, df_11_12) ## This works
#df_full <- rbind(df_full, df_09_10)

