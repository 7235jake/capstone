library(nhanesA)
library(tidyverse)

## called df_full2009
load("NHANES_Clean_2009_2010.RData") 

## called df_11_12
load("NHANES_Clean_2011_2012.RData") 

## called df_full1314
load("NHANES_Clean_2013_2014.RData") 

## called df_full1516
load("NHANES_Clean_2015_2016.RData") 

## called df_full
load("NHANES_Clean_2017_2018.RData") 


# ------- Make names consistent ---------

df_09_10 <- df_full2009
df_11_12 <- df_11_12
df_13_14 <- df_full1314
df_15_16 <- df_full1516
df_17_18 <- df_full

# ------- Make column names consistent to merge ---------
# 17-18 - year





