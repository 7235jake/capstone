library(tidyverse)
library(DescTools)
load("selected_df.RData")
load("NHANES_Clean_2009_2010.RData")
load("NHANES_Clean_2011_2012.RData")
load("NHANES_Clean_2013_2014.RData")
load("NHANES_Clean_2015_2016.RData")
load("NHANES_Clean_2017_2018.RData")

winsorize <- function(df)
{
  for(i in 1:ncol(df))
  {
    if(!(is.factor(df[,i]))){
      q <- quantile(df[,i], c(0.025, 0.975), na.rm = TRUE, type = 1)
      for(j in 1:nrow(df))
      {
        if(!(is.na(df[j, i]))){
          if(df[j, i] < q[[1]]){
            df[j, i] <- q[[1]]
          }
          if(df[j, i] > q[[2]]){
            df[j, i] <- q[[2]]
          }
        }
      }
    }
  }
  return(df)
}

# df <- df_11_12
# 
# if(!(is.factor(df[,3]))){
#   q <- quantile(df[,3], c(0.025, 0.975), na.rm = TRUE, type = 1)
#   for(j in 1:nrow(df))
#   {
#     if(!(is.na(df[j, 3]))){
#       if(df[j, 3] < q[[1]]){
#         df[j, 3] <- q[[1]]
#       }
#       if(df[j, 3] > q[[2]]){
#         df[j, 3] <- q[[2]]
#       }
#     }
#   }
# }

# # Running the Function and Saving

# select <- winsorize(selected_df)
# df0910 <- winsorize(df_full2009)
# df1112 <- winsorize(df_11_12)
# df1314 <- winsorize(df_13_14)
# df1516 <- winsorize(df_15_16)
# df1718 <- winsorize(df_17_18)
# 
# 
# save(select, file = "selected_Winsorized_df.RData")
# save(df0910, file = "NHANES_Winsorized_2009_2010.RData")
# save(df1112, file = "NHANES_Winsorized_2011_2012.RData")
# save(df1314, file = "NHANES_Winsorized_2013_2014.RData")
# save(df1516, file = "NHANES_Winsorized_2015_2016.RData")
# save(df1718, file = "NHANES_Winsorized_2017_2018.RData")

# # Table Ones
# install.packages("tableone")
# library(tableone)
# 
# load("NHANES_Winsorized_2009_2010.RData")
# load("NHANES_Winsorized_2011_2012.RData")
# load("NHANES_Winsorized_2013_2014.RData")
# load("NHANES_Winsorized_2015_2016.RData")
# load("NHANES_Winsorized_2017_2018.RData")
# 
# vars <- c("LBXGLU", "WTINT2YR", "PAD675", "LBXTC", "BMXBMI")
# CreateTableOne(vars = vars, strata = "DIQ010", data = df0910)
