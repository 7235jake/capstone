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
          if(df[j, i] < q[1]){
            df[j, i] <- q[1]
          }
          if(df[j, i] > q[2]){
            df[j, i] <- q[2]
          }
        }
      }
    }
  }
  
}

winsorize(x)
winsorize(df_11_12)
winsorize(df_13_14)
winsorize(df_15_16)
winsorize(df_17_18)
winsorize(df_full2009)

save(x, file = "selected_df.RData")
save(df_11_12, file = "selected_df.RData")
save(df_13_14, file = "selected_df.RData")
save(df_15_16, file = "selected_df.RData")
save(df_17_18, file = "selected_df.RData")
save(df_full2009, file = "selected_df.RData")
