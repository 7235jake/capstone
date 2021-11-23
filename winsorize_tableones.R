library(tidyverse)
library(DescTools)
load("selected_df.RData")

x <- selected_df

cols <- colnames(x)

for(i in 1:length(cols))
{
  if(!(is.factor(x[,i]))){
    q <- quantile(x[,i], c(0.025, 0.975), na.rm = TRUE, type = 1)
    for(j in 1:length(x[,i]))
    {
      if(!(is.na(x[j, i]))){
        if(x[j, i] < q[1]){
          x[j, i] <- q[1]
        }
        if(x[j, i] > q[2]){
          x[j, i] <- q[2]
        }
      }
    }
  }
}

