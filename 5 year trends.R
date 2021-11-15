library(tidyverse)
library(ggplot2)
library(ggalt)

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
common_09_10 <- df_09_10[which(colname_09_10 %in% common_ROVq_5year)]
common_11_12 <- df_11_12[which(colname_11_12 %in% common_ROVq_5year)]
common_13_14 <- df_13_14[which(colname_13_14 %in% common_ROVq_5year)]
common_15_16 <- df_15_16[which(colname_15_16 %in% common_ROVq_5year)]
common_17_18 <- df_17_18[which(colname_17_18 %in% common_ROVq_5year)]


# Plots
x_axis <- c("2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018")

avgs <- matrix(0, 5, 80)

for(i in 1:ncol(common_09_10))
{
  vec_09_10 <- common_09_10[i]
  vec_11_12 <- common_11_12[i]
  vec_13_14 <- common_13_14[i]
  vec_15_16 <- common_15_16[i]
  vec_17_18 <- common_17_18[i]
  
  
  v_09_10 <- vec_09_10[!is.na(vec_09_10)]
  v_11_12 <- vec_11_12[!is.na(vec_11_12)]
  v_13_14 <- vec_13_14[!is.na(vec_13_14)]
  v_15_16 <- vec_15_16[!is.na(vec_15_16)]
  v_17_18 <- vec_17_18[!is.na(vec_17_18)]
  
  avgs[1, i] <- mean(v_09_10)
  avgs[2, i] <- mean(v_11_12)
  avgs[3, i] <- mean(v_13_14)
  avgs[4, i] <- mean(v_15_16)
  avgs[5, i] <- mean(v_17_18)
  
}

colnames(avgs) <- colnames(common_09_10)
rownames(avgs) <- x_axis

index <- c(1, 2, 3, 4, 5)

for(i in 1:length(colnames(common_09_10)))
{
  df <- data.frame("Year" = c("2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"), "Averages" = avgs[,i])
  df

  subtitle_vec <- Hmisc::label(common_09_10)
  sub_title <- paste(colnames(common_09_10)[i], subtitle_vec[[i]], sep = " - ")
  
  gg <- ggplot(df, aes(x=Year, y=Averages, group=1)) +  geom_line() + geom_point(aes(size=Averages)) +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(subtitle=sub_title , 
         y="Average", x="Year", title="Average Vs Year")
  
  # Saving Plots
  
  filename = paste("C:/Users/owner/OneDrive/CMDA Capstone/capstone/ROV_avgs_5_year_plots/", colnames(common_09_10)[i],"-", "Avg_vs_Year.pdf", sep="")
  ggsave(filename, gg)

}


