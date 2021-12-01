library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)

load("NHANESCleanFactors.RData")
load("TranslationsFull.RData")

load("NHANES_Winsorized_2009_2010.RData")
load("NHANES_Winsorized_2011_2012.RData")
load("NHANES_Winsorized_2013_2014.RData")
load("NHANES_Winsorized_2015_2016.RData")
load("NHANES_Winsorized_2017_2018.RData")

# Getting list of all column names for each year

colname_09_10 <- colnames(df0910)
colname_11_12 <- colnames(df1112)
colname_13_14 <- colnames(df1314)
colname_15_16 <- colnames(df1516)
colname_17_18 <- colnames(df1718)
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
common_09_10 <- df0910[which(colname_09_10 %in% common_ROVq_5year)]
common_11_12 <- df1112[which(colname_11_12 %in% common_ROVq_5year)]
common_13_14 <- df1314[which(colname_13_14 %in% common_ROVq_5year)]
common_15_16 <- df1516[which(colname_15_16 %in% common_ROVq_5year)]
common_17_18 <- df1718[which(colname_17_18 %in% common_ROVq_5year)]


#Order Names

common_17_18 <- common_17_18[ , order(names(common_17_18))]
common_15_16 <- common_15_16[ , order(names(common_15_16))]
common_13_14 <- common_13_14[ , order(names(common_13_14))]
common_11_12 <- common_11_12[ , order(names(common_11_12))]
common_09_10 <- common_09_10[ , order(names(common_09_10))]



# Plots

for(i in 1:ncol(common_09_10))
{
  vec_09_10 <- common_09_10[,i]
  vec_11_12 <- common_11_12[,i]
  vec_13_14 <- common_13_14[,i]
  vec_15_16 <- common_15_16[,i]
  vec_17_18 <- common_17_18[,i]
  
  df <- data.frame("Year" = c("2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2018"), 
                   "Quantities" = c(vec_09_10, vec_11_12, vec_13_14, vec_15_16, vec_15_16 ))
  df <- na.omit(df)
  subtitle_vec <- Hmisc::label(common_09_10)
  sub_title <- paste(colnames(common_09_10)[i], subtitle_vec[[i]], sep = " - ")
  
  # Boxplots including 2 Standard Deviations on each side (95%)
  
  gg <- ggplot(df, aes(x=Year, y=Quantities)) + 
          geom_boxplot(fill="slateblue") + 
          theme(axis.text.x = element_text(angle = 45, hjust=1)) +
          labs(subtitle=sub_title ,
               y="Quantities", x="Year", title="Quantities Vs Year")
  
  # Saving Boxplots
  plot(gg)
  filename = paste("C:/Users/owner/OneDrive/CMDA Capstone/capstone/ROV_5_year_boxplots/", colnames(common_09_10)[i],"-", "Boxplot.pdf", sep="")
  ggsave(filename, gg)
  
}

# List of Questions to re-evaluate for outliers and skewness

outlier_questions <- c("BMXARML", "BMXBMI", "BMXHT", "BPD035", "CBD111", "CBD121", 
                       "CBD131", "DBD030", "DBD041", "DBD050", "DBD055", "DBD061", 
                       "DBD381", "DBD411", "DBD895", "DBD900", "DBD905", "DBD910", 
                       "DID040", "DID060", "DID250", "DID260", "DID341", "DID350", 
                       "LBDGLUSI", "LBDHDD", "LBDHDDSI", "LBDLDL", "LBDLDLSI", 
                       "LBDTCSI", "LBDTRSI", "LBXGLU", "LBXTC", "LBXTR", "MCD180A", 
                       "MCD180B", "MCD180C", "MCD180D", "MCD180E", "MCD180F", "MCD180G", 
                       "MCD180K", "MCD180L", "MCD180M", "MCD180N", "MCQ025", "PAD615", 
                       "PAD630", "PAD645", "PAD660", "PAD675", "PAD680", "PAQ610", 
                       "PAQ625", "PAQ640", "PAQ655", "PAQ670", "RIDEXAGM", "WHD010", 
                       "WHD020", "WHD050", "WHD110", "WHD120", "WHD130", "WHD140", 
                       "WHQ150", "WTINT2YR", "WTMEC2YR")


