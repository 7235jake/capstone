library(tidyverse)
library(nhanesA)
library(stringr)

df_all <- df_full
# table(df_all$DBQ770)
# table(df_all$DBQ770.1)
# 
# 
# table(df_17_18$DBQ770)
# table(df_17_18$DBQ770.1)
# 
# which(x == "DBQ770.1")
# list(c("DBQ770.1")) %>% contains(c("."))
# grepl(".", "DBQ770.1", fixed = T)
# substring("DBQ770.1", 1,nchar("DBQ770.1")-2)
# nchar("DBQ770.1")

#------------ Create translation function -----------
translate <- function(cols){
  table_names <- c("DIQ_J", "HDL_J","TRIGLY_J", "TCHOL_J", "GLU_J", "INS_J", "CBQPFC_J", "HSQ_J", "DBQ_J",  
                   "MCQ_J", "PAQ_J", "PFQ_J", "WHQ_J", "CBQ_J", 
                   "CBQPFA_J", "MCQ_J", "BMX_J", "BPQ_J", "DEMO_J")
  trans_table <- list()
  for (i in seq_along(table_names)){
    if(length(nhanesTranslate(table_names[i], colnames = cols)) > 0){
      return((nhanesTranslate(table_names[i], colnames = cols)))
    }
  }
  return (0)
  
}
response_values = list()

# Go through all columns in dataframe
for (i in seq_along(colnames(df_all))) {
  #Translate call for SEQN returns an error so skip over
  if(colnames(df_all[i])=='SEQN' || colnames(df_all[i])=='Year'){
    i = i+1
  }
  if( grepl(".1", colnames(df_all[i]), fixed = T)){
    tran <- translate( substring( colnames(df_all[i]), 1, nchar(colnames(df_all[i]))-2 ) )
  }
  else{
  # Grab translation of column
    tran <- translate(colnames(df_all[i]))
  }
  #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
  #responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
  if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
      ('No' %in% responses && !('Range of Values' %in% responses))){
    response_values <- append(response_values,tran)
  }
}
  


#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_all) == questions[i])
  
  for (row in seq_along(df_all[,col_num])){
    
    if( df_all[row,col_num] != 1 && df_all[row,col_num] != 2 && !is.na(df_all[row,col_num])){
      df_all[row,col_num] <- 2
    }
    
  }
}

df_full <- df_all
#save(df_full, file = "NHANES_Clean.RData")
#save(response_values, file = "FullDataResponseCleanYesNo.RData")


#----------Create list of translations for all questions

translate_full <- list()

for (i in 429:length(colnames(df_full))) {
  #Translate call for SEQN returns an error so skip over
  if(colnames(df_full[i])=='SEQN' || colnames(df_full[i])=='Year'){
    i = i+1
  }
  if( grepl(".1", colnames(df_full[i]), fixed = T)){
    tran <- translate( substring( colnames(df_full[i]), 1, nchar(colnames(df_full[i]))-2 ) )
  }
  else{
    # Grab translation of column
    tran <- translate(colnames(df_full[i]))
  }
    translate_full <- append(translate_full,tran)
  }
#save(translate_full, file = "TranslationsFull.RData")


#---------Convert questions to factors
factor_questions <- c()
for( i in seq_along(translate_full)){
  if(names(translate_full)[i] !='' &&  !('Range of Values' %in% as.data.frame(translate_full[i])[,2])){
    factor_questions <- append(factor_questions, names(translate_full)[i])
  }
}

#Find dupicates in factor questions to account for .1
n_occur <- data.frame(table(factor_questions))
dup_questions <- as.character(n_occur[n_occur$Freq > 1,1])
for(i in seq_along(dup_questions)){
  index = which(factor_questions==dup_questions[i])[2]
  factor_questions[index] = str_c(factor_questions[index], ".1")
}

#make sure the questions are in the dataframe
not_in <- factor_questions[!factor_questions %in% colnames(df_full)]
factor_questions <- factor_questions[!factor_questions %in% c(not_in)]
#Set columns to be factors

df_full[,c(factor_questions)] <- lapply(df_full[,c(factor_questions)], factor)




#Delete check item questions 
df_full <- df_full %>% select(-c("CBQ738CD","CBQ738CD.1"))
translate_full <- (translate_full[-c(97,98)])
#Delete questions at the end that have exact same responses
df_full <- df_full %>% select(-c(502,503))

# Identify questions with one response level

single_level_questions <- list()
for (x in seq_along(colnames(df_full))){
  if( length(table(df_full[,x]) ) ==1 ){
    single_level_questions <- append(single_level_questions, colnames(df_full)[x])
  }
  
}

#Remove BMI questions with "could not contain" responses for body measurements
df_full <- df_full %>% select(-unlist(single_level_questions[1:5]))

#list of single level questions
single_level_questions <- (unlist(single_level_questions))
single_level_questions <- single_level_questions[(single_level_questions %in% colnames(df_full))]
#handle questions with .1 first
unique_single_level <- unique(str_sub(single_level_questions, start = 1, end = 6))



#CBQ695_(.1)

cbq695_1 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[1]] %>% .[str_sub(., start = -2, end = -1)==".1"]
#Combine columns together with unite
df_full <- df_full %>% unite(col = "CBQ695.1", all_of(cbq695_1), na.rm = TRUE)
#Put back in NAS
#df_full$CBQ695.1 <- df_full %>% replace_na(replace = list(CBQ695.1 = c("")))
table(df_full$CBQ695.1)
df_full$CBQ695.1


cbq695 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[1]] %>% .[str_sub(., start = -2, end = -1)!=".1"]
df_full <- df_full %>% unite(col = "CBQ695", all_of(cbq695), na.rm = TRUE)
select(df_full,CBQ695)
#df_full$CBQ695 <- df_full %>% replace_na(replace = list(CBQ695 = c("")))
table(df_full$CBQ695)
df_full$CBQ695



#CBQ698_(.1)
cbq698_1 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[2]] %>% .[str_sub(., start = -2, end = -1)==".1"]
df_full <- df_full %>% unite(col = "CBQ698.1", all_of(cbq698_1), na.rm = TRUE)
#df_full$CBQ698.1 <- df_full %>% replace_na(replace = list(CBQ098.1 = c("")))
table(df_full$CBQ698.1)
df_full$CBQ698.1

cbq698 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[2]] %>% .[str_sub(., start = -2, end = -1)!=".1"]
df_full <- df_full %>% unite(col = "CBQ698", all_of(cbq698), na.rm = TRUE)
#df_full$CBQ698 <- df_full %>% replace_na(replace = list(CBQ698 = c("")))
table(df_full$CBQ698)
df_full$CBQ698


#CBQ738_(.1)
cbq738_1 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[3]] %>% .[str_sub(., start = -2, end = -1)==".1"]
df_full <- df_full %>% unite(col = "CBQ738.1", all_of(cbq738_1), na.rm = TRUE)
table(df_full$CBQ738.1)
df_full$CBQ738.1

cbq738 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[3]] %>% .[str_sub(., start = -2, end = -1)!=".1"]
df_full <- df_full %>% unite(col = "CBQ738", all_of(cbq738), na.rm = TRUE)
table(df_full$CBQ738)
df_full$CBQ738


#DBQ073
dbq073 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[4]]
df_full <- df_full %>% unite(col = "DBQ073", all_of(dbq073), na.rm = TRUE)
table(df_full$DBQ073)
df_full$DBQ073


#DBQ223
dbq223 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[5]]
df_full <- df_full %>% unite(col = "DBQ223", all_of(dbq223), na.rm = TRUE)
table(df_full$DBQ223)
df_full$DBQ223

#DIQ175
diq175 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[6]]
df_full <- df_full %>% unite(col = "DIQ175", all_of(diq175), na.rm = TRUE)
table(df_full$DIQ175)
df_full$DIQ175

#MCQ230
mcq230 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[7]]
df_full <- df_full %>% unite(col = "MCQ230", all_of(mcq230), na.rm = TRUE)
table(df_full$MCQ230)
df_full$MCQ230

#MCQ510
mcq510 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[8]]
df_full <- df_full %>% unite(col = "MCQ510", all_of(mcq510), na.rm = TRUE)
table(df_full$MCQ510)
df_full$MCQ510

#WHD080
whd080 <- colnames(df_full) %>% .[str_sub(., start=1, end=6) == unique_single_level[9]]
df_full <- df_full %>% unite(col = "WHD080", all_of(whd080), na.rm = TRUE)
table(df_full$WHD080)
df_full$WHD080


#Now we have to map our responses again for combined valeues (ex: 1_2_3)
#Store new column names
merged_cols <- c("CBQ695.1", "CBQ695",  "CBQ698.1", "CBQ698", "CBQ738.1", "CBQ738", "DBQ073", "DBQ223", "DIQ175", "MCQ230", "MCQ510", "WHD080")
df_merged <- df_full[,merged_cols]

for( col in seq_along(df_merged)){
  
  for(row in seq(dim(df_merged)[1])){
    # if underscore in response
    if(unlist(gregexpr("_", df_merged[row,col]))[1] != -1){
      
      value = df_merged[row,col]
      #get index of underscore
      index = str_locate(pattern = "_",df_merged[row,col])[1,1]
      #Get rid of values after underscore
      new_value <- str_sub(value, start = 1, end = index-1 )
      #if our new value is 99 change to empty string
      if(new_value == "99" || new_value =="77"){
        df_merged[row,col] <- ""
      }
      else {
        df_merged[row,col] <- new_value
        
      }
    }
    else if( df_merged[row,col]=="99" || df_merged[row,col]=="77"){
      df_merged[row,col] <- ""
    }
    
  }
  df_full[,merged_cols[col]] <- as.factor(df_merged[,col])
  print((table(df_merged[,col])))
  
}

#Remove 99 and 77 from remaining factor questions 
for(i in seq_along(factor_questions)){
  if(length(which(colnames(df_full)==factor_questions[i])) > 0){
    colnum <- which(colnames(df_full)==factor_questions[i])
    
    for( row in seq(dim(df_full)[1]) ){
      if(is.na(df_full[row, colnum])){
        row = row + 1
      }
      else if(df_full[row, colnum]=="99" || df_full[row,colnum]=="77"){
        df_full[row,colnum] <-  NA
      }
    }
    droplevels(df_full[,colnum])
    #print(table(df_full[,colnum]))
  }
  
}

for(i in seq_along(factor_questions)){
  if(length(which(colnames(df_full)==factor_questions[i])) > 0){
    colnum <- which(colnames(df_full)==factor_questions[i])
  
      print(table(df_full[,colnum]))
    }
}


#save(df_full, file = "NHANESCleanFactors.RData")
