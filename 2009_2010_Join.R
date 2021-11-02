library(nhanesA)
library(tibble)
library(tidyverse)

#------------ Create translation function -----------
translate <- function(cols){
  table_names <- c("DIQ_F", "HDL_F","TRIGLY_F", "TCHOL_F", "GLU_F", 
                   "CBQ_F", "CBQPFA_F", "CBQPFC_F", "HSQ_I", "DBQ_I",  
                   "MCQ_I", "PAQ_I", "PFQ_I", "WHQ_I", 
                   "BMX_I", "BPQ_I", "DEMO_I")
  trans_table <- list()
  for (i in seq_along(table_names)){
    if(length(nhanesTranslate(table_names[i], colnames = cols)) > 0){
      return((nhanesTranslate(table_names[i], colnames = cols)))
    }
  }
  return (0)
  
}

# Diabetes
dia_09_10 <- nhanes("DIQ_F")
hdl_09_10 <- nhanes("HDL_F")
tri_09_10 <- nhanes("TRIGLY_F")
chol_09_10 <- nhanes("TCHOL_F")
glu_09_10 <- nhanes("GLU_F")

# Obesity
cbq_09_10 <- nhanes("CBQ_F")
cbq_adult_09_10 <- nhanes("CBQPFA_F")
cbq_child_09_10 <- nhanes("CBQPFC_F")
hsq_09_10 <- nhanes("HSQ_F")
dbq_09_10 <- nhanes("DBQ_F")
mcq_09_10 <- nhanes("MCQ_F")
paq_09_10 <- nhanes("PAQ_F")
pfq_09_10 <- nhanes("PFQ_F")
whq_09_10 <- nhanes("WHQ_F")

# Body Measures
bmx_09_10 <- nhanes("BMX_F")

# Hypertension
bpq_09_10 <- nhanes("BPQ_F")

# Demographic
demo_09_10 <- nhanes("DEMO_F")

# ------------------- Join all together -------------------------------
df_full2009 <- merge(x = dia_09_10, y = hdl_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = tri_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = chol_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = glu_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_adult_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_child_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = hsq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = dbq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = mcq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = paq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = pfq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = whq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = bmx_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = bpq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = demo_09_10, by = "SEQN", all = TRUE)


col_headers <- colnames(df_full2009)

# ----------- Clean Header names --------------
for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
    
    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
    
  }
}


#------- Run script to determine which questions need responses mapped -------------------
response_values2009 = list()
 
# Go through all columns in dataframe
for (i in seq_along(colnames(df_full2009))) {
#   #Translate call for SEQN returns an error so skip over
   if(colnames(df_full2009[i])=='SEQN'){
     i = i+1
   }
   # Grab translation of column
   tran <- translate(colnames(df_full2009[i]))
   
   #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
   responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
   if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
       ('No' %in% responses && !('Range of Values' %in% responses))){
     response_values2009 <- append(response_values2009,tran)
   }
}

save(response_values2009, file = "ResponseCleaningIdentifiers2009.RData")

#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values2009)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_full2009) == questions[i])
  
  for (row in seq_along(df_full2009[,col_num])){
    
    if( df_full2009[row,col_num] != 1 && df_full2009[row,col_num] != 2 && !is.na(df_full2009[row,col_num])){
      df_full2009[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
for (i in seq(questions)){
  print(questions[i])
  print(table(df_full2009[c(questions[i])]))
}




# ----------- Delete columns and Change Names

col_names2017 <- names(df_full)
remove_cols <- c()

not_in2017 <- col_headers[which(!(col_headers%in%col_names2017))]

changed_names <- c()

# for each value in 2009 dataset
for (i in 1:length(not_in2017)){
  # if the column name is the same as a certain value in 
  sub2009 <- substr(not_in2017[i], 1, nchar(not_in2017[i]) - 1)
  for (j in (i + 1):length(col_names2017)){
    sub2017 <- substr(col_names2017[j], 1, nchar(col_names2017[j]) - 1)
    if (sub2009 == sub2017){
      # IF the last value is numeric and the subtraction == 1
      num2009 <- as.numeric(substr(not_in2017[i], nchar(not_in2017[i]), nchar(not_in2017[i])))
      num2017 <- as.numeric(substr(col_names2017[j], nchar(col_names2017[j]), nchar(col_names2017[j])))
      subtract <- num2017 - num2009
      if (subtract == 1){
        # rename the value
        names(df_full2009)[i] <- col_names2017[j]
        changed_names <- append(changed_names, col_names2017[j])
      }
    }
  }
}

colnames(df_full2009) <- col_headers
not_in2017again <- col_headers[which(!(col_headers%in%col_names2017))]

df_full2009 <- subset(df_full2009, select = -(not_in2017again))
df_full2009 <- df_full2009[ , -which(names(df_full2009) %in% not_in2017again)]

#   else, check if the value is in the column



# -- Delete some columns from the insulin dataset. 


# Rename
df_full2009 %>% 
  rename(
    CBQ505 = CBQ506,
    CBQ535 = CBQ536,
    CBQ540 = CBQ541,
    CBQ550 = CBQ551
  )


# Remove Columns
# "HSQ480"  "HSQ490"  "HSQ493" "HSQ496"
df_full2009 <- subset(df_full2009, select = -c(HSQ480, HSQ490 , HSQ493, HSQ496))


