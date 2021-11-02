library(nhanesA)
library(tibble)

#------------ Create translation function -----------
translate <- function(cols){
  table_names <- c("DIQ_I", "HDL_I","TRIGLY_I", "TCHOL_I", "GLU_I", "HSQ_I", "DBQ_I",  
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
dia_15_16 <- nhanes("DIQ_I")
hdl_15_16 <- nhanes("HDL_I")
tri_15_16 <- nhanes("TRIGLY_I")
chol_15_16 <- nhanes("TCHOL_I")
glu_15_16 <- nhanes("GLU_I")

# Obesity
hsq_15_16 <- nhanes("HSQ_I")
dbq_15_16 <- nhanes("DBQ_I")
mcq_15_16 <- nhanes("MCQ_I")
paq_15_16 <- nhanes("PAQ_I")
pfq_15_16 <- nhanes("PFQ_I")
whq_15_16 <- nhanes("WHQ_I")

# Body Measures
bmx_15_16 <- nhanes("BMX_I")

# Hypertension
bpq_15_16 <- nhanes("BPQ_I")

# Demographic
demo_15_16 <- nhanes("DEMO_I")


#--------------------Merge by SEQN-------------------------------------



df_full1516 <- merge(x = dia_15_16, y = hdl_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = tri_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = chol_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = glu_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = hsq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = dbq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = mcq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = paq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = pfq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = whq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = bmx_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = bpq_15_16, by = "SEQN", all = TRUE)
df_full1516 <- merge(x = df_full1516, y = demo_15_16, by = "SEQN", all = TRUE)

col_headers <- colnames(df_full1516)

# ----------- Clean Header names --------------
for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
    
    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
    
  }
}

col_headers <- colnames(df_full1516)

#------- Run script to determine which questions need responses mapped -------------------
response_values = list()
 
# Go through all columns in dataframe
for (i in seq_along(colnames(df_full1516))) {
 #Translate call for SEQN returns an error so skip over
 if(colnames(df_full1516[i])=='SEQN'){
   i = i+1
 }
 # Grab translation of column
 tran <- translate(colnames(df_full1516[i]))
   
 #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
 responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
 if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
    ('No' %in% responses && !('Range of Values' %in% responses))){
   response_values <- append(response_values,tran)
 }
}

save(response_values, file = "ResponseCleaningIdentifiers.RData")

#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_full1516) == questions[i])
  
  for (row in seq_along(df_full1516[,col_num])){
    
    if( df_full1516[row,col_num] != 1 && df_full1516[row,col_num] != 2 && !is.na(df_full1516[row,col_num])){
      df_full1516[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
#for (i in seq(questions)){
#  print(questions[i])
#  print(table(df_full1516[c(questions[i])]))
#}



# --------- Check if questions exist in 2017-2018 & print ---------

load("1718questions.RData")
list_of_qs <- x
list_of_qs

naquestions = list()

for (i in list_of_qs){
  
  if (!(colnames(df_full1516)[i] %in% list_of_qs)){
    naquestions <- append(naquestions, colnames(df_full1516[i]))
  }
}


naquestions
# --------- Change names of necessary columns ---------






newnaquestions <- naquestions #All questions after the name changes
# End up with a new list of naquestions (some may now exist in 2017-2018)




# --------- Now remove questions if they don't exist in 2017-2018 ---------
gonequestions = list()
for (i in seq(newnaquestions)){
  
  if (!(newnaquestions[i] %in% list_of_qs)){
    gonequestions <- append(gonequestions, newnaquestions[i])
  }
}

# remove all questions from full data set that still don't exist in 2017-2018
df_full1516 <- df_full1516[-gonequestions,]




