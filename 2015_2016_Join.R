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
#response_values = list()
 
# Go through all columns in dataframe
#for (i in seq_along(colnames(df_full1516))) {
# #Translate call for SEQN returns an error so skip over
# if(colnames(df_full1516[i])=='SEQN'){
#   i = i+1
# }
 # Grab translation of column
# tran <- translate(colnames(df_full1516[i]))
   
 #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
# responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
# if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
#    ('No' %in% responses && !('Range of Values' %in% responses))){
#   response_values <- append(response_values,tran)
# }
#}

#save(response_values, file = "ResponseCleaningIdentifiers.RData")

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



# --------- Check if questions don't exist in 2017-2018 but in 15-16 ---------

load("NHANES_Clean_2017_2018.RData")
list_of_qs17 <- names(df_full)
list_of_qs17

notin17 <- list()

col_head <- names(df_full1516)

notin17 <- col_head[which(!(col_head %in% list_of_qs17))]
notin17

# --------- Deleting all necessary columns ---------
colsToDel <- notin17[c(2, 24:53, 62:145, 147)]

df_full1516 <- df_full1516[-which(names(df_full1516) %in% colsToDel)]

# --------- Changing all necessary columns ---------

colsToChange = notin17[-c(2, 24:53, 62:145, 147)]

whatToChangeTo = c("WTSAF2YR", "CBQ506", "CBQ536", "CBQ541", "CBQ546", "CBQ551", 
                   "CBQ553", "CBQ581", "CBQ586", "CBQ591", "MCD180A", "MCD180N",
                   "MCD180B", "MCD180C", "MCD180D", "MCD180E", "MCD180F", "MCD180G",
                   "MCD180M", "MCD180K", "MCD180L", "MCD240A", "MCQ366A", "MCQ366B",   
                   "MCQ366C", "MCQ366D", "MCQ371A", "MCQ371B", "MCQ371C", "MCQ371D",
                   "DMDHRAGZ", "DMDHREDZ", "DMDHRMAZ", "DMDHSEDZ")

count = 1
for(i in 1:length(df_full1516))
{
  if(names(df_full1516[i]) %in% colsToChange)
  {
    colnames(df_full1516)[i] <- whatToChangeTo[count]  
    count <- count + 1
  }
}


save(df_full1516, file = "NHANES_Clean_2015_2016.RData")


