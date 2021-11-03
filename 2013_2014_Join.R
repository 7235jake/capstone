library(nhanesA)
library(tibble)

#------------ Create translation function -----------
translate <- function(cols){
  table_names <- c("DIQ_H", "HDL_H","TRIGLY_H", "TCHOL_H", "GLU_H", "HSQ_H", "DBQ_H",  
                   "MCQ_H", "PAQ_H", "PFQ_H", "WHQ_H", 
                   "BMX_H", "BPQ_H", "DEMO_H")
  trans_table <- list()
  for (i in seq_along(table_names)){
    if(length(nhanesTranslate(table_names[i], colnames = cols)) > 0){
      return((nhanesTranslate(table_names[i], colnames = cols)))
    }
  }
  return (0)
  
}

# Diabetes
dia_13_14 <- nhanes("DIQ_H")
hdl_13_14 <- nhanes("HDL_H")
tri_13_14 <- nhanes("TRIGLY_H")
chol_13_14 <- nhanes("TCHOL_H")
glu_13_14 <- nhanes("GLU_H")

# Obesity
hsq_13_14 <- nhanes("HSQ_H")
dbq_13_14 <- nhanes("DBQ_H")
mcq_13_14 <- nhanes("MCQ_H")
paq_13_14 <- nhanes("PAQ_H")
pfq_13_14 <- nhanes("PFQ_H")
whq_13_14 <- nhanes("WHQ_H")

# Body Measures
bmx_13_14 <- nhanes("BMX_H")

# Hypertension
bpq_13_14 <- nhanes("BPQ_H")

# Demographic
demo_13_14 <- nhanes("DEMO_H")

#--------------------Merge by SEQN-------------------------------------

df_full1314 <- merge(x = dia_13_14, y = hdl_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = tri_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = chol_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = glu_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = hsq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = dbq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = mcq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = paq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = pfq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = whq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = bmx_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = bpq_13_14, by = "SEQN", all = TRUE)
df_full1314 <- merge(x = df_full1314, y = demo_13_14, by = "SEQN", all = TRUE)

col_headers <- colnames(df_full1314)

# ----------- Clean Header names --------------
for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
    
    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
    
  }
}

col_headers <- colnames(df_full1314)

#------- Run script to determine which questions need responses mapped -------------------
#response_values = list()

# Go through all columns in dataframe
#for (i in seq_along(colnames(df_full1314))) {
# #Translate call for SEQN returns an error so skip over
# if(colnames(df_full1314[i])=='SEQN'){
#   i = i+1
# }
# Grab translation of column
# tran <- translate(colnames(df_full1314[i]))

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
  col_num <- which(colnames(df_full1314) == questions[i])
  
  for (row in seq_along(df_full1314[,col_num])){
    
    if( df_full1314[row,col_num] != 1 && df_full1314[row,col_num] != 2 && !is.na(df_full1314[row,col_num])){
      df_full1314[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
#for (i in seq(questions)){
#  print(questions[i])
#  print(table(df_full1314[c(questions[i])]))
#}



# --------- Check if questions don't exist in 2017-2018 but in 15-16 ---------

load("NHANES_Clean_2017_2018.RData")
list_of_qs17 <- names(df_full)
list_of_qs17

notin17 <- list()

col_head <- names(df_full1314)

notin17 <- col_head[which(!(col_head %in% list_of_qs17))]
notin17

# --------- Deleting all necessary columns ---------
colsToDel <- notin17[c(2, 24:53, 62:145, 147)]

df_full1314 <- df_full1314[-which(names(df_full1314) %in% colsToDel)]

# --------- Changing all necessary columns ---------

colsToChange = notin17[-c(2, 24:53, 62:145, 147)]

whatToChangeTo = c("WTSAF2YR", "CBQ506", "CBQ536", "CBQ541", "CBQ546", "CBQ551", 
                   "CBQ553", "CBQ581", "CBQ586", "CBQ591", "MCD180A", "MCD180N",
                   "MCD180B", "MCD180C", "MCD180D", "MCD180E", "MCD180F", "MCD180G",
                   "MCD180M", "MCD180K", "MCD180L", "MCD240A", "MCQ366A", "MCQ366B",   
                   "MCQ366C", "MCQ366D", "MCQ371A", "MCQ371B", "MCQ371C", "MCQ371D",
                   "DMDHRAGZ", "DMDHREDZ", "DMDHRMAZ", "DMDHSEDZ")

count = 1
for(i in 1:length(df_full1314))
{
  if(names(df_full1314[i]) %in% colsToChange)
  {
    colnames(df_full1314)[i] <- whatToChangeTo[count]  
    count <- count + 1
  }
}

df_full1314["Year"] <- rep("2013-2014", length(df_full1314[1]))

save(df_full1314, file = "NHANES_Clean_2013_2014.RData")



