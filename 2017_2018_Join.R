library(nhanesA)


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




# Diabetes
dia_17_18 <- nhanes("DIQ_J")
hdl_17_18 <- nhanes("HDL_J")
tri_17_18 <- nhanes("TRIGLY_J")
chol_17_18 <- nhanes("TCHOL_J")
glu_17_18 <- nhanes("GLU_J")
ins_17_18 <- nhanes("INS_J")



# Obesity
cb_17_18 <- nhanes("CBQPFC_J")
hsq_17_18 <- nhanes("HSQ_J")
dbq_17_18 <- nhanes("DBQ_J")
mcq_17_18 <- nhanes("MCQ_J")
paq_17_18 <- nhanes("PAQ_J")
pfq_17_18 <- nhanes("PFQ_J")
whq_17_18 <- nhanes("WHQ_J")
cbq_17_18 <- nhanes("CBQ_J")
cbqpfa_17_18 <- nhanes("CBQPFA_J")
mcq_17_18 <- nhanes("MCQ_J")

# Body Measures
bmx_17_18 <- nhanes("BMX_J")

# Hypertension
bpq_17_18 <- nhanes("BPQ_J")

# Demographic
demo_17_18 <- nhanes("DEMO_J")

# ---------------Join all together-------------------------------------
df_full <- merge(x = dia_17_18, y = hdl_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = tri_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = chol_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = glu_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = ins_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = cb_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = hsq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = dbq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = mcq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = paq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = pfq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = whq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = cbq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = cbqpfa_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bmx_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bpq_17_18, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = demo_17_18, by = "SEQN", all = TRUE)

col_headers <- colnames(df_full)

# ----------- Clean Header names --------------
for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
    
    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
    
  }
}


colnames(df_full) <- col_headers



#------- Run script to determine which questions need responses mapped -------------------
# response_values = list()
# 
# # Go through all columns in dataframe
# for (i in seq_along(colnames(df_full))) {
#   #Translate call for SEQN returns an error so skip over
#   if(colnames(df_full[i])=='SEQN'){
#     i = i+1
#   }
#   # Grab translation of column
#   tran <- translate(colnames(df_full[i]))
#   
#   #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
#   responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
#   if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
#       ('No' %in% responses && !('Range of Values' %in% responses))){
#     response_values <- append(response_values,tran)
#   }
# }

#save(response_values, file = "ResponseCleaningIdentifiers.RData")


#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_full) == questions[i])
  
  for (row in seq_along(df_full[,col_num])){
    
    if( df_full[row,col_num] != 1 && df_full[row,col_num] != 2 && !is.na(df_full[row,col_num])){
      df_full[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
for (i in seq(questions)){
  print(questions[i])
  print(table(df_full[c(questions[i])]))
}
