library(nhanesA)

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
df_full <- merge(x = df_full, y = mcq_17_18, by = "SEQN", all = TRUE)
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
  # Set last character to lowercase if is a letter and letter before is number
  #strtoi is string to integer
  # if (is.na ( strtoi(  substring( col_headers[i], nchar( col_headers[i]) ) ) )  
  #     && !is.na( strtoi( substring( col_headers[i], nchar( col_headers[i]) -1, nchar(col_headers[i]) -1) ) ) ){
  #       col_headers[i] <- paste(substring(col_headers[i], 1, nchar(col_headers[i])-1),
  #                         tolower(substring(col_headers[i], nchar(col_headers[i]))), sep = "")
  #}
}
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


colnames(df_full) <- col_headers


#frame <- (nhanesTranslate("DIQ_J", colnames = c("DIQ010","DID040")))
#data.frame(frame[1])[,2]
#names(frame)[1]

response_values = list()
a <- translate("MCQ371A")
#question_var <- list()
for (i in 501:length(colnames(df_full))) {
  if(colnames(df_full[i])=='SEQN'){
    i = i+1
  }
  tran <- translate(colnames(df_full[i]))
  id <- names(tran)[1]
  
  responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
  if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
      ('No' %in% responses && !('Range of Values' %in% responses))){
    #question_var <- append(question_var,id)
    response_values <- append(response_values,tran)
  }
}

#save(response_values, file = "ResponseCleaningIdentifiers.RData")


colnames(df_full)
