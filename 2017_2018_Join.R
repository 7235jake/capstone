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


for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
        
        col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
        
  }
  # Set last character to lowercase if is a letter and letter before is number
  #strtoi is string to integer
  if (is.na ( strtoi(  substring( col_headers[i], nchar( col_headers[i]) ) ) )  
      && !is.na( strtoi( substring( col_headers[i], nchar( col_headers[i]) -1, nchar(col_headers[i]) -1) ) ) ){
        col_headers[i] <- paste(substring(col_headers[i], 1, nchar(col_headers[i])-1),
                          tolower(substring(col_headers[i], nchar(col_headers[i]))), sep = "")
  }
}


colnames(df_full) <- col_headers

colnames(df_full)
