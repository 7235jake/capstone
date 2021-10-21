library(nhanesA)

# Diabetes
dia_09_10 <- nhanes("DIQ_F")
hdl_09_10 <- nhanes("HDL_F")
tri_09_10 <- nhanes("TRIGLY_F")
chol_09_10 <- nhanes("TCHOL_F")
glu_09_10 <- nhanes("GLU_F")

# Obesity
cb_09_10 <- nhanes("CBQPFC_F")
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


# Cleaning Diabetes Dataset
#dia_09_10$DIQ172 <- c()

# ---------------Join all together-------------------------------------
df_full <- merge(x = dia_09_10, y = hdl_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = tri_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = chol_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = glu_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = cb_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = hsq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = dbq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = mcq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = paq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = pfq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = whq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bmx_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bpq_09_10, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = demo_09_10, by = "SEQN", all = TRUE)
