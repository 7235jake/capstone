library(nhanesA)
library(tibble)

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

# -------------------Cleaning Diabetes Dataset-------------------------
# --Clean Variables--
# -------------------- Obesity --------------------------



#--------------------Merge by SEQN-------------------------------------
df_full <- merge(x = df_full, y = cb_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = hsq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = dbq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = mcq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = paq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = pfq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = whq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bmx_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bpq_15_16, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = demo_15_16, by = "SEQN", all = TRUE)