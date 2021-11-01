library(nhanesA)
library(tibble)

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

# -------------------Cleaning Diabetes Dataset-------------------------
# --Clean Variables--
# -------------------- Obesity --------------------------




#--------------------Merge by SEQN-------------------------------------
df_full <- merge(x = df_full, y = cb_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = hsq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = dbq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = mcq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = paq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = pfq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = whq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bmx_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = bpq_13_14, by = "SEQN", all = TRUE)
df_full <- merge(x = df_full, y = demo_13_14, by = "SEQN", all = TRUE)