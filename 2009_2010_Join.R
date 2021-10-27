library(nhanesA)
library(tibble)
library(dplyr)

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


# -------------------Cleaning Diabetes Dataset-------------------------

#--Adding Columns--
# Adding "Feel could be at risk for diabetes" 
dia_09_10 <- add_column(dia_09_10, DIQ172 = NA, .after = "DIQ170")

# Adding "Why you think you are  at risk for diabetes or prediabetes?"
dia_09_10 <- add_column(dia_09_10, DIQ175A = NA, .after = "DIQ172")
dia_09_10 <- add_column(dia_09_10, DIQ175B = NA, .after = "DIQ175A")
dia_09_10 <- add_column(dia_09_10, DIQ175C = NA, .after = "DIQ175B")
dia_09_10 <- add_column(dia_09_10, DIQ175D = NA, .after = "DIQ175C")
dia_09_10 <- add_column(dia_09_10, DIQ175E = NA, .after = "DIQ175D")
dia_09_10 <- add_column(dia_09_10, DIQ175F = NA, .after = "DIQ175E")
dia_09_10 <- add_column(dia_09_10, DIQ175G = NA, .after = "DIQ175F")
dia_09_10 <- add_column(dia_09_10, DIQ175H = NA, .after = "DIQ175G")
dia_09_10 <- add_column(dia_09_10, DIQ175I = NA, .after = "DIQ175H")
dia_09_10 <- add_column(dia_09_10, DIQ175J = NA, .after = "DIQ175I")
dia_09_10 <- add_column(dia_09_10, DIQ175K = NA, .after = "DIQ175J")
dia_09_10 <- add_column(dia_09_10, DIQ175L = NA, .after = "DIQ175K")
dia_09_10 <- add_column(dia_09_10, DIQ175M = NA, .after = "DIQ175L")
dia_09_10 <- add_column(dia_09_10, DIQ175N = NA, .after = "DIQ175M")
dia_09_10 <- add_column(dia_09_10, DIQ175O = NA, .after = "DIQ175N")
dia_09_10 <- add_column(dia_09_10, DIQ175P = NA, .after = "DIQ175O")
dia_09_10 <- add_column(dia_09_10, DIQ175Q = NA, .after = "DIQ175P")
dia_09_10 <- add_column(dia_09_10, DIQ175R = NA, .after = "DIQ175Q")
dia_09_10 <- add_column(dia_09_10, DIQ175S = NA, .after = "DIQ175R")
dia_09_10 <- add_column(dia_09_10, DIQ175T = NA, .after = "DIQ175S")
dia_09_10 <- add_column(dia_09_10, DIQ175U = NA, .after = "DIQ175T")
dia_09_10 <- add_column(dia_09_10, DIQ175V = NA, .after = "DIQ175U")
dia_09_10 <- add_column(dia_09_10, DIQ175W = NA, .after = "DIQ175V")
dia_09_10 <- add_column(dia_09_10, DIQ175X = NA, .after = "DIQ175W")

# Adding Past year Dr checked for A1C
dia_09_10 <- add_column(dia_09_10, DIQ275 = NA, .after = "DIQ260U")
# Adding What was your last A1C level
dia_09_10 <- add_column(dia_09_10, DIQ280 = NA, .after = "DIQ275")
# Adding What does Dr say A1C should be
dia_09_10 <- add_column(dia_09_10, DIQ291 = NA, .after = "DIQ280")
# Adding What was your recent SBP
dia_09_10 <- add_column(dia_09_10, DIQ300S = NA, .after = "DIQ291")
# Adding What was your recent DBP
dia_09_10 <- add_column(dia_09_10, DIQ300D = NA, .after = "DIQ300S")
# Adding What does Dr say SBP should be
dia_09_10 <- add_column(dia_09_10, DID310S = NA, .after = "DIQ300D")
# Adding What does Dr say DBP should be
dia_09_10 <- add_column(dia_09_10, DID310D = NA, .after = "DID310S")
# Adding What was most recent LDL number
dia_09_10 <- add_column(dia_09_10, DID320 = NA, .after = "DID310D")
# Adding What does Dr say LDL should be
dia_09_10 <- add_column(dia_09_10, DID330 = NA, .after = "DID320")



# --Clean Variables--

# Changing to 1 or 2
dia_09_10$DIQ010[dia_09_10$DIQ010 != 1] <- 2
# If above 80 -> Missing
dia_09_10$DID040[dia_09_10$DID040 > 80] <- NA
# If not 1 change to 2
dia_09_10$DIQ160[dia_09_10$DIQ160 == 7 | dia_09_10$DIQ160 == 9] <- 2
# If not 1 change to 2
dia_09_10$DIQ170[dia_09_10$DIQ170 == 7 | dia_09_10$DIQ170 == 9] <- 2
# If not 1 change to 2
dia_09_10$DIQ180[dia_09_10$DIQ180 == 7 | dia_09_10$DIQ180 == 9] <- 2
# If not 1 change to 2
dia_09_10$DIQ050[dia_09_10$DIQ050 == 7 | dia_09_10$DIQ050 == 9] <- 2
# change values > 666 to NA
dia_09_10$DID060[dia_09_10$DID060 > 666] <- NA
# If not 1 change to 2
dia_09_10$DIQ070[dia_09_10$DIQ070 == 7 | dia_09_10$DIQ070 == 9] <- 2
# change values > 600 to NA
dia_09_10$DID260[dia_09_10$DID260 > 600] <- NA
# Change values = 999 to zero
dia_09_10$DID341[dia_09_10$DID341 == 9999] <- 0
dia_09_10$DID350[dia_09_10$DID350 == 9999] <- 0
# If not 1 change to 2
dia_09_10$DIQ080[dia_09_10$DIQ080 == 7 | dia_09_10$DIQ080 == 9] <- 2



# --Join Cholesterol - High - Density Lipoprotein (HDL)-
diabetes <- merge(x = dia_09_10, y = hdl_09_10, by = "SEQN", all = TRUE)

# --Join Cholesterol - Low-Density Lipoproteins (LDL) & Triglycerides---
diabetes <- merge(x = diabetes, y = tri_09_10, by = "SEQN", all = TRUE)

# --Join Cholesterol - Total--
diabetes <- merge(x = diabetes, y = chol_09_10, by = "SEQN", all = TRUE)

# --Join Insulin--
# -- Delete some columns from the insulin dataset. 
glu_09_10 <- subset(glu_09_10, select = -c(WTSAF2YR, LBXGLU, LBXIN, PHAFSTHR, PHAFSTMN))
glu_09_10 <- add_column(glu_09_10, LBDINLC = NA, .after = "LBDINSI")
diabetes <- merge(x = diabetes, y = glu_09_10, by = "SEQN", all = TRUE)


# -------------------- Obesity --------------------------

# -- Consumer Behavior -- 
cbq_09_10$CBD070[cbq_09_10$CBD070 >= 777777 ] <- NA
cbq_09_10$CBD091[cbq_09_10$CBD091 >= 777777 ] <- NA
cbq_09_10$CBD111[cbq_09_10$CBD111 >= 777777 ] <- NA
cbq_09_10$CBD121[cbq_09_10$CBD121 >= 777777 ] <- NA
cbq_09_10$CBD131[cbq_09_10$CBD131 >= 777777 ] <- NA

# -- Consumer Behavior Adult --
# Add Variables
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ581 = NA, .after = "CBQ550")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ586 = NA, .after = "CBQ581")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ830 = NA, .after = "CBQ586")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ835 = NA, .after = "CBQ830")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ840 = NA, .after = "CBQ835")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ845 = NA, .after = "CBQ840")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ850 = NA, .after = "CBQ845")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ855 = NA, .after = "CBQ850")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ860 = NA, .after = "CBQ855")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ865 = NA, .after = "CBQ860")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ870 = NA, .after = "CBQ865")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ875 = NA, .after = "CBQ870")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ880 = NA, .after = "CBQ875")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ885 = NA, .after = "CBQ880")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ890 = NA, .after = "CBQ885")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ895 = NA, .after = "CBQ890")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ900 = NA, .after = "CBQ895")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ645 = NA, .after = "CBQ900")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ700 = NA, .after = "CBQ645")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, DBQ780 = NA, .after = "CBQ700")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, DBQ750 = NA, .after = "DBQ780")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, DBQ760 = NA, .after = "DBQ750")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, DBQ770 = NA, .after = "DBQ760")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ905 = NA, .after = "DBQ770")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ910 = NA, .after = "CBQ905")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ685 = NA, .after = "DBQ910")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ915 = NA, .after = "DBQ685")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ925 = NA, .after = "CBQ915")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ930 = NA, .after = "CBQ925")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ930 = NA, .after = "CBQ925")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ935 = NA, .after = "CBQ930")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ945 = NA, .after = "CBQ935")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ950 = NA, .after = "CBQ945")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738a = NA, .after = "CBQ950")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738b = NA, .after = "CBQ738a")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738c = NA, .after = "CBQ738b")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738d = NA, .after = "CBQ738c")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738e = NA, .after = "CBQ738d")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738f = NA, .after = "CBQ738e")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738g = NA, .after = "CBQ738f")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738h = NA, .after = "CBQ738g")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738i = NA, .after = "CBQ738h")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738j = NA, .after = "CBQ738i")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738k = NA, .after = "CBQ738j")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ738cd = NA, .after = "CBQ738k")

cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698a = NA, .after = "CBQ738cd")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698b = NA, .after = "CBQ698a")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698c = NA, .after = "CBQ698b")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698d = NA, .after = "CBQ698c")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698e = NA, .after = "CBQ698d")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698f = NA, .after = "CBQ698e")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698g = NA, .after = "CBQ698f")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698h = NA, .after = "CBQ698g")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698i = NA, .after = "CBQ698h")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ698cd = NA, .after = "CBQ698i")


cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ695a = NA, .after = "CBQ698cd")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ695b = NA, .after = "CBQ695a")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ695c = NA, .after = "CBQ695b")

cbq_adult_09_10 <- add_column(cbq_adult_09_10, CBQ785 = NA, .after = "CBQ695c")
cbq_adult_09_10 <- add_column(cbq_adult_09_10, WTDRD1 = NA, .after = "CBQ785")




# Clean Variables
cbq_adult_09_10$CBQ502[cbq_adult_09_10$CBQ502 == 7 | cbq_adult_09_10$CBQ502 == 9] <- 2
cbq_adult_09_10$CBQ503[cbq_adult_09_10$CBQ503 == 7 | cbq_adult_09_10$CBQ503 == 9] <- 2
cbq_adult_09_10$CBQ505[cbq_adult_09_10$CBQ505 == 7 | cbq_adult_09_10$CBQ505 == 9] <- 2
cbq_adult_09_10$CBQ535[cbq_adult_09_10$CBQ535 == 7 | cbq_adult_09_10$CBQ535 == 9] <- 2
cbq_adult_09_10$CBQ540[cbq_adult_09_10$CBQ540 == 7 | cbq_adult_09_10$CBQ540 == 9] <- 2
cbq_adult_09_10$CBQ550[cbq_adult_09_10$CBQ550 == 7 | cbq_adult_09_10$CBQ550 == 9] <- 2


# Rename
cbq_adult_09_10 %>% 
  rename(
    CBQ505 = CBQ506,
    CBQ535 = CBQ536,
    CBQ540 = CBQ541,
    CBQ550 = CBQ551
  )

obesity <- merge(x = cbq_09_10, y = cbq_adult_09_10, by = "SEQN", all = TRUE)

# -- Consumer Behavior Child --
# Add Variables
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ536 = NA, .after = "CBQ506")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ541 = NA, .after = "CBQ536")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ551 = NA, .after = "CBQ541")

cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ581 = NA, .after = "CBQ550")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ586 = NA, .after = "CBQ581")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ830 = NA, .after = "CBQ586")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ835 = NA, .after = "CBQ830")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ840 = NA, .after = "CBQ835")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ845 = NA, .after = "CBQ840")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ850 = NA, .after = "CBQ845")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ855 = NA, .after = "CBQ850")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ860 = NA, .after = "CBQ855")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ865 = NA, .after = "CBQ860")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ870 = NA, .after = "CBQ865")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ875 = NA, .after = "CBQ870")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ880 = NA, .after = "CBQ875")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ885 = NA, .after = "CBQ880")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ890 = NA, .after = "CBQ885")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ895 = NA, .after = "CBQ890")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ900 = NA, .after = "CBQ895")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ700 = NA, .after = "CBQ645")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBQ780 = NA, .after = "CBQ700")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBQ750 = NA, .after = "DBQ780")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBQ760 = NA, .after = "DBQ750")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBQ770 = NA, .after = "DBQ760")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ905 = NA, .after = "DBQ770")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ910 = NA, .after = "CBQ905")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ685 = NA, .after = "DBQ910")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ915 = NA, .after = "DBQ685")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ925 = NA, .after = "CBQ915")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ930 = NA, .after = "CBQ925")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ935 = NA, .after = "CBQ930")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ945 = NA, .after = "CBQ935")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ950 = NA, .after = "CBQ945")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738a = NA, .after = "CBQ950")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738b = NA, .after = "CBQ738a")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738c = NA, .after = "CBQ738b")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738d = NA, .after = "CBQ738c")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738e = NA, .after = "CBQ738d")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738f = NA, .after = "CBQ738e")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738g = NA, .after = "CBQ738f")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738h = NA, .after = "CBQ738g")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738i = NA, .after = "CBQ738h")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738j = NA, .after = "CBQ738i")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738k = NA, .after = "CBQ738j")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ738cd = NA, .after = "CBQ738k")

cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698a = NA, .after = "CBQ738cd")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698b = NA, .after = "CBQ698a")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698c = NA, .after = "CBQ698b")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698d = NA, .after = "CBQ698c")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698e = NA, .after = "CBQ698d")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698f = NA, .after = "CBQ698e")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698g = NA, .after = "CBQ698f")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698h = NA, .after = "CBQ698g")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698i = NA, .after = "CBQ698h")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ698cd = NA, .after = "CBQ698i")


cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ695a = NA, .after = "CBQ698cd")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ695b = NA, .after = "CBQ695a")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ695c = NA, .after = "CBQ695b")

cbq_child_09_10 <- add_column(cbq_child_09_10, DBD930 = NA, .after = "CBQ695b")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBD935 = NA, .after = "CBQ695b")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBD940 = NA, .after = "CBQ695b")
cbq_child_09_10 <- add_column(cbq_child_09_10, DBD945 = NA, .after = "CBQ695b")

cbq_child_09_10 <- add_column(cbq_child_09_10, CBD760 = NA, .after = "CBQ695b")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBD765 = NA, .after = "CBQ695b")
cbq_child_09_10 <- add_column(cbq_child_09_10, CBD770 = NA, .after = "CBQ695b")

cbq_child_09_10 <- add_column(cbq_child_09_10, CBQ785 = NA, .after = "CBQ695c")
cbq_child_09_10 <- add_column(cbq_child_09_10, WTDRD1 = NA, .after = "CBQ785")



# Clean Variables
cbq_child_09_10$CBQ502[cbq_child_09_10$CBQ502 == 7 | cbq_child_09_10$CBQ502 == 9] <- 2
cbq_child_09_10$CBQ503[cbq_child_09_10$CBQ503 == 7 | cbq_child_09_10$CBQ503 == 9] <- 2
cbq_child_09_10$CBQ505[cbq_child_09_10$CBQ506 == 7 | cbq_child_09_10$CBQ506 == 9] <- 2


obesity <- merge(x = obesity, y = cbq_child_09_10, by = "SEQN", all = TRUE)

# -- Current Health Status -- 

# Clean 
hsq_09_10$HSD010[hsq_09_10$HSD010 > 5] <- NA
hsq_09_10$HSQ500[hsq_09_10$HSQ500 == 7 | hsq_09_10$HSQ500 == 9] <- 2
hsq_09_10$HSQ510[hsq_09_10$HSQ510 == 7 | hsq_09_10$HSQ510 == 9] <- 2
hsq_09_10$HSQ520[hsq_09_10$HSQ520 == 7 | hsq_09_10$HSQ520 == 9] <- 2
hsq_09_10$HSQ571[hsq_09_10$HSQ571 == 7 | hsq_09_10$HSQ571 == 9] <- 2
hsq_09_10$HSQ580[hsq_09_10$HSQ580 > 50] <- NA
hsq_09_10$HSQ590[hsq_09_10$HSQ590 == 7 | hsq_09_10$HSQ590 == 9] <- 2

# Remove Columns
# "HSQ480"  "HSQ490"  "HSQ493" "HSQ496"
hsq_09_10 <- subset(hsq_09_10, select = -c(HSQ480, HSQ490 , HSQ493, HSQ496))

#Merge 
obesity <- merge(x = obesity, y = hsq_09_10, by = "SEQN", all = TRUE)

# -- Diet Behavior & Nutrition --
dbq_09_10$DBQ010[dbq_09_10$DBQ010 == 7 | dbq_09_10$DBQ010 == 9] <- 2
dbq_09_10$DBD030[dbq_09_10$DBD030 >= 777777] <- NA
dbq_09_10$DBD041[dbq_09_10$DBD041 >= 777777] <- NA
dbq_09_10$DBD050[dbq_09_10$DBD050 >= 777777] <- NA
dbq_09_10$DBD055[dbq_09_10$DBD055 >= 777777] <- NA
dbq_09_10$DBD061[dbq_09_10$DBD061 >= 777777] <- NA
dbq_09_10$DBQ700[dbq_09_10$DBQ700 == 7 | dbq_09_10$DBQ700 == 9] <- NA
dbq_09_10$DBQ197[dbq_09_10$DBQ197 == 7 | dbq_09_10$DBQ197 == 9] <- NA


