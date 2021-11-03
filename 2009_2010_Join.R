library(nhanesA)
library(tibble)
library(tidyverse)

load("C:/Users/laure/git/capstone/NHANES_Clean_2017_2018.RData")

#------------ Create translation function -----------
translate <- function(cols){
  table_names <- c("DIQ_F", "HDL_F","TRIGLY_F", "TCHOL_F", "GLU_F", 
                   "CBQ_F", "CBQPFA_F", "CBQPFC_F", "HSQ_F", "DBQ_F",  
                   "MCQ_F", "PAQ_F", "PFQ_F", "WHQ_F", 
                   "BMX_F", "BPQ_F", "DEMO_F")
  trans_table <- list()
  for (i in seq_along(table_names)){
    if(length(nhanesTranslate(table_names[i], colnames = cols)) > 0){
      return((nhanesTranslate(table_names[i], colnames = cols)))
    }
  }
  return (0)
  
}

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




# ------------------- Join all together -------------------------------
df_full2009 <- merge(x = dia_09_10, y = hdl_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = tri_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = chol_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = glu_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_adult_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = cbq_child_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = hsq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = dbq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = mcq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = paq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = pfq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = whq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = bmx_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = bpq_09_10, by = "SEQN", all = TRUE)
df_full2009 <- merge(x = df_full2009, y = demo_09_10, by = "SEQN", all = TRUE)

col_names2017 <- names(df_full)
col_headers <- colnames(df_full2009)

# ----------- Clean Header names --------------
for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){
    
    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)
    
  }
}

colnames(df_full2009) <- col_headers

#------- Run script to determine which questions need responses mapped -------------------
response_values2009 = list()
 
# Go through all columns in dataframe
for (i in seq_along(colnames(df_full2009))) {
#   #Translate call for SEQN returns an error so skip over
   if(colnames(df_full2009[i])=='SEQN'){
     i = i+1
   }
   # Grab translation of column
   tran <- translate(colnames(df_full2009[i]))
   
   #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
   responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
   if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
       ('No' %in% responses && !('Range of Values' %in% responses))){
     response_values2009 <- append(response_values2009,tran)
   }
}

save(response_values2009, file = "ResponseCleaningIdentifiers2009.RData")

#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values2009)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_full2009) == questions[i])
  
  for (row in seq_along(df_full2009[,col_num])){
    
    if( df_full2009[row,col_num] != 1 && df_full2009[row,col_num] != 2 && !is.na(df_full2009[row,col_num])){
      df_full2009[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
for (i in seq(questions)){
  print(questions[i])
  print(table(df_full2009[c(questions[i])]))
}


# --------- Check if questions don't exist in 2017-2018 but in 15-16 ---------

list_of_qs17 <- names(df_full)

cols2009 <- names(df_full2009)

notin17 <- cols2009[which(!(cols2009 %in% list_of_qs17))]

# --------- Deleting all necessary columns ---------

delete_cols <- c("PHAFSTHR", 
                 "PHAFSTMN", 
                 "CBD010",
                 "CBQ020",  
                 "CBQ030",  
                 "CBQ040",
                 "CBQ050" ,
                 "CBQ060",
                 "CBD070",
                 "CBD090",
                 "CBQ140",
                 "CBD150",
                 "CBD160",
                 "CBD170",
                 "CBD180",
                 "CBQ190",
                 "CBQ515",
                 "CBQ520",
                 "CBQ525", 
                 "CBQ530",
                 "CBQ550",
                 "CBQ555",
                 "CBQ560",
                 "CBQ565",
                 "CBQ570",
                 "CBQ575",
                 "CBQ580",
                 "CBQ585",
                 "CBQ590",
                 #"CBQ595",
                 "CBQ600",
                 "CBD620",
                 "CBD625",
                 "CBD630",
                 "CBD635",
                 "CBD637",
                 "CBD640",
                 "DBQ890",
                 "CBQ655",
                 "CBQ660",
                 "CBQ665",
                 "CBQ670",
                 "CBQ675",
                 "CBQ680",
                 "CBD710",
                 "CBD715",
                 "CBD720",
                 "CBD725",
                 "CBD730",
                 "CBD735",
                 "CBD740",
                 "CBQ790",
                 "CBQ795",
                 "CBQ800",
                 "CBQ805",
                 "CBQ810",
                 "CBQ520",
                 "CBQ525",
                 "CBQ530",
                 "CBQ555",
                 "CBQ560",
                 "CBQ565",
                 "CBQ570",
                 "CBQ575",
                 "CBQ580",
                 "CBQ585",
                 "CBQ590",
                 "CBQ600",
                 #"CBQ610",
                 "DBQ890",
                 "CBQ660",
                 "CBQ665",
                 "CBQ670",
                 "CBQ675",
                 "CBQ680",
                 "CBD710",
                 "CBD715",
                 "CBD720",
                 "CBD725",
                 "CBD730",
                 "CBD735",
                 "CBD740",
                 "CBQ790",
                 "CBQ795",
                 "CBQ800",
                 "CBQ805",
                 "CBQ810",
                 "CBQ815",
                 "CBQ820",
                 "CBQ825",
                 "HSQ490",
                 "HSQ493",
                 "HSQ470",
                 "HSQ480",  
                 "HSQ496",
                 "CBQ550",
                 "DBQ915",
                 "DBQ920",
                 "DBQ925A",
                 "DBQ925B",
                 "DBQ925C",
                 "DBQ925D",
                 "DBQ925E",
                 "DBQ925F",
                 "DBQ925G",
                 "DBQ925H",
                 "DBQ925I",
                 "DBQ925J",
                 "PAQ706",
                 "PAD590",
                 "PAD600","PAAQUEX",  "WHD045",   "WHQ270" ,  "WHQ280A"  ,"WHQ280B" , "WHQ280C" 
                 ,"WHQ280D" , "WHQ280E" , "WHQ090"  , "WHD100A" , "WHD100B" , "WHD100C" ,
                 "WHD100D" , "WHD100E",  "WHD100F" , "WHD100G",  "WHD100H" , "WHD100I", 
                 "WHD100J" , "WHD100K" , "WHD100L" , "WHD100M"  ,"WHD100N" , "WHD100O", 
                 "WHD100P" , "WHD100Q" , "WHD100R" , "WHD100S" , "WHD100T"  ,"WHQ210"  ,
                 "WHD220",
                 "BMXTRI",
                 "BMXSUB",
                 "BMISUB" ,
                 "BPQ057" ,
                 "BPQ056" ,  "BPD058" ,  "BPQ059" ,  "BPQ090A",  "BPQ090B" , "BPQ090C" 
                 ,"BPQ100A" , "BPQ100B",  "BPQ100C"
                 ,"DMDSCHOL",
                 "CBQ515", "MCQ070", "MCQ082", "MCQ086", "MCQ140",
                 "MCQ240BB", "MCQ240AA", "MCQ240CC","MCQ240DD", "MCQ240DK",
                 "MCD240D",
                 "MCQ240E", 
                 "MCQ240F", 
                 "MCQ240G",
                 "MCQ240H", 
                 "MCQ240I", 
                 "MCQ240J", 
                 "MCQ240K", 
                 "MCQ240L",
                 "MCQ240M",
                 "MCQ240N",
                 "MCQ240O", 
                 "MCQ240P", 
                 "MCQ240Q", 
                 "MCQ240R", 
                 "MCQ240S", 
                 "MCQ240T", 
                 "MCQ240U", 
                 "MCQ240V", 
                 "MCQ240W", 
                 "MCQ240X",
                 "MCQ240Y",  
                 "MCQ240Z",
                 "DMDHRBR2",
                 "DMDSCHOL")
   
df_full2009deleted <- df_full2009[-which(names(df_full2009) %in% delete_cols)]

# -------------Keep / Change------------------------

df_full2009deleted <- df_full2009deleted %>%
  rename(
    CBD111 = CBD110,
    CBD121 = CBD120,
    CBD131 = CBD130,     
    CBQ506 = CBQ505,
    CBQ535 = CBQ535,
    CBQ541 = CBQ540,
    CBQ546 = CBQ545, 
    CBQ606 = CBQ605,
    CBQ611 = CBQ610,   
    CBQ511 = CBQ510,
    CBQ536 = CBQ535,
    CBQ541 = CBQ540,
    CBQ945 = CBQ545,
    CBQ596 = CBQ595,   
    AGQ030 = MCQ051,
    MCD180A = MCQ180A,
    MCQ195 = MCQ191,
    MCD180N = MCQ180N,
    MCD180B = MCQ180B,
    MCD180C = MCQ180C,
    MCD180D = MCQ180D,
    MCD180E = MCQ180E,
    MCD180F = MCQ180F,
    MCD180G = MCQ180G,
    MCD180M = MCQ180M,
    MCD180K = MCQ180K,
    MCD180L = MCQ180L,
    MCD240A = MCQ240A, 
    MCD240B = MCQ240B, 
    MCD240C = MCQ240C,
    RIDEXAGM =  RIDAGEEX,
    DMQMILIZ = DMQMILIT,
    DMDBORN4 = DMDBORN2,
    DMDHRAGZ = DMDHRAGE,
    DMDHREDZ = DMDHREDU,
    DMDHRMAZ = DMDHRMAR,
    DMDHSEDZ = DMDHSEDU,
    AIALANGA = AIALANG, 
  )

# Check 2
cols2009 <- names(df_full2009deleted)
notin17pt2 <- cols2009[which(!(cols2009 %in% list_of_qs17))]

df_full2009deleted <- df_full2009deleted[-which(names(df_full2009deleted) %in% notin17pt2)]


df_full2009 <- df_full2009deleted

save(df_full2009, file = "NHANES_Clean_2009_2010.RData")




