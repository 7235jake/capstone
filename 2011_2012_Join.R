library(nhanesA)
library(tidyverse)
library(tibble)

#------------ Create translation function -----------
translate <- function(cols){
  #table_names <- c("DIQ_J", "HDL_J","TRIGLY_J", "TCHOL_J", "GLU_J", "INS_J", "CBQPFC_J", "HSQ_J", "DBQ_J",  
#                "MCQ_J", "PAQ_J", "PFQ_J", "WHQ_J", "CBQ_J", 
#                   "CBQPFA_J", "MCQ_J", "BMX_J", "BPQ_J", "DEMO_J")
  table_names <- c("MCQ_J", "BMX_J", "BPQ_J", "DEMO_J",
                   "MCQ_G", "BMX_G", "BPQ_G", "DEMO_G")
  trans_table <- list()
  for (i in seq_along(table_names)){
    if(length(nhanesTranslate(table_names[i], colnames = cols)) > 0){
      return((nhanesTranslate(table_names[i], colnames = cols)))
    }
  }
  return (0)
  
}

# Diabetes
dia_11_12 <- nhanes("DIQ_G")
hdl_11_12 <- nhanes("HDL_G")
tri_11_12 <- nhanes("TRIGLY_G")
chol_11_12 <- nhanes("TCHOL_G")
glu_11_12 <- nhanes("GLU_G")

# Obesity
hsq_11_12 <- nhanes("HSQ_G")
dbq_11_12 <- nhanes("DBQ_G")
mcq_11_12 <- nhanes("MCQ_G")
paq_11_12 <- nhanes("PAQ_G")
pfq_11_12 <- nhanes("PFQ_G")
whq_11_12 <- nhanes("WHQ_G")
cbq_11_12 <- nhanes("CBQ_G")
whqmec_11_12 <- nhanes("WHQMEC_G")

# Body Measures
bmx_11_12 <- nhanes("BMX_G")

# Hypertension
bpq_11_12 <- nhanes("BPQ_G")

# Demographic
demo_11_12 <- nhanes("DEMO_G")

nhanesA::browseNHANES(nh_table = 'BMX_G')
nhanesA::browseNHANES(nh_table = "BMX_J")
library(Hmisc)
label(select(df_11_12,BMDAVSAD))


#--------------------Merge by SEQN-------------------------------------
rm(df_11_12)
df_11_12 <- merge(x = dia_11_12, y = hdl_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = tri_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = chol_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = glu_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = hsq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = dbq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = mcq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = paq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = pfq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = whq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = cbq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = whqmec_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = bmx_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = bpq_11_12, by = "SEQN", all = TRUE)
df_11_12 <- merge(x = df_11_12, y = demo_11_12, by = "SEQN", all = TRUE)

col_headers <- colnames(df_11_12)

col_headers

# ----------- Clean Header names --------------
df_11_12 <- select(df_11_12, -WTSAF2YR.x)

for (i in seq_along(col_headers)){
  # To handle column names ending in ".x" or ".y"
  if (substring(col_headers[i], nchar(col_headers[i])-1, nchar(col_headers[i])-1) == '.'){

    col_headers[i] <- substring(col_headers[i], 1, nchar(col_headers[i])-2)

  }
}

colnames(df_11_12) <- col_headers

#------- Run script to determine which questions need responses mapped -------------------
response_values = list()



#Go through all columns in dataframe
for (i in 293:length(colnames(df_11_12))) {
  #Translate call for SEQN returns an error so skip over
  if(colnames(df_11_12[i])=='SEQN'){
    i = i+1
  }
  #Grab translation of column
  tran <- translate(colnames(df_11_12[i]))
  
  #tryCatch to avoid questions that will throw errors in translation method (ex: skip item)
  responses <- tryCatch(data.frame(tran[1])[,2], error = function(e) {NULL})
  if (('Yes' %in% responses && !('Range of Values' %in% responses)) ||
      ('No' %in% responses && !('Range of Values' %in% responses))){
    response_values <- append(response_values,tran)
  }
}

#save(response_values, file = "ResponseCleaningIdentifiers.RData")

#---------- Change question responses ----------------
# Goal: Change all responses not 1 (Yes) or NA to be 2 (No)

#Store questions identifiers that need responses changed
questions <- names(response_values)

for (i in seq_along(questions)){
  #Determine column number corresponding to question identifier
  col_num <- which(colnames(df_11_12) == questions[i])
  
  for (row in seq_along(df_11_12[,col_num])){
    
    if( df_11_12[row,col_num] != 1 && df_11_12[row,col_num] != 2 && !is.na(df_11_12[row,col_num])){
      df_11_12[row,col_num] <- 2
    }
    
  }
}

#---------- Check if responses have changed ------------
for (i in seq(questions)){
 print(questions[i])
 print(table(df_11_12[c(questions[i])]))
}



# --------- Check if questions don't exist in 2017-2018 but in 11-12 ---------

load("NHANES_Clean_2017_2018.RData")
list_of_qs17 <- names(df_full)
list_of_qs17

notin17 <- list()


col_head <- names(df_11_12)

notin17 <- col_head[which(!(col_head %in% list_of_qs17))]
notin17

#-----------Check if not in 2017 questions exist in data dictionary
datadic_11_12 <- read_lines("~/Capstone/capstone/DD_2011_2012.csv")
datadic_11_12 <- toupper(datadic_11_12)
datadic_11_12 <- trimws(datadic_11_12, which = c("right"))

col_head[which(!(notin17 %in% datadic_11_12))]

tester = list()
for (i in seq_along(notin17)){
   for( x in seq_along(datadic_11_12)){
      if (notin17[i]==datadic_11_12[x]){
        tester <- append(tester, notin17[i])
      }
   } 
}

#df_11_12 <- select(df_11_12, -unlist(tester))
df_11_12 <- select(df_11_12, -c("MCQ051", "WHQ030M","WHQ500","WHQ520"))

notin17 <- notin17[! notin17 %in% unlist(tester)]
notin17 <- notin17[! notin17 %in% c("MCQ240A")]


sort(list_of_qs17)
sort(datadic_11_12)

sort(notin17)


sort(unlist(tester))

(df_11_12$MCQ240A)
#Change questions that exist in da
df_11_12 <- df_11_12 %>% rename(
  MCD240A = MCQ240A,
  CBD071 = CBD070,
  CBD091 = CBD090,
  CBD111 = CBD110,
  CBD131 = CBD130,
  DMDHRAGZ = DMDHRAGE,
  DMDHREDZ = DMDHREDU,
  DMDHRMAZ = DMDHRMAR,
  DMDHSEDZ = DMDHSEDU,
  MCD180A = MCQ180A,
  MCD180B = MCQ180B,
  MCD180C = MCQ180C,
  MCD180D = MCQ180D,
  MCD180E = MCQ180E,
  MCD180F = MCQ180F,
  MCD180G = MCQ180G,
  MCD180K = MCQ180K,
  MCD180L = MCQ180L,
  MCD180M = MCQ180M,
  MCD180N = MCQ180N,
  MCQ366A = MCQ365A,
  MCQ366B = MCQ365B,
  MCQ366C = MCQ365C,
  MCQ366D = MCQ365D,
  MCQ371A = MCQ370A,
  MCQ371B = MCQ370B,
  MCQ371C = MCQ370C,
  MCQ371D = MCQ370D
)

# --------- Deleting all necessary columns ---------
#colsToDel <- notin17[c(2, 24:53, 62:145, 147)]

#df_11_12 <- df_11_12[-which(names(df_11_12) %in% colsToDel)]

View(label(df_full[,order(names(df_full))]))
View(label(select(df_11_12, sort(notin17))))
data.frame(ID = list_of_qs17, Question = labels)

#These are actually different questions after further examination
# Same wording but for different target variables than 2017-2018
# df_11_12 <- df_11_12 %>% rename(
#    WHDO80A = WHD100A,
#    WHD080B = WHD100B,
#    WHD080C = WHD100C,
#    WHD080D = WHD100D,
#    WHD080E = WHD100E,
#    WHD080F = WHD100F,
#    WHD080G = WHD100G,
#    WHD080H = WHD100H,
#    WHD080I = WHD100I,
#    WHD080K = WHD100J,
#    WHD080K = WHD100K,
#    WHD080L = WHD100L,
#    WHD080M = WHD100M,
#    WHD080N = WHD100N,
#    WHD080O = WHD100O,
#    WHD080P = WHD100P,
#    WHD080Q = WHD100Q,
#    WHD080R = WHD100R,
#    WHD080S = WHD100S,
#    WHD080T = WHD100T
# )
# more_removed_qs <- c("WHD100A",WHD100B,WHD100C,WHD100D,WHD100E,WHD100F,WHD100G,WHD100H, 
#                      WHD100I, WHD100J, WHD100K, WHD100L, WHD100M, WHD100N, WHD100O, WHD100P,WHD100Q, WHD100R, WHD100S,WHD100T)

# Just remove the left over columns not in 2017

df_11_12 <- select(df_11_12, -notin17)

# --------- Changing all necessary columns ---------




save(df_11_12, file = "NHANES_Clean_2011_2012.RData")


