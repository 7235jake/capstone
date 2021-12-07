# capstone
CMDA Capstone Fall 2021 (NHANES)

ROV_5_year_boxplots – 
  Folder containing boxplots for all five years of range of value questions
  
ROV_avgs_5_year_plots – 
  Folder containing plots of the mean of all range of values over 5 years
  
.RDataTmp – 
  List of 39 range of value questions that were associated with diabetes
  
.gitignore – 
  Ignore

1718questions.RData – 
  List of all questions from 2017-2018 survey

2009_2010_Join.R – 
  File loading in cleaning and combining all questions we observed from 2009-2010 

2011_2012_Join.R – 
  File loading in cleaning and combining all questions we observed from 2011-2012

2013_2014_Join.R – 
  File loading in cleaning and combining all questions we observed from 2013-2014

2015_2016_Join.R – 
  File loading in cleaning and combining all questions we observed from 2015-2016

2015_2016_Join_tidy_version.R – 
  File loading in cleaning and combining all questions from 2015 – 2016 using tidyverse package

2017_2018_Join.R – 
  File loading in cleaning and combining all questions we observed from 2017-2018

5 year boxplots.R – 
	File creating and saving all boxplots

5 year trends.R – 
	File creating and saving all mean plots

Clean Full.R – 
	File cleaning the full data sets responses

Diabetes_ROV_qs.RData – 
	List of range of value questions that are associated with diabetes

Merge.R – 
	File creating full data set including all 5 survey years combined

Modeling.Rmd – 
	File creating predictive models for diabetes

NHANESCleanFactors.RData – 
	Dataframe of all variables with the correct questions saved as factors

NHANES_Clean.RData – 
	Dataframe with all questions and answers that qualified to be considered in the model across the five survey years

NHANES_Clean_2009_2010.RData – 
	Dataframe with all questions and answers from 2009-2010 that also exist in 2017-2018 	survey (Not totally clean)

NHANES_Clean_2011_2012.RData
	Dataframe with all questions and answers from 2011-2012 that also exist in 2017-2018 	survey (Not totally clean)

NHANES_Clean_2013_2014.RData – 
  Dataframe with all questions and answers from 2013 – 2014 that also exist in 2017-2018 	survey (Not totally clean)

NHANES_Clean_2015_2016.RData – 
  Dataframe with all questions and answers from 2015 – 2016 that also exist in 2017-2018 survey (Not totally clean)

NHANES_Clean_2017_2018.RData
  Dataframe with all questions and answers from 2015 – 2016 that also exist in 2017-2018 survey

NHANES_Winsorized_2009_2010.RData – 
	Dataframe with all cleaned questions and winsorized answers from 2009-2010

NHANES_Winsorized_2011_2012.RData – 
  Dataframe with all cleaned questions and winsorized answers from 2011-2012

NHANES_Winsorized_2013_2014.RData – 
	Dataframe with all cleaned questions and winsorized answers from 2013-2014

NHANES_Winsorized_2015_2016.RData – 
	Dataframe with all cleaned questions and winsorized answers from 2015-2016

NHANES_Winsorized_2017_2018.RData – 
	Dataframe with all cleaned questions and winsorized answers from 2017-2018

PropNullDF.RData
	Dataframe listing all questions, how many responses are available to use and proportion of null responses

QualityAnalysis.Rmd
	Markdown file with quality analysis and some initial modeling

README.md – 
	Index and summary of everything in repository

ResponseCleaningIdentifiers.RData – 
	List of initial response variables we considered for the model

Selecting Variables.R – 
	File describing and saving final list of range of value questions associated with diabetes	

TranslationsFull.RData – 
	Nested list of all questions and possible responses after they were manipulated to 	simplify all possible answers

capstone.Rproj – 
	Working directory

chisquareTestFactors.RData – 
  Dataframe with all factor questions, their p value in a Chi-squared test, test statistics, and 	their row number  
  marker when their levels were dropped in the original dataframe only including factors

factors-chisquared.Rmd – 
	Markdown file executing all the chi squared tests and creating the dataframe

selected_Winsorized_df.RData – 
	Dataframe containing winsorized responses for all questions eligible to be in the model based on the null 
	threshold

selected_df.RData – 
	Dataframe of all questions eligible to be in the model based on the null threshold

winsorize_tableones.R – 
	File that winsorizes all the responses and saves the dataframe containing them


