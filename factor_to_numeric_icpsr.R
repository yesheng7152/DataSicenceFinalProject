library(prettyR)
library(readr)
library(dplyr)
da=load("~/Desktop/2019 Fall/Data Science/Final Project/20520-0001-Data.rda")
da=get(da)
# Function to change the string value to numeric value
strip_num<-function(colmn){
  lbls<-sort(levels(colmn))
  lbls<-(sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  colmn<- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", colmn))
  colmn <- add.value.labels(colmn, lbls)
  return (colmn)
}

# Getting columns from the 2015 survery and selected columns are the ones that we felt represents sucess. Combined them into the result dataframe
# V404A - Residence type/other, V407 - Highest school grade/year completed, V411 - Present work situation, V415E - Current job Treiman prestige scores,
# V419 - Current occupation satisfaction, V420 - Present income satisfaction, V429A - Respondent speak English well, V429B - Respondent understand English well,
# V429C - Respondent read English well, V429D - Respondent write English well, V434 - Respodent race/ethnic identity importance, V448L - Respodent detention/jail/prison
# V447 - Country feels like home, V449 - Respondent health. 
result<- data.frame(da$CASEID,da$V404A,da$V407,da$V411,da$V415E,da$V419,da$V420,da$V429A,da$V429B,da$V429C,da$V429D,da$V434,da$V448L,da$V447,da$V449)
# change factor values into number values, prepare for future traning 
result[3:4]<-lapply(result[3:4],strip_num)
result[6:15]<-lapply(result[6:15], strip_num)
#find the average of respondent's english skills
result$AverageEnglishSkill <- (result$da.V429A+result$da.V429B+result$da.V429C+result$da.V429D)/4
#After findiing out the avereage value delete the individuals
result$da.V429A<-NULL
result$da.V429B<-NULL
result$da.V429C<-NULL
result$da.V429D<-NULL
#update the colnnames 
colnames(result) <- c("caseId", "Residence type/other", "Highest education completed", "Present work situation", "Current job prestige scores", "Current occupation satisfaction",
                      "Present income satisfaction", "Respodent identity importance","Respodent detention/jail/prison","Country feels like home", "Respondent health","Average English Skill")
#Remove rows that contains NAs 
result<-na.omit(result)

# Getting columns from the 1992 survery and selected columns are the ones that we felt represents sucess. Combined them into the predictor1992 dataframe
# CASEID - Case Id, V128 - respondent desired job prestige score, V139 - GPA, V148 - Parent SES index,
# C4 - English Knowledge, C1 - Private school, V28 - Houshold guardians, V30 - Total number household members, V18 - Respondent sex,
# V22 - Respondent US stay length, V64 - Respondent job preference, V85 - Respodent ever felt discriminated, C14 - Depression,
# C16 - Self-esteem, V61 - Respondent education expectation, V94 - Hours/day on HW, V119 - Good grades important to me,
# V66 - Reason Father came to US, V69 - Reason Mom came to US
predictor1992<-data.frame(da$CASEID,da$V128,da$V139,da$V148,da$C4,da$C1,da$V28,da$V30,da$V18,da$V22,da$V64,da$V85,da$C14,da$C16,da$V61,da$V94,da$V119,da$V66,da$V69)
# change factor values into number values, prepare for future traning 
predictor1992[6:12]<-lapply(predictor1992[6:12],strip_num)
predictor1992[15:19]<-lapply(predictor1992[15:19],strip_num)
#Update the colnnames
colnames(predictor1992)<-c("caseId", "desired job prestige score-1992", "GPA","Parent SES index-1992","English Knowledge-1992", "Private school-1992","Houshold guardians-1992",
                           "number household members-1992","Respondent sex","Respondent US stay length-1992", "Respondent job preference-1992", "felt discriminated-1992",
                           "Depression-1992","Self-esteem-1992","education expectation-1992","Hours/day on HW-1992","Good grades importance-1992","Reason Dad came to US",
                           "Reason Mom came to US")

# CASEID - Case Id, V228 - Present living situation, V230 - Total number people living w/respondent, V243 - Economic situation/3 year ago,
# V324B - Parent divorced/separated past year, V324C - Parent re/married past year, V324D - Parent lost job/past year, V324E - Respondent ill/disabled past year,
# V324F - Parent died past year, V206 - Gender, V223 - Respodent US Citizenship, V264 - respondent job classification, V211 - Don't feel save at school, 
# V261 - attainable education level, V262 - Paren education preference, V294 - Respondent hour studying, V319 - Good grade importance, C5 - English Knowledge,
# C2 - Private school, C15 - Depression, C17 - Self-esteem, C19 - Familism index, C18 - Family cohesion, V332 - GPA, V337 - Dropped out by 1995, V357 - Percent daily school attendance
predictor1995<-data.frame(da$CASEID,da$V228,da$V230,da$V243,da$V324B,da$V324C,da$V324D,da$V324E,da$V324F,da$V206,da$V223,da$V264,
                          da$V211,da$V261,da$V262,da$V294,da$V319,da$C5,da$C2,da$C15,da$C17,da$C19,da$C18,da$V332,da$V337,da$V357)
# change factor values into number values, prepare for future traning 
predictor1995[2]<-lapply(predictor1995[2],strip_num)
predictor1995[4:17]<-lapply(predictor1995[4:17],strip_num)
predictor1995[19]<-lapply(predictor1995[19],strip_num)
predictor1995[25]<-lapply(predictor1995[25],strip_num)

#Update the colnnames 
colnames(predictor1995)<-c("caseId", "Present living situation-1995", "number people living w/respondent-1995","Economic situation/3 year ago-1995","Parent divorced/separated past year-1995",
                           "Parent re/married past year-1995","Parent lost job/past year-1995","Respondent ill/disabled past year-1995",
                           "Parent died past year-1995","Respondent sex-1995","Respodent US Citizenship-1995", "Respondent job classification-1995", 
                           "Don't feel save at school-1995","attainable education level-1995","Paren education preference-1995","Respondent hour studying-1995",
                           "Good grade importance-1995","English Knowledge-1995","Private school-1995","Depression-1995","Self-esteem-1995","Familism index-1995",
                           "Family cohesion-1995","GPA-1995","Dropped out by 1995","Percent daily school attendance-1995")

#Combine the three data frames together using their caseid, base on result data frame. Since we only want to look at respondents who paticipated and have all infor on
# the last survey. 
Final<-left_join(x = result, y = predictor1992, by = "caseId")
Final<-left_join(x=Final, y=predictor1995, by= "caseId")


