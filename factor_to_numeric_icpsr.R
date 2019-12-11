library(prettyR)
library(readr)
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
result[2:3]<-lapply(result[3:4],strip_num)
result[5:14]<-lapply(result[6:15], strip_num)
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





