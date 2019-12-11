library(prettyR)
library(readr)
load(file = "20520-0001-Data.rda") 
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
result<- data.frame(da$V404A,da$V407,da$V411,da$V415E,da$V419,da$V420,da$V429A,da$V429B,da$V429C,da$V429C,da$V429D,da$V434,da$V448L,da$V447,da$V449)
# change factor values into number values, prepare for future traning 
result[2:3]<-lapply(result[2:3],strip_num)
result[5:15]<-lapply(result[5:15], strip_num)
#find the average of respondent's english skills
result$AverageEnglishSkill <- result$V429A
#update the colnnames 
colnames(result) <- c("Residence type/other", "Highest education completed", "Present work situation", "Current job prestige scores", "Current occupation satisfaction",
                      "Present income satisfaction", "Respondent speak English well"," ")

predictor1992<-data.frame(da$V1,da$V128,da$V139,da$V148,da$C4,da$C1,da$V28,da$V30,da$V18,da$V22,da$V23,da$V65,da$V85,da$C14,da$C16,da$V61,da$V94,da$V119,da$V66,da$V69)
predictor1992[6:13]<-lapply(predictor1992[6:13],strip_num)
predictor1992[16:20]<-lapply(predictor1992[16:20],strip_num)