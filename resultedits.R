library(prettyR)
library(readr)
library(dplyr)
library(factoextra)  ## Used for PCA visualizations
da=load("~/Desktop/2019 Fall/Data Science/Final Project/20520-0001-Data.rda")
da=get(da)

strip_num<-function(colmn){
  lbls<-sort(levels(colmn))
  lbls<-(sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  colmn<- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", colmn))
  colmn <- add.value.labels(colmn, lbls)
  return (colmn)
}

Res<- data.frame(da$CASEID,if_else(strip_num(da$V425)<2,1,0),if_else(strip_num(da$V448E)==1,0,1),da$V407,da$V411,da$V415E,da$V419,da$V420,da$V429A,da$V429B,da$V429C,da$V429D,da$V434,if_else(strip_num(da$V448L)==1,0,1),da$V447,da$V449,da$V403)

# change factor values into number values, prepare for future traning 
Res[4:5]<-lapply(Res[4:5],strip_num)
Res[7:17]<-lapply(Res[7:17], strip_num)
#find the average of respondent's english skills
Res$AverageEnglishSkill <- (Res$da.V429A+Res$da.V429B+Res$da.V429C+Res$da.V429D)/4
#After findiing out the avereage value delete the individuals
Res$da.V429A<-NULL
Res$da.V429B<-NULL
Res$da.V429C<-NULL
Res$da.V429D<-NULL
#update the colnnames 
colnames(Res) <- c("caseId", "Residence Own house/aprt","Disabled or Ill", "Highest education completed", "Present work situation", "Current job prestige scores", "Current occupation satisfaction",
                      "Present income satisfaction", "Respodent identity importance","Respodent detention/jail/prison","Country feels like home", "Respondent health","Has Children","Average English Skill")
# Add mariageStatus, and change Married, Engaged to be Married, and Living with Partner to be 1, while all others to be 0
Res$MariageStatus<-strip_num(da$V402)
Res$MariageStatus<-if_else(Res$MariageStatus < 4, 1, 0)

#Change the ranking from 5-very healthy to 1-very poor
Res$`Respondent health`<-(Res$`Respondent health`-6)*-1
#To indicates the sense of belonging change if "I Don't feel 'home' in either country" to 0, while others as 1
Res$`Country feels like home`<-if_else(Res$`Country feels like home`==3,0,1)
ls<-Res$`Highest education completed`
Res$`Highest education completed`<-if_else(ls == 10, as.character(NA),as.character(ls))
Res$`Highest education completed`<-strip_num(Res$`Highest education completed`)
Res$`Present work situation`<-(Res$`Present work situation`-9)*-1

#Remove rows that contains NAs 
Res<-na.omit(Res)

colnames(Res)


### Sucess Index#####
# Based on a study conducted by Populace, in partnership with Gallup: Sucess Index: 
#45.9% Status + 19.8% Education + 4.7% Relastionships + 4.9 % Character + 8.8% Finance + 3.5% Health + 8.4% Work + 4.1 Quality of Life
#We will fit the variables from the result data frame to one of the 8 categories above. Then Calculate the sucess Index using the 


# success_index<-function(row){
#   quality_index<-(row$`Residence Own house/aprt`*.041)
#   education_index<-(row$Res$`Highest education completed`+row$`Average English Skill`)*.198
#   relation_index<-(row$`Has Children`+row$`MariageStatus`)*.047
#   character_index<-(row$`Country feels like home`+row$`Respodent identity importance`)*.049
#   finance_index<-(row$`Present income satisfaction`)*.088
#   health_index<-(row$`Disabled or Ill`+row$`Respondent health`)*.035
#   work_index<-(row$`Present work situation`+row$`Current occupation satisfaction`)*.084
#   status_index<-(row$`Current job prestige scores`+row$`Respodent detention/jail/prison`)*.459
#   success_i=quality_index+education_index+relation_index+character_index+finance_index+health_index+work_index+status_index
#   return (success_i)
# }

success_index<-function(row){
  quality_index<-(row[2]*4.1)
  education_index<-(row[4]+row[14])*19.8
  relation_index<-(row[13]+row[15])*4.7
  character_index<-(row[11]+row[9])*4.9
  finance_index<-(row[8])*8.8
  health_index<-(row[3]+row[12])*3.5
  work_index<-(row[5]+row[7])*8.4
  status_index<-(row[6]+row[10])*45.9
  success_i=quality_index+education_index+relation_index+character_index+finance_index+health_index+work_index+status_index
  return (success_i/100)
}
success_index(Res[2,])




# equation above. 