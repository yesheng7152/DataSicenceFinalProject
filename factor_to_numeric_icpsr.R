library(prettyR)
library(readr)
library(dplyr)
library(factoextra)  ## Used for PCA visualizations
library(caret)  ## package for model comparisons
library(glmnet) ## package for fitting lasso models
library(mgcv)   ## package for fitting GAM models
library(pls)
library(tidyr) 

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
# V425 - Residence Own house/aprt, V448E - Disabled or Ill, V407 - Highest school grade/year completed, V411 - Present work situation, V415E - Current job Treiman prestige scores,
# V419 - Current occupation satisfaction, V420 - Present income satisfaction, V429A - Respondent speak English well, V429B - Respondent understand English well,
# V429C - Respondent read English well, V429D - Respondent write English well, V434 - Respodent race/ethnic identity importance, V448L - Respodent detention/jail/prison
# V447 - Country feels like home, V449 - Respondent health, V404 - 
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
#Counting only the ones that have a clear degree status that could be valued from 0-9
Res$`Highest education completed`<-if_else(ls == 10, as.character(NA),as.character(ls))
Res$`Highest education completed`<-strip_num(Res$`Highest education completed`)
# Reverse the numbers values, so the better situation have a higher number value
Res$`Present work situation`<-(Res$`Present work situation`-9)*-1

#Remove rows that contains NAs 
Res<-na.omit(Res)
Res$`Current job prestige scores`<-scale(Res$`Current job prestige scores`)


### Sucess Index#####
# Based on a study conducted by Populace, in partnership with Gallup: Sucess Index: 
#45.9% Status + 19.8% Education + 4.7% Relastionships + 4.9 % Character + 8.8% Finance + 3.5% Health + 8.4% Work + 4.1 Quality of Life
#We will fit the variables from the result data frame to one of the 8 categories above. Then Calculate the sucess Index using the 


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
Res$successIndex<-apply(Res, 1, success_index)


# Getting columns from the 1991 survery and selected columns are the ones that we felt represents sucess. Combined them into the predictor1991 dataframe
# CASEID - Case Id, V128 - respondent desired job prestige score, V139 - GPA, V148 - Parent SES index,
# C4 - English Knowledge, C1 - Private school, V28 - Houshold guardians, V30 - Total number household members, V18 - Respondent sex,
# V22 - Respondent US stay length, V64 - Respondent job preference, V85 - Respodent ever felt discriminated, C14 - Depression,
# C16 - Self-esteem, V61 - Respondent education expectation, V94 - Hours/day on HW, V119 - Good grades important to me,
# V66 - Reason Father came to US, V69 - Reason Mom came to US
predictor1991<-data.frame(da$CASEID,da$V128,da$V139,da$V148,da$C4,da$C1,da$V28,da$V30,da$V18,da$V22,da$V64,da$V85,da$C14,da$C16,da$V61,da$V94,da$V119,da$V66,da$V69)
# change factor values into number values, prepare for future traning 
predictor1991[6:12]<-lapply(predictor1991[6:12],strip_num)
predictor1991[15:19]<-lapply(predictor1991[15:19],strip_num)
#Update the colnnames
colnames(predictor1991)<-c("caseId", "desired.job.prestige.score-1991", "GPA","Parent.SES.index-1991","English.Knowledge-1991", "Private.school-1991","Houshold.guardians-1991",
                           "number.household.members-1991","Sex","Respondent.US.stay.length-1991", "Respondent job preference-1991", "felt discriminated-1991",
                           "Depression-1991","Self-esteem-1991","education expectation-1991","Hours/day on HW-1991","Good grades importance-1991","Reason Dad came to US",
                           "Reason Mom came to US")
#Removing NA values for PCA
predictor1991<-na.omit(predictor1991)

# CASEID - Case Id, V228 - Present living situation, V230 - Total number people living w/respondent, V243 - Economic situation/3 year ago,
# V324B - Parent divorced/separated past year, V324C - Parent re/married past year, V324D - Parent lost job/past year, V324E - Respondent ill/disabled past year,
# V324F - Parent died past year, V206 - Gender, V223 - Respodent US Citizenship, V264 - respondent job classification, V211 - Don't feel save at school, 
# V261 - attainable education level, V262 - Paren education preference, V294 - Respondent hour studying, V319 - Good grade importance, C5 - English Knowledge,
# C15 - Depression, C17 - Self-esteem, C19 - Familism index, C18 - Family cohesion, V332 - GPA, V337 - Dropped out by 1995, V357 - Percent daily school attendance, C2 - Private school
predictor1995<-data.frame(da$CASEID,da$V228,da$V230,da$V243,da$V324B,da$V324C,da$V324D,da$V324E,da$V324F,da$V206,da$V223,da$V264,
                          da$V211,da$V261,da$V262,da$V294,da$V319,da$C5,da$C15,da$C17,da$C19,da$C18,da$V332,da$V337,da$V357,da$C2)
# change factor values into number values, prepare for future traning 
predictor1995[2]<-lapply(predictor1995[2],strip_num)
predictor1995[4:17]<-lapply(predictor1995[4:17],strip_num)
predictor1995[24]<-lapply(predictor1995[24],strip_num)
predictor1995[26]<-lapply(predictor1995[26],strip_num)

#Update the colnnames 
colnames(predictor1995)<-c("caseId", "Present living situation-1995", "number people living w/respondent-1995","Economic situation/3 year ago-1995","Parent divorced/separated past year-1995",
                           "Parent re/married past year-1995","Parent lost job/past year-1995","Respondent ill/disabled past year-1995",
                           "Parent died past year-1995","Respondent sex-1995","Respodent US Citizenship-1995", "Respondent job classification-1995", 
                           "Don't feel save at school-1995","attainable education level-1995","Paren education preference-1995","Respondent hour studying-1995",
                           "Good grade importance-1995","English Knowledge-1995","Depression-1995","Self-esteem-1995","Familism index-1995",
                           "Family cohesion-1995","GPA-1995","Dropped out by 1995","Percent daily school attendance-1995","Private school-1995")
#Removing the null values for PCA
predictor1995<-na.omit(predictor1995)

#Combine the three data frames together using their caseid, base on result data frame. Since we only want to look at respondents who paticipated and have all infor on
# the last survey. 
Final<-left_join(x = Res, y = predictor1991, by = "caseId")
Final<-left_join(x=Final, y=predictor1995, by= "caseId")
Final<-na.omit(Final)

#### Visualizations#####
#PCA on result
P <- prcomp(Res[2:15], scale = TRUE)
fviz_pca_biplot(P, repel = FALSE, # Avoid text overlapping (doesn't scale to large datasets)
                col.var = "red", # Variables color
                col.ind = "black")  

fviz_eig(P, addlabels = TRUE)
#From the previous we see that from demiensions 1-8 they are all important
fviz_contrib(P, choice = "var", axes = 1, top = 6) 
fviz_contrib(P, choice = "var", axes = 2, top = 6)
fviz_contrib(P, choice = "var", axes = 3, top = 6)
fviz_contrib(P, choice = "var", axes = 4, top = 6)
fviz_contrib(P, choice = "var", axes = 5, top = 6)
fviz_contrib(P, choice = "var", axes = 6, top = 6)
fviz_contrib(P, choice = "var", axes = 7, top = 6)
fviz_contrib(P, choice = "var", axes = 8, top = 6)

#PCA on Predictor1991
P1991<-prcomp(predictor1991[2:19],scale = TRUE)
fviz_pca_biplot(P1991, repel = FALSE, # Avoid text overlapping (doesn't scale to large datasets)
                col.var = "red",
                col.ind = "black")# Variables color
fviz_eig(P1991, addlabels = TRUE)

#From the previous we see that demensions from 1 to 4 are the most important
fviz_contrib(P1991, choice = "var", axes = 1, top = 6)
fviz_contrib(P1991, choice = "var", axes = 2, top = 6)
fviz_contrib(P1991, choice = "var", axes = 3, top = 6)
fviz_contrib(P1991, choice = "var", axes = 4, top = 6)

#PCA on Predictor1995
P1995<-prcomp(predictor1995[2:25],scale = TRUE)
fviz_pca_biplot(P1995, repel = FALSE, # Avoid text overlapping (doesn't scale to large datasets)
                col.var = "red",
                col.ind = "black")# Variables color
fviz_eig(P1995, addlabels = TRUE)
#From the previous we see that demensions from 1 to 4 are the most important
fviz_contrib(P1991, choice = "var", axes = 1, top = 6)
fviz_contrib(P1991, choice = "var", axes = 2, top = 6)
fviz_contrib(P1991, choice = "var", axes = 3, top = 6)
fviz_contrib(P1991, choice = "var", axes = 4, top = 6)






#### New Data Set ####
# Removing the result coloumns and kept only the sucess index
ImmigrantSuccess<-select(Final,-c(1:15))
colnames(ImmigrantSuccess)<-make.names(names(ImmigrantSuccess))


#### Run models####
#Set the number of repeats and folds on the dataset
fit.control <- trainControl(method = "repeatedcv", number = 5, repeats = 10,verboseIter = TRUE)
#Set seeds so that all models starts the same for compairsion purpose in the future
set.seed(123)
#Linear Regression model 
fit.linear <- train(successIndex ~ ., data = ImmigrantSuccess, method = "lm", trControl = fit.control)
#LASSO model
X = model.matrix(data = ImmigrantSuccess[,-44], ~ -1 + .) ## Set up the matrix including dummy vars
y = ImmigrantSuccess$successIndex

lasso <- glmnet(x = X, y = y)
set.seed(123)
lams <- expand.grid(alpha = 1, lambda = lasso$lambda)
fit.lasso <- train(successIndex ~ ., data = ImmigrantSuccess, method = "glmnet", trControl = fit.control, tuneGrid = lams)

#GAM Model
set.seed(123)
fit.gam <- train(successIndex ~ ., data = ImmigrantSuccess, method = "gam", trControl = fit.control)

#PCR Model
set.seed(123)
fit.pcr <- train(successIndex ~ ., data = ImmigrantSuccess, method = "pcr", trControl = fit.control)

#Summary
rs<- resamples(list(LM = fit.linear, 
                    LASSO = fit.lasso,
                    GAM = fit.gam,
                    PCR = fit.pcr))
sumdata<-summary(rs)
coef(fit.lasso$finalModel, s = fit.lasso$bestTune$lambda)


## Graph for evaluation 
# Getting a copy of the data
RMSEframe<-data.frame(sumdata$statistics$RMSE)
#Removing the NA, which are all zero
RMSEframe<-RMSEframe[1:6]
#Have a column for the model names
RMSEframe$Model<-rownames(RMSEframe)
#change the row names to null
rownames(RMSEframe)<-NULL
#Reorganize the dataframe into something plotable 
RMSEframe <- gather(data = RMSEframe, key = Quatile, value = RMSE, `Min.`,`X1st.Qu.`,`Median`,`Mean`,`X3rd.Qu.`,`Max.`)


p2 <- RMSEframe %>%
  ggplot(aes(x=Quatile, y=RMSE,color=Model,group = Model)) +
  geom_line() +
  geom_point(size=2) +
  ggtitle("RMSE Model Comparison") 
p2
