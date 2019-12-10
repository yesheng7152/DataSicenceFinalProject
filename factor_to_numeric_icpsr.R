#---------------------------------------------------------------------------
# factor_to_numeric_icpsr.R
# 2012/12/06
#
# Convert R factor variable back to numeric in an ICPSR-produced R data
# frame. This works because the original numeric codes were prepended by
# ICSPR to the factor levels in the process of converting the original
# numeric categorical variable to factor during R data frame generation.
#
# REQUIRES add.value.labels function from prettyR package
#    http://cran.r-project.org/web/packages/prettyR/index.html
#
#
# Substitute the actual variable and data frame names for da99999.0001$MYVAR
# placeholders in syntax below.
#
#    data frame = da99999.0001
#    variable   = MYVAR
#
#
# Line-by-line comments:
#
# (1) Load prettyR package
#
# (2) Create object (lbls) containing the factor levels for the specified
#     variable.  Sort will be numeric as original codes (zero-padded, if
#     necessary) were preserved in the factor levels.
#
# (3) Strip original codes from lbls, leaving only the value labels, e.g.,
#       "(01) STRONGLY DISAGREE" becomes "STRONGLY DISAGREE"
#
# (4) Strip labels from data, leaving only the original codes, e.g.,
#       "(01) STRONGLY DISAGREE" becomes "1"
#
#     Then, coerce variable to numeric
#
# (5) Add value labels, making this a named numeric vector
#---------------------------------------------------------------------------

library(prettyR)
library(readr)
da=load(file = '20520-0001-Data.rda') 
da=get(da)
# Function to change the string value to numeric value
strip_num<-function(colmn){
  lbls<-sort(levels(colmn))
  lbls<-(sub("^\\([0-9]+\\) +(.+$)", "\\1", lbls))
  colmn<- as.numeric(sub("^\\(0*([0-9]+)\\).+$", "\\1", colmn))
  colmn <- add.value.labels(colmn, lbls)
  return (colmn)
}

ckk<-data.frame()

# Getting columns from the 2015 survery and selected columns are the ones that we felt represents sucess. Combined them into the result dataframe
result<- data.frame(da$V404A,da$V407,da$V411,da$V415E,da$V419,da$V420,da$V429A,da$V429B,da$V429C,da$V429C,da$V429D,da$V434,da$V448L,da$V447,da$V449)
# change factor values into number values, prepare for future traning 
result[2:3]<-lapply(result[2:3],strip_num)
result[5:15]<-lapply(result[5:15], strip_num)

predictor1992<-data.frame(da$V1,da$V128,da$V139,da$V148,da$C4,da$C1,da$V28,da$V30,da$V18,da$V22,da$V23,da$V65,da$V85,da$C14,da$C16,da$V61,da$V94,da$V119,da$V66,da$V69)
predictor1992[6:13]<-lapply(predictor1992[6:13],strip_num)
predictor1992[16:20]<-lapply(predictor1992[16:20],strip_num)








