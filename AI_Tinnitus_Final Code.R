#Please set the working directory and load the "AI_TinnitusAnaFinal.RData" file into the same working directory.
setwd("C:/Users/18133/OneDrive - The University of Texas-Rio Grande Valley/Dr.Vinaya/Project1_Tinitus/Paper 2/AI_TinnitusFinal Code")

load("AI_TinnitusAnaFinal.RData")

install.packages("corrplot")

library(corrplot)
library(FNN)
library(RColorBrewer)
library(VIM)
library(readxl)

library(e1071)
library(kernlab)
library(nnet)
library(neuralnet)
library(ggplot2)
library(tidyverse)


library(caret)
library(class)
library(faraway)
library(olsrr)
library(MASS)
library(leaps)
library(mctest)
library(car)


##Loading the Data
TIDataRevisedAna<-read.csv("AI_DATARevisedAna_Sep29.csv",header=TRUE)
head(TIDataRevisedAna)
dim(TIDataRevisedAna)#228 by 48
str(TIDataRevisedAna)
sum(is.na(TIDataRevisedAna))#175

#Data Pre-processing and Summary Analysis

TIDataRevisedAna$Low_Freq_Tinnitus<-as.factor(TIDataRevisedAna$Low_Freq_Tinnitus)

levels(TIDataRevisedAna$Low_Freq_Tinnitus)[levels(TIDataRevisedAna$Low_Freq_Tinnitus)==""] <- "No" 
levels(TIDataRevisedAna$Low_Freq_Tinnitus)[levels(TIDataRevisedAna$Low_Freq_Tinnitus)=="Y"] <- "Yes"

levels(TIDataRevisedAna$Low_Freq_Tinnitus)
table(TIDataRevisedAna$Low_Freq_Tinnitus) #No 212 , Yes 16

TIDataRevisedAna$Multi_Sounds<-as.factor(TIDataRevisedAna$Multi_Sounds)
TIDataRevisedAna$Pulsatile_Tinnitus<-as.factor(TIDataRevisedAna$Pulsatile_Tinnitus)

TIDataRevisedAna$Gender<-as.factor(TIDataRevisedAna$Gender)
TIDataRevisedAna$Loud._Noise_Exposure<-as.factor(TIDataRevisedAna$Loud._Noise_Exposure)
TIDataRevisedAna$Work_Less<-as.factor(TIDataRevisedAna$Work_Less)
TIDataRevisedAna$Hearing_Loss<-as.factor(TIDataRevisedAna$Hearing_Loss)
# > table(TIDataRevisedAna$Hearing_Loss)
# 
# 0   1   2   3 
# 49 104  46  29 

levels(TIDataRevisedAna$Hearing_Loss)[levels(TIDataRevisedAna$Hearing_Loss)=="2"] <- "2" 
levels(TIDataRevisedAna$Hearing_Loss)[levels(TIDataRevisedAna$Hearing_Loss)=="3"] <- "2" 
levels(TIDataRevisedAna$Hearing_Loss)[levels(TIDataRevisedAna$Hearing_Loss)=="4"] <- "3" 

#After Merging
# table(TIDataRevisedAna$Hearing_Loss)
# 0   1   2   3 
# 49 104  46  29 

TIDataRevisedAna$Psychological_Condition<-as.factor(TIDataRevisedAna$Psychological_Condition)
TIDataRevisedAna$Past_Treatment<-as.factor(TIDataRevisedAna$Past_Treatment)
TIDataRevisedAna$HearingAid_usage<-as.factor(TIDataRevisedAna$HearingAid_usage)

# table(TIDataRevisedAna$HearingAid_usage)
# 
# 0   1   2   3 
# 159   5  14  50 

levels(TIDataRevisedAna$HearingAid_usage)[levels(TIDataRevisedAna$HearingAid_usage)=="1"] <- "1" #New Cat 1 is One Ear Only
levels(TIDataRevisedAna$HearingAid_usage)[levels(TIDataRevisedAna$HearingAid_usage)=="2"] <- "1" #New Cat 1 is One Ear Only
levels(TIDataRevisedAna$HearingAid_usage)[levels(TIDataRevisedAna$HearingAid_usage)=="3"] <- "2" #New Cat 2 is Both Ears

# > table(TIDataRevisedAna$HearingAid_usage)
# 
# 0   1   2 
# 159  19  50 

TIDataRevisedAna$Sounds.can.distract<-as.factor(TIDataRevisedAna$Sounds.can.distract)
TIDataRevisedAna$Medication_for_Tinnitus<-as.factor(TIDataRevisedAna$Medication_for_Tinnitus)

TIDataRevisedAna$Tinnitus_Location<-as.factor(TIDataRevisedAna$Tinnitus_Location)
# > table(TIDataRevisedAna$Tinnitus_Location)
# 
# -oth-     1     2     3     4     5 
# 21    23    38   109    34     3 

levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="1"] <- "1" 
levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="2"] <- "1" 
levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="3"] <- "2" 
levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="4"] <- "3" 
levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="5"] <- "4" 
levels(TIDataRevisedAna$Tinnitus_Location)[levels(TIDataRevisedAna$Tinnitus_Location)=="-oth-"] <- "5" 


TIDataRevisedAna$Tinnitus_Location<-relevel(TIDataRevisedAna$Tinnitus_Location,ref="1")

TIDataRevisedAna$TypeTinn_Ringing<-as.factor(TIDataRevisedAna$TypeTinn_Ringing)
TIDataRevisedAna$TypeTinn_Buzzing<-as.factor(TIDataRevisedAna$TypeTinn_Buzzing)
TIDataRevisedAna$TypeTinn_HighPitch<-as.factor(TIDataRevisedAna$TypeTinn_HighPitch)
TIDataRevisedAna$TypeTinn_LowPitch<-as.factor(TIDataRevisedAna$TypeTinn_LowPitch)
TIDataRevisedAna$TypeTinn_Pulsing<-as.factor(TIDataRevisedAna$TypeTinn_Pulsing)
TIDataRevisedAna$TypeTinn_Clicking<-as.factor(TIDataRevisedAna$TypeTinn_Clicking)
TIDataRevisedAna$TypeTinn_Music<-as.factor(TIDataRevisedAna$TypeTinn_Music)
TIDataRevisedAna$TypeTinn_Voices<-as.factor(TIDataRevisedAna$TypeTinn_Voices)
TIDataRevisedAna$TypeTinn_Humming<-as.factor(TIDataRevisedAna$TypeTinn_Humming)

TIDataRevisedAna$How_of_Tinnitus_Heard<-as.factor(TIDataRevisedAna$How_of_Tinnitus_Heard)
levels(TIDataRevisedAna$How_of_Tinnitus_Heard)
table(TIDataRevisedAna$How_of_Tinnitus_Heard)
# table(TIDataRevisedAna$How_of_Tinnitus_Heard)
# 
# 2   3   4   5   6 
# 4   3   4  63 154 


TIDataRevisedAna$Education_Level<-as.factor(TIDataRevisedAna$Education_Level)
# > table(TIDataRevisedAna$Education_Level)
# 
# 0  1  2  3  4  5  6 
# 2 57 47 31 61 24  6
TIDataRevisedAna$Employment_Type<-as.factor(TIDataRevisedAna$Employment_Type)
# > table(TIDataRevisedAna$Employment_Type)
# 
# 1  2  3  4  5  6  7  8  9 10 11 12 
# 27 46 13 17 11 11  6  8  4  1 73 11 

TIDataRevisedAna$Difficulty_Hearing<-as.factor(TIDataRevisedAna$Difficulty_Hearing)
# > table(TIDataRevisedAna$Difficulty_Hearing)
# 
# 0   1   2   3 
# 47 125  48   8 
TIDataRevisedAna$Hearing_Aid_Help<-as.factor(TIDataRevisedAna$Hearing_Aid_Help)
TIDataRevisedAna$Hearing_Aid_Help<-relevel(TIDataRevisedAna$Hearing_Aid_Help,ref="6")

# > table(TIDataRevisedAna$Hearing_Aid_Help)
# 
# 6   1   2   3   4   5 
# 159  11  18  20  18   2 
TIDataRevisedAna$TypeTinn_LowPitch<-as.factor(TIDataRevisedAna$TypeTinn_LowPitch)
# > table(TIDataRevisedAna$TypeTinn_LowPitch)
# 
# 0   1 
# 212  16 

levels(TIDataRevisedAna$TypeTinn_LowPitch)
table(TIDataRevisedAna$TypeTinn_LowPitch)

TIDataRevisedAna$Employment_Type<-as.factor(TIDataRevisedAna$Employment_Type)
#> table(TIDataRevisedAna$Employment_Type)
# 
# 1  2  3  4  5  6  7  8  9 10 11 12 
# 27 46 13 17 11 11  6  8  4  1 73 11

dim(TIDataRevisedAna) #228 by 48
str(TIDataRevisedAna)

TIDataRevisedAna2<-TIDataRevisedAna[,-c(1,3,9:12)]
head(TIDataRevisedAna2)
dim(TIDataRevisedAna2) ## 228 by 42
str(TIDataRevisedAna2)

#Visualizing Missing Data Graph 1
install.packages("naniar")
library(naniar)
vis_miss(TIDataRevisedAna2)

#Visualizing Missing Data Graph 2
install.packages("UpSetR")
library(UpSetR)
gg_miss_upset(TIDataRevisedAna2,nsets = n_var_miss(TIDataRevisedAna2))
n_var_miss(TIDataRevisedAna2)
is.na(TIDataRevisedAna2)
sum(is.na(TIDataRevisedAna2$POST_TFISCore)) # 98 is missing

head(TIDataRevisedAna2)
dim(TIDataRevisedAna2)#228 by 42
str(TIDataRevisedAna2)

TIDataRevisedAnaCtsVar2 <- TIDataRevisedAna2[,c(6:7,12,21:27,46)]
str(TIDataRevisedAnaCtsVar2)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(TIDataRevisedAnaCtsVar2,histogram=TRUE, pch=19,color="blue")

# Correlated variables
library(caret)
library(GGally)


findCorrelation(round(cor(TIDataRevisedAnaCtsVar2),4),cutoff = 0.75,verbose =FALSE,name =TRUE)
ggpairs(TIDataRevisedAnaCtsVar2) #This gives a better visulaization

ggpairs(TIDataRevisedAnaCtsVar2, columns = 1:10, ggplot2::aes(colour=TFIReductionCat),method="spearman") 

M <- cor(TIDataRevisedAnaCtsVar2[,-c(4,11)],method="kendall")
M

library(corrplot)
corrplot(M, order = "hclust", addrect = 2)

res1 <- cor.mtest(TIDataRevisedAnaCtsVar2[,-c(4,11)], conf.level = .95,method="kendall")
corrplot(M, p.mat = res1$p, insig = "blank")
corrplot(M, p.mat = res1$p, insig = "p-value")
corrplot(M, p.mat = res1$p, insig = "p-value", sig.level = -1)

round(cor(TIDataRevisedAnaCtsVar2),4)
sum(is.na(TIDataRevisedAna$TFI2monthFollowup_Score)) #77
is.na(TIDataRevisedAna2$TFI_Reduction)
str(TIDataRevisedAna2)

sum(is.na(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Control",2])) #Column 2 is PRE_TFIScore
sum(is.na(TIDataRevisedAna2[TIDataRevisedAna2$Groups!="Control",3])) #Column 3 is POST_TFISCore 25 of them are missing
sum(is.na(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Control",4])) #Column 4 is TFI2monthFollowup_Score 12 are missing
sum(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Control",5]==0) #Column 5 is NewPost_TFIScore 13 are missing
sum(TIDataRevisedAna2[TIDataRevisedAna2$Groups!="Control",5]==0)  # 25 data are missing
> table(TIDataRevisedAna2$Groups)
# 
# Control iCBT group Phase 3              Pilot         Treatment  
# 73                 46                 40                 69 
levels(TIDataRevisedAna2$Groups)

#****************************************************************************
#Data Imputation for Post_TFI Scores based on the groups
#*****************************************************************************
mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Control",4], na.rm = TRUE)#40.5377
mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="iCBT group Phase 3",3], na.rm = TRUE)#29.39535
mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Pilot",3], na.rm = TRUE)#38.7871
mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Treatment ",3], na.rm = TRUE)#35.40714

TIDataRevisedAna2$NewPost_TFIScore2<-TIDataRevisedAna2$NewPost_TFIScore

TIDataRevisedAna2$NewPost_TFIScore2[TIDataRevisedAna2$Groups=="Control" & TIDataRevisedAna2$NewPost_TFIScore2==0] <- mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Control",4], na.rm = TRUE)
TIDataRevisedAna2$NewPost_TFIScore2[TIDataRevisedAna2$Groups=="iCBT group Phase 3" & TIDataRevisedAna2$NewPost_TFIScore2==0]<-mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="iCBT group Phase 3",3], na.rm = TRUE)
TIDataRevisedAna2$NewPost_TFIScore2[TIDataRevisedAna2$Groups=="Pilot" & TIDataRevisedAna2$NewPost_TFIScore2==0]<-mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Pilot",3], na.rm = TRUE)
TIDataRevisedAna2$NewPost_TFIScore2[TIDataRevisedAna2$Groups=="Treatment " & TIDataRevisedAna2$NewPost_TFIScore2==0]<-mean(TIDataRevisedAna2[TIDataRevisedAna2$Groups=="Treatment ",3], na.rm = TRUE)

TIDataRevisedAna2$NewPost_TFIScore2<-round(TIDataRevisedAna2$NewPost_TFIScore2,1)#rounding up to the first decimal place

TIDataRevisedAna2$GroupNew[TIDataRevisedAna2$Groups=="Control"]<-"Control"
TIDataRevisedAna2$GroupNew[TIDataRevisedAna2$Groups!="Control"]<-"Treatment"
TIDataRevisedAna2$GroupNew<-as.factor(TIDataRevisedAna2$GroupNew)
table(TIDataRevisedAna2$GroupNew)
# > table(TIDataRevisedAna2$GroupNew)
# 
# Control Treatment 
# 73       155 
table(TIDataRevisedAna2$Groups)
table(TIDataRevisedAna2$Gender)
# > table(TIDataRevisedAna2$Gender)
# 
# 1   2 
# 130  98 
summary(TIDataRevisedAna2)
hist(TIDataRevisedAna2$PRE_TFIScore)

sum(is.na(TIDataRevisedAna2$POST_TFISCore))#98
sum(is.na(TIDataRevisedAna2$POST_TFISCore & TIDataRevisedAna2$TFI2monthFollowup_Score))#144
sum(is.na(TIDataRevisedAna2$NewPost_TFIScore))#0



meandata<-apply(TIDataRevisedAna2[,c(2:7,12,21:27)], 2, mean,na.rm=TRUE)
meandata
sddata<-apply(TIDataRevisedAna2[,c(2:7,12,21:27)], 2, sd,na.rm=TRUE)
sddata

TIDataRevisedAna2[65:80,]

#**********************************************************************
#New TFI Score Reduction Variable (This will be used as the response)
#**********************************************************************
TIDataRevisedAna2$TFIReduction_New<-TIDataRevisedAna2$PRE_TFIScore-TIDataRevisedAna2$NewPost_TFIScore2
TIDataRevisedAna2$TFIReductionCat[TIDataRevisedAna2$TFIReduction_New>=13]<-"1"  # "Succsess"
TIDataRevisedAna2$TFIReductionCat[TIDataRevisedAna2$TFIReduction_New<13]<-"0"  # Failure


TIDataRevisedAna2$TFIReductionCat<-as.factor(TIDataRevisedAna2$TFIReductionCat)


table(TIDataRevisedAna2$TFIReductionCat)  #150 sucesses and 78 failures




#*************************************************************************
#*Creating Categorical Variables for Clinical Factors in the Analysis
#************************************************************************
TIDataRevisedAna2$Anxiety<-ifelse(TIDataRevisedAna2$Higher_Anxiety>10,1,0)
TIDataRevisedAna2$Depression<-ifelse(TIDataRevisedAna2$Higher_Depression>10,1,0)
TIDataRevisedAna2$Insomnia<-ifelse(TIDataRevisedAna2$Higher_Insomnia>14,1,0)

TIDataRevisedAna2$Anxiety<-as.factor(TIDataRevisedAna2$Anxiety)
TIDataRevisedAna2$Depression<-as.factor(TIDataRevisedAna2$Depression)
TIDataRevisedAna2$Insomnia<-as.factor(TIDataRevisedAna2$Insomnia)

TIDataRevisedAna2$Anxiety_c<-as.factor(ifelse(TIDataRevisedAna2$Higher_Anxiety>=10,1,0))
TIDataRevisedAna2$Depression_c<-as.factor(ifelse(TIDataRevisedAna2$Higher_Depression>=15,1,0))
TIDataRevisedAna2$Insomnia_c<-as.factor(ifelse(TIDataRevisedAna2$Higher_Insomnia>=15,1,0))

TIDataRevisedAna2$Hyperacusis_c<-as.factor(ifelse(TIDataRevisedAna2$Hyperacusis>=29,1,0))
TIDataRevisedAna2$HHIA_c<-as.factor(ifelse(TIDataRevisedAna2$Higher_Hearing_Disability>=10,1,0))
TIDataRevisedAna2$CFQ_c<-as.factor(ifelse(TIDataRevisedAna2$CFQ>=33,1,0))
TIDataRevisedAna2$SWLS_c<-as.factor(ifelse(TIDataRevisedAna2$SWLS>=20,1,0))

dim(TIDataRevisedAna2) # 228 by 49
str(TIDataRevisedAna2)

#*******************************
#*****Correlation Analysis******
install.packages("corrplot")
library("corrplot")
library(RColorBrewer)

M<-cor(TIDataRevisedAnaCtsVar2)
head(round(M,2))

corrplot(M, type="upper")
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="PuOr"))
#Chi square test
library(MASS)       # load the MASS package 
tbl = table(TIDataRevisedAna2$GroupNew,TIDataRevisedAna2$TFIReductionCat)
tbl  
chisq.test(tbl)
#Use TIDataRevisedAna2$NewPost_TFIScore2 as the final NewPostTFI Scores.
TIDataRevisedAna2[65:80,]
head(TIDataRevisedAna2)
dim(TIDataRevisedAna2)  #228 by 58





#*********************************************************************************************************
#**************************************CART Decision Tree Models*********************************************
#*********************************************************************************************************
#To get help
?rpart()
#rparttreeRevised.modelnew is the revised model ran based on all binary variables (all clinical factors and several other) considered in AI paper 1
#training14 contains the training data with new variables
##Decision Tree on RV1
library(rpart)

set.seed(745)
rparttreeRevised.modelnew<- rpart(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                                    Loud._Noise_Exposure+Psychological_Condition+Work_Less+
                                    PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                                    TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                                    TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                                    Multi_Sounds+Hearing_Loss+
                                    Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                                    Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c, data=training14,parms=list(split="gini"),
                               control=rpart.control(minsplit = 20,minbucket = round(20/3),xval = 3,maxdepth = 10))

rparttreeRevised.modelnew
summary(rparttreeRevised.modelnew)

rparttreeRevised.modelnew$variable.importance #Feature Importance based on the regular rpart algorithm.

#Plotting the Tree
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(rparttreeRevised.modelnew,type=3,under = TRUE,
           space = 0,tweak=0.8,under.cex = 1,fallen.leaves = TRUE,branch.lty = 3)
#Making Predictions using Test Data
tree.pred1new=predict(rparttreeRevised.modelnew,testing14,type="class")
tree.pred1new[1:10]

A1new<-table(testing14$TFIReductionCat,tree.pred1new)
A1new
Accuracy1new=(A1new[1]+A1new[4])/sum(A1new)
Accuracy1new

#**Plotting the ROC Curve for Rpart Decison Tree Model.
library(pROC)
predictions_DT0new <- as.data.frame(predict(rparttreeRevised.modelnew,testing14, type = "prob"))
predictions_DT0new$predict <- names(predictions_DT0new)[1:2][apply(predictions_DT0new[,1:2],1, which.max)]
predictions_DT0new$observed <- testing14$TFIReductionCat

roc.DT0new <- roc(ifelse(predictions_DT0new$observed=="1", "1", "0"),
               as.numeric(predictions_DT0new$`1`), grid=T, plot=T,
               print.auc =T, col ="red")

#Pruned Tree with Best Cp Score (However, this pruning did not improve the model)
cp_goodRevisednew<-rparttreeRevised.modelnew$control$cp
cp_goodRevisednew

rparttreeRevised.modelnewPruned<-prune(rparttreeRevised.modelnew, cp =cp_goodRevised,FUN=prune.tree)
rparttreeRevised.modelnewPruned

library(rpart.plot)
rpart.plot(rparttreeRevised.modelnewPruned,type=3,under = TRUE,
           space = 0,tweak=0.8,under.cex = 1,fallen.leaves = TRUE,branch.lty = 3)
#Making Predictions using Test Data
treeRevisednewPruned.pred1=predict(rparttreeRevised.modelnewPruned,testing14,type="class")

A2new<-table(testing14$TFIReductionCat,treeRevisednewPruned.pred1)
A2new
Accuracy2new=(A2new[1]+A2new[4])/sum(A2new)
Accuracy2new


#*********************************************************************************************************
#***********************Gradient Boosting Algorithm*******************************************************
#********************************************************************************************************

library("caret")

#On RV1
set.seed(7882)
tc = trainControl(method = "repeatedcv", number = 3)

#training12Boost<- training12[,-c(16:18,37)]

#boost.model = train(TFIReductionCat~.-Higher_Anxiety-Higher_Depression-Higher_Insomnia-TFIReduction_New,data =training12, method="gbm", trControl=tc)

boost.modelnew = train(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                         Loud._Noise_Exposure+Psychological_Condition+Work_Less+
                         PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                         TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                         TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                         Multi_Sounds+Hearing_Loss+
                         Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                         Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                       data =training14, method="gbm", trControl=tc)

boost.prednew = predict(boost.modelnew,testing14)
boost.resultnew = data.frame(testing14,boost.prednew)
print(boost.resultnew)


boost_cmnew = confusionMatrix(testing14$TFIReductionCat, as.factor(boost.prednew),positive = "1")
print(boost_cmnew)

library(pROC)
# library(ROCR)
# library(PRROC)
predictions_boost1new <- as.data.frame(predict(boost.modelnew,testing14, type = "prob"))
predictions_boost1new$predict <- names(predictions_boost1new)[1:2][apply(predictions_boost1new[,1:2],1, which.max)]
predictions_boost1new$observed <- testing14$TFIReductionCat
head(predictions_boost1new)
roc.rfboost1new <- roc(ifelse(predictions_boost1new$observed=="1", "1", "0"),
                    as.numeric(predictions_boost1new$'1'), grid=T, plot=T,
                    print.auc =T, col ="red")





#*************************************************************************************
#******************* Random Forest Models*********************************************
#*************************************************************************************


library(randomForest)

set.seed(123)

library(e1071)
library(StMoMo)
library(demography)
library(rcompanion)
library(forecast)
# #2 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=2, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "Accuracy"
set.seed(123)
# #Number randomely variable selected is mtry
#mtry <- sqrt(ncol(training14)-2) #used in the Old analysis
mtry<-sqrt(33) # As we 33 predictor variables
tunegrid <- expand.grid(.mtry=mtry)
rf_defaultnew <- train(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                         Loud._Noise_Exposure+Psychological_Condition+Work_Less+
                         PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                         TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                         TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                         Multi_Sounds+Hearing_Loss+
                         Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                         Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                       data=training14, 
                    method='rf', 
                    tuneGrid=tunegrid,
                    metric='Accuracy', 
                    trControl=control)
print(rf_defaultnew)
varImp(rf_defaultnew)

rf_default.prednew<-predict(rf_defaultnew, testing14)
rf_default.predProbnew<-predict(rf_defaultnew, testing14,type="prob")
rf_default.predProbnew
rf_default.predCustomClassnew<-as.factor(ifelse(rf_default.predProbnew[,2]>0.55,1,0))
rf_default.predCustomClassnew
table(testing14$TFIReductionCat,rf_default.predCustomClassnew)

confusionMatrix(testing14$TFIReductionCat,rf_default.predCustomClassnew,positive = "1")
confusionMatrix(testing14$TFIReductionCat,rf_default.prednew,positive = "1")

conf.matrixnew<-table(testing14$TFIReductionCat,rf_default.predCustomClassnew)
conf.matrixnew
sensitivity(conf.matrixnew)
specificity(conf.matrixnew)



n <- sum(conf.matrix)
diag <- diag(conf.matrix)
accuracy <- sum(diag) / n
accuracy

library(pROC)
library(ROCR)
library(PRROC)
# predict test set, get probs instead of response
predictions <- as.data.frame(predict(rf_defaultnew,testing14, type = "prob"))
predictions$predict <- names(predictions)[1:2][apply(predictions[,1:2],1, which.max)]
predictions$observed <- testing14$TFIReductionCat
head(predictions)
roc.rf <- roc(ifelse(predictions$observed=="1", "1", "0"),
              as.numeric(predictions$'1'), grid=T, plot=T,
              print.auc =T, col ="red")
#AUC: 0.611

#  Second RF Model without PRE TFI Score
metric <- "Accuracy"
set.seed(123)
# #Number randomely variable selected is mtry
#mtry <- sqrt(ncol(training14)-2) #used in the Old analysis

mtry<-sqrt(33) # As we 33 predictor variables
tunegrid <- expand.grid(.mtry=mtry)
rf_defaultnew2 <- train(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                         Loud._Noise_Exposure+Psychological_Condition+Work_Less-
                         PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                         TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                         TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                         Multi_Sounds+Hearing_Loss+
                         Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                         Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                       data=training14, 
                       method='rf', 
                       tuneGrid=tunegrid,
                       metric='Accuracy', 
                       trControl=control)
print(rf_defaultnew2)
varImp(rf_defaultnew2)

rf_default.prednew2<-predict(rf_defaultnew2, testing14)
rf_default.predProbnew2<-predict(rf_defaultnew2, testing14,type="prob")
rf_default.predProbnew2
rf_default.predCustomClassnew2<-as.factor(ifelse(rf_default.predProbnew2[,2]>0.55,1,0))
rf_default.predCustomClassnew2
table(testing14$TFIReductionCat,rf_default.predCustomClassnew2)

confusionMatrix(rf_default.predCustomClassnew2,testing14$TFIReductionCat,positive = "1")
confusionMatrix(testing14$TFIReductionCat,rf_default.prednew2,positive = "1")

conf.matrixnew2<-table(testing14$TFIReductionCat,rf_default.predCustomClassnew2)
conf.matrixnew2
sensitivity(conf.matrixnew2)
specificity(conf.matrixnew2)



n <- sum(conf.matrix)
diag <- diag(conf.matrix)
accuracy <- sum(diag) / n
accuracy

library(pROC)

# predict test set, get probs instead of response
predictions <- as.data.frame(predict(rf_defaultnew,testing14, type = "prob"))
predictions$predict <- names(predictions)[1:2][apply(predictions[,1:2],1, which.max)]
predictions$observed <- testing14$TFIReductionCat
head(predictions)
roc.rf <- roc(ifelse(predictions$observed=="1", "1", "0"),
              as.numeric(predictions$'1'), grid=T, plot=T,
              print.auc =T, col ="red")
#AUC: 0.608

#Predictions without PRE TFI SCore
predictions2 <- as.data.frame(predict(rf_defaultnew2,testing14, type = "prob"))
predictions$predict2 <- names(predictions2)[1:2][apply(predictions2[,1:2],1, which.max)]
predictions$observed <- testing14$TFIReductionCat
head(predictions2)
roc.rf <- roc(ifelse(predictions$observed=="1", "1", "0"),
              as.numeric(predictions2$'1'), grid=T, plot=T,
              print.auc =T, col ="red")
#AUC: 0.640

library("iml")
set.seed(42)
#ZX <- training12[,-c(16:18,37:38)]
ZX <- training14[,-c(16:18,37:38)]

mod <- Predictor$new(boost.model, data = ZX, type = "prob")
# Then we explain the first instance of the dataset with the Shapley() method:
shapley <- Shapley$new(mod, x.interest=ZX[1,])
shapley$results
plot(shapley)



#*************************************************************************************
#*******************AdaBag Boosting *************************************************
#*************************************************************************************
library(adabag)
set.seed(7882)
#adabo<-boosting(TFIReductionCat~.-Higher_Anxiety-Higher_Depression-Higher_Insomnia-TFIReduction_New, data=training12,boos=TRUE,mfinal = 50)


adabo<-boosting(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                  Loud._Noise_Exposure+Psychological_Condition+Work_Less+
                  PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                  TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                  TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                  Multi_Sounds+Hearing_Loss+
                  Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                  Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,training14,boos=TRUE,mfinal = 50)

print(adabo$trees[50])
sort(adabo$importance)

importanceplot(adabo)
pred_adabo<-predict(adabo,testing14)
print(pred_adabo$votes)
print(pred_adabo$confusion)

conf.matrix_adaboost<-table(testing14$TFIReductionCat,pred_adabo$class)
conf.matrix_adaboost
sensitivity(conf.matrix_adaboost)
specificity(conf.matrix_adaboost)

accuracy_adaboo <- sum(diag(conf.matrix_adaboost)) / sum(conf.matrix_adaboost)
accuracy_adaboo 

AdaboostOrigClassConvertion<-ifelse(as.factor(pred_adabo$class)==2,1,0)
confusionMatrix(testing14$TFIReductionCat,as.factor(AdaboostOrigClassConvertion),positive = "1")

library(pROC)
library(ROCR)
library(PRROC)
# predict test set, get probs instead of response
predictions_adaboost <- as.data.frame(pred_adabo$prob)
colnames(predictions_adaboost)<-c("0","1")
predictions$predict_adaboost <- names(predictions_adaboost)[1:2][apply(predictions_adaboost[,1:2],1, which.max)]
predictions$observed <- testing14$TFIReductionCat
head(predictions_adaboost)
roc_adaboost<- roc(ifelse(predictions$observed=="1", "1", "0"),
                   as.numeric(predictions_adaboost$'1'), grid=T, plot=T,
                   print.auc =T, col ="red")
#AUC: 0.578



#Variables Included in Jan 15th 2021 Analysis for Paper 1 and 2

# 1.Cat_Age+
# 2.Gender+
# 3. Education_Level+
# 4. Employment_Type+
# 5. Loud._Noise_Exposure+
# 6. Psychological_Condition+
# 7. Work_Less-

# 8. PRE_TFIScore_c+
# 9. Cat_TinnDuration+
# 10. How_of_Tinnitus_Heard+
# 11. Tinnitus_Location+
# 12. TypeTinn_Ringing+
# 13. TypeTinn_Buzzing
# 14. TypeTinn_HighPitch+
# 15. TypeTinn_LowPitch+
# 16. TypeTinn_Pulsing+
# 17. TypeTinn_Clicking+
# 18. TypeTinn_Music
# 19. TypeTinn_Voices+
# 20. TypeTinn_Humming+
# 21. Multi_Sounds
# 22. Hearing_Loss+
# 23. Past_Treatment+
# 24. Sounds.can.distract+
# 25. HearingAid_usage+
# 26. Medication_for_Tinnitus+

# 27. Anxiety_c+
# 28. Depression_c+
# 29. Insomnia_c+
# 30. Hyperacusis_c+
# 31. HHIA_c+
# 32. CFQ_c+
# 33. SWLS_c
#############################################################
###########################ANN Analysis #######################
#############################################################

trainingANN<-training14[,-c(1:3,5,7,16:22,36:37,39,41:43)]
testingANN<-testing14[,-c(1:3,5,7,16:22,36:37,39,41:43)]

names(trainingANN)
dim(trainingANN)

nnfit.revised1<- train(trainingANN[,-c(25)],trainingANN[,25], method = "nnet",
                       trControl = trainControl(method = "cv",number=3), act.fct = "logistic") # Column 25 is the trainingANN$TFIReductionCat

plot(nnfit.revised1)
nn.Revpred1 <- predict(nnfit.revised1,testingANN)
head(nn.Revpred1)
confusionMatrix(testingANN$TFIReductionCat,nn.Revpred1,positive = "1")

nn.Revpred2Prob <- predict(nnfit.revised1,testingANN, type='prob')
head(nn.Revpred2Prob)
ANN.predCustomClass<-as.factor(ifelse(nn.Revpred2Prob[,2]>0.35,1,0))
ANN.predCustomClass
table(testing14$TFIReductionCat,ANN.predCustomClass)

confusionMatrix(testing14$TFIReductionCat,ANN.predCustomClass,positive = "1")


nn.Revpred2.frame<-as.data.frame(nn.Revpred2Prob)
nn.Revpred2.prob <- as.numeric(nn.Revpred2.frame[,2])
nn.Revroc1<- roc(as.numeric(testingANN$TFIReductionCat),nn.Revpred2.prob, grid=T,
                 plot=T, print.auc =T, col ="red")

#AUC: 0.713




#*******************************************************************
#***************************Support Vector Machines*****************

library(e1071)

#On RV1
set.seed(4677)
svm.Revlinear <- tune(svm,TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                        Loud._Noise_Exposure+Psychological_Condition+Work_Less-
                        PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                        TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                        TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                        Multi_Sounds+Hearing_Loss+
                        Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                        Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                      data=training14,kernel = "linear",ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000)))
summary(svm.Revlinear)

##For a linear kernel,A cost of 0.01 seems to perform best.
set.seed(4677)
svm.linearRevbest <- svm(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                           Loud._Noise_Exposure+Psychological_Condition+Work_Less-
                           PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                           TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                           TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                           Multi_Sounds+Hearing_Loss+
                           Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                           Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                         data=training14, kernel = "linear", 
                         cost = 0.01,probability = TRUE)
summary(svm.linearRevbest)
plot(svm.linearRevbest,training14)

training14$TFIReductionCat<-as.factor(training14$TFIReductionCat)

SVM_model1_Revpred<-predict(svm.linearRevbest,newdata=testing14)
confusionMatrix(testing14$TFIReductionCat,SVM_model1_Revpred,positive = "1")

SVM.Revpred1Prob <- predict(svm.linearRevbest,testing14, decision.values = TRUE, probability = TRUE)
attr(SVM.Revpred1Prob, "probabilities")[1:7,]

customConfSVmlinear<-ifelse(attr(SVM.Revpred1Prob, "probabilities")[,1]>0.65,1,0)
table(testing14$TFIReductionCat,customConfSVmlinear)
confusionMatrix(testing14$TFIReductionCat,as.factor(customConfSVmlinear),positive = "1")

SVM.pred1.frame<-as.data.frame(attr(SVM.Revpred1Prob, "probabilities"))
SVM.pred1.prob <- as.numeric(SVM.pred1.frame[,1])
SVMLinear.roc1<- roc(as.numeric(testing14$TFIReductionCat),SVM.pred1.prob , grid=T,
                     plot=T, print.auc =T, col ="red")
#AUC 0.704 So far the best

set.seed(123)
svm.radial <- tune(svm,TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                     Loud._Noise_Exposure+Psychological_Condition+Work_Less-
                     PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                     TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                     TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                     Multi_Sounds+Hearing_Loss+
                     Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                     Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                   data=training14, kernel = "radial", 
                   ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100), gamma = c(0.01, 0.1,1, 5, 10, 100)))
summary(svm.radial)

#For a radial kernel, the lowest cross-validation error is obtained for a gamma of 0.01 and a cost of 0.01.
set.seed(4553)
svm.radialbest <- svm(TFIReductionCat~Cat_Age+Gender+Education_Level+Employment_Type+
                        Loud._Noise_Exposure+Psychological_Condition+Work_Less-
                        PRE_TFIScore_c+Cat_TinnDuration+How_of_Tinnitus_Heard+Tinnitus_Location+
                        TypeTinn_Ringing+TypeTinn_Buzzing+TypeTinn_HighPitch+TypeTinn_LowPitch+
                        TypeTinn_Pulsing+TypeTinn_Clicking+TypeTinn_Music+TypeTinn_Voices+TypeTinn_Humming+
                        Multi_Sounds+Hearing_Loss+
                        Past_Treatment+Sounds.can.distract+HearingAid_usage+Medication_for_Tinnitus+
                        Anxiety_c+Depression_c+Insomnia_c+Hyperacusis_c+HHIA_c+CFQ_c+SWLS_c,
                      data=training14, kernel = "radial",
                      cost = 0.01, gamma = 0.01,probability = TRUE)
summary(svm.radialbest)

SVM_Radialmodel1_pred<-predict(svm.radialbest,newdata=testing14)
SVM_Radialmodel1_pred
confusionMatrix(testing14$TFIReductionCat,SVM_Radialmodel1_pred,positive = "1")

SVM.Radialpred1Prob <- predict(svm.radialbest,testing14, decision.values = TRUE, probability = TRUE)
SVM.Radialpred1Prob

attr(SVM.Radialpred1Prob , "probabilities")[1:7,1]

#This cut off was taken as of Jan 15, 2021 Analysis
customConfSVmRadial<-ifelse(attr(SVM.Radialpred1Prob, "probabilities")[,1]>0.65,1,0)
table(testing14$TFIReductionCat,customConfSVmRadial)

SVM.Radialpred1.frame<-as.data.frame(attr(SVM.Radialpred1Prob , "probabilities"))
SVM.Radialpred1.prob <- as.numeric(SVM.Radialpred1.frame[,1])
SVMRadial.roc1<- roc(as.numeric(testing14$TFIReductionCat),SVM.Radialpred1.prob, grid=T,
                     plot=T, print.auc =T, col ="red")

table(true=testing14$TFIReductionCat, pred_svmlinear=predict(svm.linearbest,newdata=testing14))

table(true=testing14$TFIReductionCat, pred_svmradial=predict(svm.radialbest,newdata=testing14))

#AUC: 0.676



#*************************************************************
#*************SHAP Analysis***********************************
#*************************************************************

devtools::install_github('christophM/iml')

set.seed(42)
library("iml")
library("randomForest")


data("Boston", package = "MASS")
rf_boston <- randomForest(medv ~ ., data = Boston, ntree = 50)

XSA <- Boston[which(names(Boston) != "medv")]
predictor_XSA <- Predictor$new(rf_boston, data = XSA, y = Boston$medv)

imp <- FeatureImp$new(predictor_XSA, loss = "mae")
library("ggplot2")
plot(imp)

training14$TFIReductionCat<-ifelse(training14$TFIReductionCat==1)
trainingANN<-training14[,-c(1:3,5,7,16:22,36:37,39,41:43)]
testingANN<-testing14[,-c(1:3,5,7,16:22,36:37,39,41:43)]

predictor_XSA2 <- Predictor$new(rf_defaultnew, data = training14[,-c(1:3,5,7,16:22,36:37,39,41:43)], y = training14$TFIReductionCat)
imp2 <- FeatureImp$new(predictor_XSA2, loss = "ce")
plot(imp2)
imp2$results

predictor_XSA3 <- Predictor$new(boost.modelnew, data = training14[,-c(1:3,5,7,16:22,36:37,39,41:43)], y = training14$TFIReductionCat)
imp3 <- FeatureImp$new(predictor_XSA3, loss = "ce")
plot(imp3)
imp3$results

predictor_XSA4 <- Predictor$new(rparttreeRevised.modelnew, data = training14[,-c(1:3,5,7,16:22,36:37,39,41:43)], y = training14$TFIReductionCat)
imp4 <- FeatureImp$new(predictor_XSA4, loss = "ce")
plot(imp4)
imp4$results

predictor_XSA5 <- Predictor$new(nnfit.revised1, data = trainingANN, y = trainingANN$TFIReductionCat)
imp5 <- FeatureImp$new(predictor_XSA5, loss = "ce")
plot(imp5)
imp5$results

pdf(file = "SHAP_CART_Features.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
library(patchwork)
effs4 <- FeatureEffects$new(predictor_XSA4)
plot(effs4)
lines(lowess(effs4), col = 2)

# Step 3: Run dev.off() to create the file!
dev.off()


pdf(file = "SHAP_RF_Features.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
library(patchwork)
effs2 <- FeatureEffects$new(predictor_XSA2)
plot(effs2)
lines(lowess(effs2), col = 2)

# Step 3: Run dev.off() to create the file!
dev.off()
pdf(file = "SHAP_GB_Features.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
library(patchwork)
effs3 <- FeatureEffects$new(predictor_XSA3)
plot(effs3)

# Step 3: Run dev.off() to create the file!
dev.off()


# Step 3: Run dev.off() to create the file!
dev.off()



pdf(file = "SHAP_ANN_Features.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 10) # The height of the plot in inches

# Step 2: Create the plot with R code
library(patchwork)
effs5 <- FeatureEffects$new(predictor_XSA5)
plot(effs5)

# Step 3: Run dev.off() to create the file!
dev.off()

interact_rfoverall <- Interaction$new(predictor_XSA2)
plot(interact_rfoverall)

interact_GBoverall <- Interaction$new(predictor_XSA3)
plot(interact_GBoverall)

interact_EduLevel <- Interaction$new(predictor_XSA2, feature = "Education_Level")
plot(interact_EduLevel)

interact_PreTFI<- Interaction$new(predictor_XSA2, feature = "PRE_TFIScore_c")
plot(interact_PreTFI)

interact_HearingAid_usage<- Interaction$new(predictor_XSA2, feature = "HearingAid_usage")
plot(interact_HearingAid_usage)

interact_Tinnitus_Location<- Interaction$new(predictor_XSA2, feature = "Tinnitus_Location")
plot(interact_Tinnitus_Location)

interact_How_of_Tinnitus_Heard<- Interaction$new(predictor_XSA2, feature = "How_of_Tinnitus_Heard")
plot(interact_How_of_Tinnitus_Heard)

interact_Hearing_Loss<- Interaction$new(predictor_XSA2, feature = "Hearing_Loss")
plot(interact_Hearing_Loss)


interact_EduLevel_GB <- Interaction$new(predictor_XSA3, feature = "Education_Level")
plot(interact_EduLevel_GB)

interact_Sounds.can.distract_GB <- Interaction$new(predictor_XSA3, feature = "Sounds.can.distract")
plot(interact_Sounds.can.distract_GB)

interact_Anxiety_c_GB <- Interaction$new(predictor_XSA3, feature = "Anxiety_c")
plot(interact_Anxiety_c_GB)

interact_PRE_TFIScore_c_GB <- Interaction$new(predictor_XSA3, feature = "PRE_TFIScore_c")
plot(interact_PRE_TFIScore_c_GB)

interact_Employment_Type_GB <- Interaction$new(predictor_XSA3, feature = "Employment_Type")
plot(interact_Employment_Type_GB)

interact_Hearing_Loss_GB <- Interaction$new(predictor_XSA3, feature = "Hearing_Loss")
plot(interact_Hearing_Loss_GB)

#SHAP Analysis
#Source: https://cran.r-project.org/web/packages/iml/vignettes/intro.html
# New objects can be created by calling Predictor$new().

library("iml")
set.seed(42)
ZX <- training14[,-c(1:3,5,7,16:22,36:37,39,41:43)]
# 
mod <- Predictor$new(rf_defaultnew, data = ZX, type = "prob")
mod_GB<- Predictor$new(boost.modelnew, data = ZX, type = "prob")
mod_ANN<- Predictor$new(nnfit.revised1, data = ZX, type = "prob")
mod_CART<- Predictor$new(rparttreeRevised.modelnew, data = ZX, type = "prob")

#*** For best RF Model SHAP values Analysis:

strore_RF<-array(0,c(183,33,1))
for (i in 1:dim(ZX)[1]){
  shapley_RF <- Shapley$new(mod, x.interest=ZX[i,])
  for (j in 1:33){
    strore_RF[i,j,1]<-abs(shapley_RF$results[33+j,3])
  }}

Mean_values<-array(c(0),dim=c(33,1))
for (j in 1:33){
  Mean_values[j]<-mean(strore_RF[,j,1])
}

varnames<-shapley_RF$results[34:66,1]

Abs_SHAP<-round(Mean_values,6)

full_RF_SHAP<-as.data.frame(cbind(varnames,Abs_SHAP))
full_RF_SHAP
write.csv(full_RF_SHAP,"full_RF_SHAP.csv")

library("ggplot2")
data <- data.frame(Abs_SHAP, varnames)    
RF_SHAP_Plot <-ggplot(data,aes(x= reorder(varnames,Abs_SHAP),Abs_SHAP))+geom_bar(stat ="identity",color='skyblue',fill='steelblue')+coord_flip()
RF_SHAP_Plot
#RF_SHAP_Plot<-RF_SHAP_Plot+theme(axis.text.x=element_text(angle=45, hjust=1))


#***********************************************
#  For best CART Tree Model SHAP values Analysis:
#***********************************************

strore_CART<-array(0,c(183,33,1))
for (i in 1:dim(ZX)[1]){
  shapley_CART <- Shapley$new(mod_CART, x.interest=ZX[i,])
  for (j in 1:33){
    strore_CART[i,j,1]<-abs(shapley_CART$results[33+j,3])
  }}

Mean_values_CART<-array(c(0),dim=c(33,1))
for (j in 1:33){
  Mean_values_CART[j]<-mean(strore_CART[,j,1])
}

varnames<-shapley_CART$results[34:66,1]

Abs_SHAP_CART<-round(Mean_values_CART,6)

full_CART_SHAP<-as.data.frame(cbind(varnames,Abs_SHAP_CART))
full_CART_SHAP
write.csv(full_CART_SHAP,"full_CART_SHAP.csv")

library("ggplot2")
data_CART <- data.frame(Abs_SHAP_CART, varnames)    
CART_SHAP_Plot <-ggplot(data_CART,aes(x= reorder(varnames,Abs_SHAP_CART),Abs_SHAP_CART))+geom_bar(stat ="identity",color='skyblue',fill='steelblue')+coord_flip()
CART_SHAP_Plot


#********************************************
## For best GB Model SHAP values Analysis:
#*********************************************
strore_GB<-array(0,c(183,33,1))
for (i in 1:dim(ZX)[1]){
  shapley_GB <- Shapley$new(mod_GB, x.interest=ZX[i,])
  for (j in 1:33){
    strore_GB[i,j,1]<-abs(shapley_GB$results[33+j,3])
  }}

Mean_values_GB<-array(c(0),dim=c(33,1))
for (j in 1:33){
  Mean_values_GB[j]<-mean(strore_GB[,j,1])
}

varnames_GB<-shapley_GB$results[34:66,1]

Abs_SHAP_GB<-round(Mean_values_GB,6)

full_GB_SHAP<-as.data.frame(cbind(varnames_GB,Abs_SHAP_GB))
full_GB_SHAP
write.csv(full_GB_SHAP,"full_GB_SHAP.csv")

library("ggplot2")
data_GB <- data.frame(Abs_SHAP_GB, varnames_GB)    
GB_SHAP_Plot <-ggplot(data_GB,aes(x= reorder(varnames_GB,Abs_SHAP_GB),Abs_SHAP_GB))+geom_bar(stat ="identity",color='skyblue',fill='steelblue')+coord_flip()
GB_SHAP_Plot

#********************************************
## For best ANN Model SHAP values Analysis:
#*********************************************
strore_ANN<-array(0,c(183,33,1))
for (i in 1:dim(ZX)[1]){
  shapley_ANN <- Shapley$new(mod_ANN, x.interest=ZX[i,])
  for (j in 1:33){
    strore_ANN[i,j,1]<-abs(shapley_ANN$results[33+j,3])
  }}

Mean_values_ANN<-array(c(0),dim=c(33,1))
for (j in 1:33){
  Mean_values_ANN[j]<-mean(strore_ANN[,j,1])
}

varnames_ANN<-shapley_ANN$results[34:66,1]

Abs_SHAP_ANN<-round(Mean_values_ANN,6)

full_ANN_SHAP<-as.data.frame(cbind(varnames_ANN,Abs_SHAP_ANN))
full_ANN_SHAP
write.csv(full_ANN_SHAP,"full_ANN_SHAP.csv")

library("ggplot2")
data_ANN <- data.frame(Abs_SHAP_ANN, varnames_ANN)    
ANN_SHAP_Plot <-ggplot(data_ANN,aes(x= reorder(varnames_ANN,Abs_SHAP_ANN),Abs_SHAP_ANN))+geom_bar(stat ="identity",color='skyblue',fill='steelblue')+coord_flip()
ANN_SHAP_Plot


###Association Data Rules
AssociationData<-TIDataRevisedAna5[,-c(1:3,5,7,16:22,36:37,39,41:43)]
names(AssociationData)

library(arules)
rules <- apriori(AssociationData)
inspect(rules)

rules <- apriori(AssociationData,parameter = list(minlen=2, supp=0.005, conf=0.8),
                 appearance = list(rhs=c("TFIReductionCat=0", "TFIReductionCat=1"),default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)


save.image("AI_TinnitusAnaFinal.RData")
