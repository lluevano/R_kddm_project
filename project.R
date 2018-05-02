#CS513 - Knowledge Distribution and Data Mining
#US Visa dataset

#Uncomment these for the first run and then comment again
#install.packages('rstudioapi')
#install.packages('readr')

rm(list=ls())

library(rstudioapi) 
library(readr)
library(lubridate)

current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

dataset <- read_csv("us_perm_visas.csv")
#summary(dataset)
clean_dataset <- dataset

#start cleaning dataset
#pass values from country of citizenship 2 to the blank rows of country of citizenship 1
clean_dataset$country_of_citizenship = ifelse(is.na(clean_dataset$country_of_citizenship), clean_dataset$country_of_citzenship, clean_dataset$country_of_citizenship)
#eliminate extra row for country of citizenship
clean_dataset <- clean_dataset[,-12]
#eliminate non-meaningful fields
clean_dataset$employer_address_1 <- NULL
clean_dataset$employer_address_2 <- NULL
clean_dataset$employer_phone <- NULL
clean_dataset$employer_phone_ext <- NULL
clean_dataset$case_no <- NULL
clean_dataset$case_number <- NULL
clean_dataset$employer_decl_info_title <- NULL
clean_dataset$employer_state <- NULL

clean_dataset$foreign_worker_info_city <- NULL
clean_dataset$orig_case_no <- NULL
clean_dataset$pw_soc_title <- NULL
clean_dataset$agent_city <- NULL


#check for completeness of the data
NA_percentage_threshold_column <- .3
NA_percentage_threshold_row <- .01
column_names <- colnames(clean_dataset)
for (column_name in column_names){
  NA_percentage <- nrow(clean_dataset[is.na(clean_dataset[,column_name]),column_name]) / nrow(clean_dataset)
  if (NA_percentage > NA_percentage_threshold_column){
    #drop the entire column
    clean_dataset[,column_name] <- NULL
  } else {
    if(NA_percentage<NA_percentage_threshold_row){
      #drop the rows
      clean_dataset <- clean_dataset[!is.na(clean_dataset[,column_name]),]
    }
  }
}

#verify columns with na values
# column_names <- colnames(clean_dataset)
# for (column_name in column_names){
#   NA_percentage <- nrow(clean_dataset[is.na(clean_dataset[,column_name]),column_name]) / nrow(clean_dataset)
#   if (NA_percentage){
#     print(column_name)
#     print(nrow(clean_dataset[is.na(clean_dataset[,column_name]),column_name]))
#   }
# }

#replacing class of admission by mode value
ll<-data.frame(table(clean_dataset$class_of_admission))
mlv_class_of_adm <- ll[which.max(ll$Freq),]
mlv_class_of_adm <- as.character(mlv_class_of_adm$Var1[1])
clean_dataset[is.na(clean_dataset$class_of_admission),"class_of_admission"] <- mlv_class_of_adm

ll<-data.frame(table(clean_dataset$pw_level_9089))
mlv_pw_level <- ll[which.max(ll$Freq),]
mlv_pw_level <- as.character(mlv_pw_level$Var1[1])
clean_dataset[is.na(clean_dataset$pw_level_9089),"pw_level_9089"] <- mlv_pw_level

clean_dataset$employer_city <- NULL
clean_dataset$job_info_work_city <- NULL

unique(clean_dataset$job_info_work_state)
states <- read.csv("USstateAbbreviations.csv")

for (i in 1:length(states$Name)){
  clean_dataset[clean_dataset$job_info_work_state==toupper(states[i,"Name"]),"job_info_work_state"] <- as.character(states[i,"ANSI.letters"])
}

#replace empty postal code for employers with the most frequent one from the job_info_work_state
empty_postal_code_states <- unique(clean_dataset[is.na(clean_dataset$employer_postal_code),"job_info_work_state"])
empty_postal_code_states <- empty_postal_code_states[[1]]
for (empty_pc_state in empty_postal_code_states){
  ll<-data.frame(table(clean_dataset[clean_dataset$job_info_work_state==empty_pc_state,"employer_postal_code"]))
  mlv_postal_code <- ll[which.max(ll$Freq),]
  mlv_postal_code <- as.numeric(mlv_postal_code$Var1[1])
  clean_dataset[is.na(clean_dataset$employer_postal_code)&clean_dataset$job_info_work_state==empty_pc_state,"employer_postal_code"] <- mlv_postal_code
}

#no NA values at this point
#start by dropping withdrawals
clean_dataset<- clean_dataset[!clean_dataset$case_status=="Withdrawn",]

#change status for certified-expired to certified
clean_dataset[clean_dataset$case_status=="Certified-Expired","case_status"] <- "Certified"

#get month and year from decision date
clean_dataset$decision_month <- as.factor(months(clean_dataset$decision_date))
clean_dataset$decision_year <- as.factor(year(clean_dataset$decision_date))
clean_dataset$decision_date<-NULL

#convert categorical variables as factors
clean_dataset$case_status <- as.factor(clean_dataset$case_status)
clean_dataset$class_of_admission <- as.factor(clean_dataset$class_of_admission)
clean_dataset$country_of_citizenship <- as.factor(clean_dataset$country_of_citizenship)
clean_dataset$employer_name <- as.factor(clean_dataset$employer_name)
clean_dataset$job_info_work_state <- as.factor(clean_dataset$job_info_work_state)
clean_dataset$pw_level_9089 <- as.factor(clean_dataset$pw_level_9089)
clean_dataset$pw_soc_code <- as.factor(clean_dataset$pw_soc_code)
clean_dataset$pw_source_name_9089 <- as.factor(clean_dataset$pw_source_name_9089)
clean_dataset$pw_unit_of_pay_9089 <- as.factor(clean_dataset$pw_unit_of_pay_9089)

#run Naive Bayes
# library(rattle)
# library(e1071)
# set.seed(123)
# 
#  index<-sort(sample(nrow(clean_dataset),round(.25*nrow(clean_dataset))))
#  training<-clean_dataset[-index,]
#  test<-clean_dataset[index,]
# 
#  nBayes_all <- naiveBayes(case_status ~., data =training)
# 
#  category_all<-predict(nBayes_all,test  )
# 
#  table(NBayes_all=category_all,case_status=test$case_status)
#  NB_wrong<-sum(category_all!=test$case_status)
#  NB_error_rate<-NB_wrong/length(category_all)
#  NB_error_rate
# 
# cleanString <-function(x){
#    x=gsub("[[:punct:]]"," ",x)
# 
# }
# #C5.0
# c5.0ds<-clean_dataset
# sum(is.na(c5.0ds))
# str(c5.0ds)
# c5.0ds$employer_name<-sapply(c5.0ds$employer_name,cleanString)
# c5.0ds$employer_name = as.character(c5.0ds$employer_name)
# set.seed(123)
# 
# indexc5<-sort(sample(nrow(c5.0ds),round(.25*nrow(c5.0ds))))
# trainingc5<-c5.0ds[-indexc5,]
# testc5<-c5.0ds[indexc5,]
# 
# #install.packages("C50")
# library('C50')
# 
# # C50  classification trainFinal=trainingc5[,2:12]
# C50_class <- C5.0(case_status~.,data=trainingc5 )
# 
# 
# summary(C50_class )
# dev.off()
# plot(C50_class)
# C50_predict<-predict( C50_class ,test , type="class" )
# table(actual=test[,10],C50=C50_predict)
# wrong<- (test[,10]!=C50_predict)
# error_rate<-sum(wrong)/length(test[,10])
# paste0('The  C 5.0 model gives an Error Rate of:',round(error_rate*100,2),'%')

#Random Forest
library(randomForest)
library(plyr)
#split into training and testing and removing Sample Column as it is an identifier column
rf<-clean_dataset
str(rf)
rf$employer_name = as.character(rf$employer_name)
rf$pw_soc_code = as.character(rf$pw_soc_code)
#Merge R-1 and R-2 visas
rf[rf$class_of_admission=="R-2","class_of_admission"] <- "R-1"
rf[rf$class_of_admission=="Parol","class_of_admission"] <- "Parolee"
rf[rf$class_of_admission=="AOS/H-1B","class_of_admission"] <- "H1B"
rf[rf$class_of_admission=="AOS","class_of_admission"] <- "H1B"
rf$class_of_admission=droplevels.factor(rf$class_of_admission)
unique(rf$class_of_admission)

citizenshipCount=table(rf$country_of_citizenship)
jobStateCount=table(rf$job_info_work_state)
#for Reducing number of countries of citizenship
reduceCountries <-function(x){
  c=as.integer(citizenshipCount[x])
  ifelse(c<450,"Others",as.character(x))
}
#for Reducing number of work location states
reduceStates <-function(x){
  c=as.integer(jobStateCount[x])
  ifelse(c<115,"Others",as.character(x))
}
#new column for country of citizenship
rf["CC"]<-sapply(rf[["country_of_citizenship"]],reduceCountries) 
#new column for Work State
rf["Work_State"]<-sapply(rf[["job_info_work_state"]],reduceStates) 
rf$CC = as.factor(rf$CC)
rf$Work_State = as.factor(rf$Work_State)
unique(rf$CC)
table(rf$CC)
unique(rf$Work_State)
table(rf$Work_State)
rf$country_of_citizenship<-NULL
rf$job_info_work_state<-NULL
rf$pw_soc_code<-NULL
rf$employer_name<-NULL
str(rf)
sum(is.na(rf))

indexRf<-sort(sample(nrow(rf),round(.25*nrow(rf))))
trainingRF<-rf[-indexRf,]
testRF<-rf[indexRf,]

fit <- randomForest( case_status~., data=trainingRF, importance=TRUE, ntree=2)
summary(fit)
features<-(importance(fit))
View(features)
varImpPlot(fit)
finalTest<-testRF[,-1]
Prediction <- predict(fit,finalTest )
table(actual=testRF$case_status,Prediction)

wrong<- (testRF$case_status!=Prediction )
RF_error_rate<-sum(wrong)/length(wrong)
RF_error_rate
