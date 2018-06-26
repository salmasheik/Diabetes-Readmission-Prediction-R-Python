# Clear Environmet Variables
# Clear Environmet Variables
rm(list = ls(all = T))


#Get and Set Working Directory
getwd()
setwd("E:\\SalmaWorkSpace\\Data_Model_diabetes_readmission\\Data_Model_diabetes_readmission\\")


TrainRaw <- read.csv("Train.csv", header = T, sep = ",")
TrainHospitalizationRaw <- read.csv("Train_HospitalizationData.csv", header = T, sep = ",")
TrainDiagnosisRaw <- read.csv("Train_Diagnosis_TreatmentData.csv", header = T, sep = ",")

TestRaw <- read.csv("Test.csv", header = T, sep = ",")
TestHospitalizationRaw <- read.csv("Test_HospitalizationData.csv", header = T, sep = ",")
TestDiagnosisRaw <- read.csv("Test_Diagnosis_TreatmentData.csv", header = T, sep = ",")



#Check number of records in each Train file
nrow(TrainRaw)
NROW(TrainHospitalizationRaw)
NROW(TrainDiagnosisRaw)

#Check number of records in each file
NROW(TestRaw)
NROW(TestHospitalizationRaw)
NROW(TestDiagnosisRaw)

library(sqldf)

#Check if duplicate patient Id's are there
sqldf("select count(distinct patientID) from TrainRaw")
sqldf("select count(distinct patientID) from TrainHospitalizationRaw")
sqldf("select count(distinct patientID) from TrainDiagnosisRaw")

#Checking types of all dataframes
str(TrainRaw)
str(TrainHospitalizationRaw)
str(TrainDiagnosisRaw)

# Merging three Train data frames

Train_Hopitalization <- merge(TrainRaw, TrainHospitalizationRaw, by = c("patientID"))
str(Train_Hopitalization)

Train_Hopitalization_Diag <- merge(Train_Hopitalization, TrainDiagnosisRaw, by = c("patientID"))
str(Train_Hopitalization_Diag)

# Merging three Test data frames

Test_Hopitalization <- merge(TestRaw, TestHospitalizationRaw, by = c("patientID"))
str(Test_Hopitalization)

Test_Hopitalization_Diag <- merge(Test_Hopitalization, TestDiagnosisRaw, by = c("patientID"))
str(Test_Hopitalization_Diag)

# Deal with missing values

na_count <- sapply(Train_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count <- data.frame(na_count)
Train_Hopitalization_Diag[Train_Hopitalization_Diag == '?'] <- NA

na_count <- sapply(Train_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count <- data.frame(na_count)
na_count['TotalRows'] <- NROW(Train_Hopitalization_Diag)

# Deal with missing Test values

na_count_test <- sapply(Test_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count_test <- data.frame(na_count)
Test_Hopitalization_Diag[Test_Hopitalization_Diag == '?'] <- NA

na_count_test <- sapply(Test_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count_test <- data.frame(na_count_test)
na_count_test['TotalRows'] <- NROW(Test_Hopitalization_Diag)


#Dropping weight, payer-code and mdeical-specialtiy as more than 50% are na
Train_Hopitalization_Diag$weight <- NULL
Train_Hopitalization_Diag$payer_code <- NULL
Train_Hopitalization_Diag$medical_specialty <- NULL

Test_Hopitalization_Diag$weight <- NULL
Test_Hopitalization_Diag$payer_code <- NULL
Test_Hopitalization_Diag$medical_specialty <- NULL

#Checking NA Counts again
na_count <- sapply(Train_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count <- data.frame(na_count)
na_count['TotalRows'] <- NROW(Train_Hopitalization_Diag)

na_count_test <- sapply(Test_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count_test <- data.frame(na_count_test)
na_count_test['TotalRows'] <- NROW(Test_Hopitalization_Diag)

# Checking Null in admission_type_id , discharge_disposition_id , admission_source_id 

Train_Hopitalization_Diag$admission_type_id[Train_Hopitalization_Diag$admission_type_id == 6] <- NA
Train_Hopitalization_Diag$discharge_disposition_id[Train_Hopitalization_Diag$discharge_disposition_id == 18] <- NA
Train_Hopitalization_Diag$admission_source_id[Train_Hopitalization_Diag$admission_source_id == 17] <- NA

Test_Hopitalization_Diag$admission_type_id[Test_Hopitalization_Diag$admission_type_id == 6] <- NA
Test_Hopitalization_Diag$discharge_disposition_id[Test_Hopitalization_Diag$discharge_disposition_id == 18] <- NA
Test_Hopitalization_Diag$admission_source_id[Test_Hopitalization_Diag$admission_source_id == 17] <- NA


sqldf("select race, count(race) from Train_Hopitalization_Diag group by race")

table(Train_Hopitalization_Diag$race, Train_Hopitalization_Diag$gender)
str(Train_Hopitalization_Diag)
levels(Train_Hopitalization_Diag$race)

sqldf("select diagnosis_1, count(diagnosis_1) from Train_Hopitalization_Diag group by diagnosis_1 order by 2 desc")
summary(Train_Hopitalization_Diag$diagnosis_1)

#Converting few variables to categorical
Train_Hopitalization_Diag$admission_type_id <- as.factor(Train_Hopitalization_Diag$admission_type_id)
Train_Hopitalization_Diag$discharge_disposition_id <- as.factor(Train_Hopitalization_Diag$discharge_disposition_id)
Train_Hopitalization_Diag$admission_source_id <- as.factor(Train_Hopitalization_Diag$admission_source_id)

Test_Hopitalization_Diag$admission_type_id <- as.factor(Test_Hopitalization_Diag$admission_type_id)
Test_Hopitalization_Diag$discharge_disposition_id <- as.factor(Test_Hopitalization_Diag$discharge_disposition_id)
Test_Hopitalization_Diag$admission_source_id <- as.factor(Test_Hopitalization_Diag$admission_source_id)

#Checking NA Counts again
na_count <- sapply(Train_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count <- data.frame(na_count)
na_count['TotalRows'] <- NROW(Train_Hopitalization_Diag)

na_count_test <- sapply(Test_Hopitalization_Diag, function(x) { sum(is.na(x)) })
na_count_test <- data.frame(na_count_test)
na_count_test['TotalRows'] <- NROW(Test_Hopitalization_Diag)

library(DMwR)
TrainData <- centralImputation(Train_Hopitalization_Diag)
TestData <- centralImputation(Test_Hopitalization_Diag)

na_count_Train_PostImpute <- sapply(TrainData, function(x) { sum(is.na(x)) })
na_count_Train_PostImpute <- data.frame(na_count_Train_PostImpute)

na_count_Test_PostImpute <- sapply(TestData, function(x) { sum(is.na(x)) })
na_count_Test_PostImpute <- data.frame(na_count_Test_PostImpute)

#str(TrainData)
#length(levels(TrainData$A1Cresult))

#levels(TrainData[, 1])
#colnames(TrainData)
#totalCols <- ncol(TestData)
#is.factor(TestData[, 1])

#for (i in 1:totalCols) {
#if (is.factor(TestData[, i])) {
#if (length(levels(TrainData[, i])) != length(levels(TestData[, i]))) {
#print(i)
#print(' Not Matching')
#}
#}
#}
#levels(TrainData[, 39])
#levels(TestData[, 39])

#write.csv(TrainData)
#write.csv(TestData)
colnames(TrainData)

TrainData$DaysSpent <- as.numeric(as.Date(TrainData$Discharge_date) - as.Date(TrainData$Admission_date))
TestData$DaysSpent <- as.numeric(as.Date(TestData$Discharge_date) - as.Date(TestData$Admission_date))

NCOL(TestData)

GetAgeMean = function(arg) {
    result <- 0;
    if (arg == '[0-10)') {
        result <- 5
    }
    else if (arg == '[10-20)') {
        result <- 15
    }
    else if (arg == '[20-30)') {
        result <- 25
    }
    else if (arg == '[30-40)') {
        result <- 35
    }
    else if (arg == '[40-50)') {
        result <- 45
    }
    else if (arg == '[50-60)') {
        result <- 55
    }
    else if (arg == '[60-70)') {
        result <- 65
    }
    else if (arg == '[70-80)') {
        result <- 75
    }
    else if (arg == '[80-90)') {
        result <- 85
    }
    else if (arg == '[90-100)') {
        result <- 95
    }
    return(as.numeric(result))
}

TrainData$age_new <- sapply(TrainData$age, GetAgeMean)
TrainData$age <- NULL
TestData$age_new <- sapply(TestData$age, GetAgeMean)
TestData$age <- NULL


GetDiagnosticCategory = function(arg1) {
    arg <- as.numeric(arg1)
    result <- "Other";
    if (arg >= 0 & arg <= 139) {
        result <- "Parasitic";
    }
    else if (arg > 139 & arg <= 239) {
        result <- "Neoplasms";
    }
    else if (arg > 239 & arg <= 279) {
        result <- "E_N_M";
    }
    else if (arg > 279 & arg <= 289) {
        result <- "BloodDiseases";
    }
    else if (arg > 289 & arg <= 319) {
        result <- "MentalDisorders";
    }
    else if (arg > 319 & arg <= 389) {
        result <- "NervousSystems";
    }
    else if (arg > 390 & arg <= 459) {
        result <- "Circulatory";
    }
    else if (arg > 459 & arg <= 519) {
        result <- "Respiratory";
    }
    else if (arg > 520 & arg <= 579) {
        result <- "Digestive";
    }
    else if (arg > 579 & arg <= 629) {
        result <- "Gentitorinary";
    }
    else if (arg > 629 & arg <= 679) {
        result <- "Genic";
    }
    else if (arg > 679 & arg <= 709) {
        result <- "Skin";
    }
    else if (arg > 709 & arg <= 739) {
        result <- "Masculoskeletal";
    }
    else if (arg > 739 & arg <= 759) {
        result <- "CongenitalAnamolies";
    }
    else if (arg > 759 & arg <= 779) {
        result <- "ParinetalPeriod";
    }
    else if (arg > 779 & arg <= 799) {
        result <- "NoSymptoms";
    }
    else if (arg > 799 & arg <= 999) {
        result <- "Injury";
    }
    else {
        result <- "Other";
    }
    return(result)
}

TrainData$diagnosis_1_new <- sapply(TrainData$diagnosis_1, GetDiagnosticCategory)
TrainData$diagnosis_2_new <- sapply(TrainData$diagnosis_2, GetDiagnosticCategory)
TrainData$diagnosis_3_new <- sapply(TrainData$diagnosis_3, GetDiagnosticCategory)

TrainData$diagnosis_1_new <- as.factor(TrainData$diagnosis_1_new)
TrainData$diagnosis_2_new <- as.factor(TrainData$diagnosis_2_new)
TrainData$diagnosis_3_new <- as.factor(TrainData$diagnosis_3_new)

str(TrainData)
TrainData$diagnosis_1 <- NULL
TrainData$diagnosis_2 <- NULL
TrainData$diagnosis_3 <- NULL

TestData$diagnosis_1_new <- sapply(TestData$diagnosis_1, GetDiagnosticCategory)
TestData$diagnosis_2_new <- sapply(TestData$diagnosis_2, GetDiagnosticCategory)
TestData$diagnosis_3_new <- sapply(TestData$diagnosis_3, GetDiagnosticCategory)

TestData$diagnosis_1_new <- as.factor(TestData$diagnosis_1_new)
TestData$diagnosis_2_new <- as.factor(TestData$diagnosis_2_new)
TestData$diagnosis_3_new <- as.factor(TestData$diagnosis_3_new)


TestData$diagnosis_1 <- NULL
TestData$diagnosis_2 <- NULL
TestData$diagnosis_3 <- NULL

#Removing varialbes with only 1 levels
TrainData$acetohexamide <- NULL
TestData$acetohexamide <- NULL

ncol(TrainData)
ncol(TestData)
colnames(TrainData)
colnames(TestData)


TrainDataFinal = subset(TrainData, select = c(age_new, race, admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent,
                                            readmitted))
#levels(TrainDataFinal1$diagnosis_3_new)
#levels(TestDataFinal1$diagnosis_3_new)


TestDataFinal = subset(TestData, select = c(age_new, race, admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent))
colnames(TestDataFinal)
table(TestDataFinal$race)
table(TrainData$readmitted)
table(smoted_data$readmitted)
colnames(TrainData)
smoted_data <- SMOTE(readmitted ~ ., TrainDataFinal, perc.over = 550, perc.under = 28)
table(smoted_data$readmitted)
#smoted_data <- SMOTE(readmitted ~ ., TrainDataFinal, perc.over = 500, perc.under = 28)
#smoted_data <- SMOTE(readmitted ~ ., TrainData, perc.over = 400, perc.under = 50) # for recall 35.57
#smoted_data <- SMOTE(readmitted ~ ., TrainData, perc.over = 500, perc.under = 100) # for recall 14.39
str(smoted_data)
smoted_data$acetohexamide = NULL


library(caret)
set.seed(9983)
datapart <- createDataPartition(smoted_data$readmitted, times = 1, p = 0.8, list = F)
train = smoted_data[datapart,]
validation = smoted_data[-datapart,]
?rpart
library(rpart)
DT_rpart_Reg <- rpart(readmitted ~ ., data = train, method = "class", minsplit = 34, minbucket = 3, cp = 0.0006,
              maxdepth = 13)
print(DT_rpart_Reg)
printcp(DT_rpart_Reg)
plot(printcp(DT_rpart_Reg), type = 'b')

# Predict on Train and Test data
predTrain <- predict(DT_rpart_Reg, newdata = train, type = "class")
predValidation <- predict(DT_rpart_Reg, newdata = validation, type = "class")

#c. Error Metrics on train and test
trainCM = confusionMatrix(train$readmitted, predTrain, positive = "Within30days")
validationCM = confusionMatrix(validation$readmitted, predValidation, positive = "Within30days")
library(pROC)
print(roc(as.numeric(validation$readmitted), as.numeric(predValidation)))
predTest <- predict(DT_rpart_Reg, newdata = TestDataFinal, type = "class")
pred_test_df <- data.frame(predTest)
table(pred_test_df$predTest)


x <- data.frame(TestData$patientID, predTest,TestData$discharge_disposition_id)


###########################################################################

library(caret)
set.seed(9983)
datapart1 <- createDataPartition(TrainDataFinal$readmitted, times = 1, p = 0.8, list = F)
train1 = TrainDataFinal[datapart1,]
validation1 = TrainDataFinal[-datapart1,]
table(train1$readmitted)
table(validation1$readmitted)

smoted_data1 <- SMOTE(readmitted ~ ., train1, perc.over = 550, perc.under = 28)
table(smoted_data1$readmitted)

library(rpart)
DT_rpart_Reg1 <- rpart(readmitted ~ ., data = smoted_data1, method = "class", minsplit = 34, minbucket = 3, cp = 0.0005,
              maxdepth = 13)
print(DT_rpart_Reg1)
printcp(DT_rpart_Reg1)
plot(printcp(DT_rpart_Reg1), type = 'b')

# Predict on Train and Test data
predTrain1 <- predict(DT_rpart_Reg1, newdata = smoted_data1, type = "class")
predValidation1 <- predict(DT_rpart_Reg1, newdata = validation1, type = "class")

#c. Error Metrics on train and test
trainCM1 = confusionMatrix(smoted_data1$readmitted, predTrain1, positive = "Within30days")
validationCM1 = confusionMatrix(validation1$readmitted, predValidation1, positive = "Within30days")
library(pROC)
table(predValidation1)
print(roc(as.numeric(validation1$readmitted), as.numeric(predValidation1)))
predTest1 <- predict(DT_rpart_Reg1, newdata = TestDataFinal, type = "class")
pred_test_dff <- data.frame(predTest1)
table(pred_test_dff$predTest1)


x <- data.frame(TestData$patientID, predTest, TestData$discharge_disposition_id)