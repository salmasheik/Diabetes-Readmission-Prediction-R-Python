# Clear Environmet Variables
rm(list = ls(all = T))


#Get and Set Working Directory
getwd()
setwd("E:\\SalmaWorkSpace\\Data_Model_diabetes_readmission\\Data_Model_diabetes_readmission")

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


TrainDataFinal = subset(TrainData, select = c(age_new, race, admission_source_id, admission_type_id, change,
                                            diabetesMed, diagnosis_1_new,
                                            diagnosis_2_new, diagnosis_3_new,
                                            discharge_disposition_id, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent,
                                            readmitted))


TestDataFinal = subset(TestData, select = c(age_new, race, admission_source_id, admission_type_id, change,
                                            diabetesMed, diagnosis_1_new,
                                            diagnosis_2_new, diagnosis_3_new,
                                            discharge_disposition_id, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent))
colnames(TestDataFinal)
table(TestDataFinal$race)
table(TrainData$readmitted)

##############################################################################

###########################Random Forest
TrainDataFinal1 = subset(TrainData, select = c(age_new, race,admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent,
                                            readmitted))
#levels(TrainDataFinal1$diagnosis_3_new)
#levels(TestDataFinal1$diagnosis_3_new)


TestDataFinal1 = subset(TestData, select = c(age_new, race, admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent))
 
#smoted_data1 <- SMOTE(readmitted ~ ., TrainDataFinal1, perc.over = 400, perc.under = 50)
smoted_data1 <- SMOTE(readmitted ~ ., TrainDataFinal1, perc.over = 550, perc.under =28)
table(smoted_data1$readmitted)

library(vegan)
#### Separating numeric variables separately
num_data <- smoted_data1[, sapply(smoted_data1, is.numeric)]
num_data_scale <- decostand(num_data, "range")
summary(num_data_scale)


### Separating categorical variables
cat_data <- smoted_data1[, sapply(smoted_data1, is.factor)]
cat_data_wo_target <- subset(cat_data, select = -c(readmitted))
colnames(cat_data_wo_target)

# Getting Target variable in separate data frame and running dummyfication on it
readmitted <- data.frame(readmitted = smoted_data1$readmitted)

target_binned <- data.frame(model.matrix(~., data = cat_data_wo_target,
                              contrasts.arg = lapply(cat_data_wo_target, contrasts, contrasts = FALSE))[, -1])

finalData <- data.frame(num_data_scale, target_binned, readmitted)

############################Test Data #####################################

#### Separating numeric variables separately
num_data_Test <- TestDataFinal1[, sapply(TestDataFinal1, is.numeric)]
num_data_scale_Test <- decostand(num_data_Test, "range")
summary(num_data_scale_Test)


### Separating categorical variables
cat_data_Test <- TestDataFinal1[, sapply(TestDataFinal1, is.factor)]



#########running dummyfication on cat_data_Test


target_binned_Test <- data.frame(model.matrix(~., data = cat_data_Test,
                              contrasts.arg = lapply(cat_data_Test, contrasts, contrasts = FALSE))[, -1])

finalData_Test <- data.frame(num_data_scale_Test, target_binned_Test)

############################################################
library(caret)
set.seed(9983)
datapart <- createDataPartition(finalData$readmitted, times = 1, p = 0.8, list = F)
train = finalData[datapart,]
validation = finalData[-datapart,]


#############################
library(randomForest)
rf <- randomForest(readmitted ~ ., data = train, ntree = 50, mtry = 11)

ncol(TrainDataFinal1)
importance(rf)
varImpPlot(rf)

pred_train <- predict(rf, train, type = "class")
pred_validate <- predict(rf, validation, type = "class")
pred_test <- predict(rf, finalData_Test, type = "class")

#c. Confusion Metrics on train and test
trainCM = confusionMatrix(train$readmitted, pred_train, positive = "Within30days")
validationCM = confusionMatrix(validation$readmitted, pred_validate, positive = "Within30days")

library(pROC)
#library(roc)
predictions = as.vector(rf$votes[, 2])
pred = prediction(predictions, train$readmitted)
print(roc(as.numeric(validation$readmitted), as.numeric(pred_validate)))

#prediction on testdata
pred_test_df <- data.frame(pred_test)
table(smoted_data1$readmitted)
table(pred_test_df$pred_test)
write.csv(pred_test_df, file = "pred_test.csv")
