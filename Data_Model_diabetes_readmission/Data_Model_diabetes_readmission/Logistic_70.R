# Clear Environmet Variables
rm(list = ls(all = T))


#Get and Set Working Directory
getwd()
setwd("E:\\SalmaWorkSpace\\Data_Model_diabetes_readmission\\Data_Model_diabetes_readmission")

#Reading Train Data files
TrainRaw <- read.csv("Train.csv", header = T, sep = ",")
TrainHospitalizationRaw <- read.csv("Train_HospitalizationData.csv", header = T, sep = ",")
TrainDiagnosisRaw <- read.csv("Train_Diagnosis_TreatmentData.csv", header = T, sep = ",")

#Reading Test data files
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


colnames(TrainData)

#Getting Days spent in hospital
TrainData$DaysSpent <- as.numeric(as.Date(TrainData$Discharge_date) - as.Date(TrainData$Admission_date))
TestData$DaysSpent <- as.numeric(as.Date(TestData$Discharge_date) - as.Date(TestData$Admission_date))

NCOL(TestData)

#Function to get age as int and the mean value
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

#Adding age column to dataframes (Train and Test)
TrainData$age_new <- sapply(TrainData$age, GetAgeMean)
TrainData$age <- NULL
TestData$age_new <- sapply(TestData$age, GetAgeMean)
TestData$age <- NULL

#Reducing dimensions of Diagnostic category by merging cateogories
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

#Applying Diagnostic category new values with reduced dimensions on Train Data
TrainData$diagnosis_1_new <- sapply(TrainData$diagnosis_1, GetDiagnosticCategory)
TrainData$diagnosis_2_new <- sapply(TrainData$diagnosis_2, GetDiagnosticCategory)
TrainData$diagnosis_3_new <- sapply(TrainData$diagnosis_3, GetDiagnosticCategory)

#Converting new categories to factors on Train DAta
TrainData$diagnosis_1_new <- as.factor(TrainData$diagnosis_1_new)
TrainData$diagnosis_2_new <- as.factor(TrainData$diagnosis_2_new)
TrainData$diagnosis_3_new <- as.factor(TrainData$diagnosis_3_new)

str(TrainData)

#Removing old diagnostic columns from Train DAta
TrainData$diagnosis_1 <- NULL
TrainData$diagnosis_2 <- NULL
TrainData$diagnosis_3 <- NULL

#Applying Diagnostic category new values with reduced dimensions on Test Data
TestData$diagnosis_1_new <- sapply(TestData$diagnosis_1, GetDiagnosticCategory)
TestData$diagnosis_2_new <- sapply(TestData$diagnosis_2, GetDiagnosticCategory)
TestData$diagnosis_3_new <- sapply(TestData$diagnosis_3, GetDiagnosticCategory)

#Converting new categories to factors on Test DAta
TestData$diagnosis_1_new <- as.factor(TestData$diagnosis_1_new)
TestData$diagnosis_2_new <- as.factor(TestData$diagnosis_2_new)
TestData$diagnosis_3_new <- as.factor(TestData$diagnosis_3_new)

#Removing old diagnostic columns from Train DAta
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


#Subsetting TrainData with important varialbes
TrainDataFinal = subset(TrainData, select = c(age_new, race, admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent,
                                            readmitted))

#Subsetting TestData with important varialbes
TestDataFinal = subset(TestData, select = c(age_new, race, admission_type_id, change,
                                            diabetesMed, glipizide, insulin, max_glu_serum,
                                            metformin, repaglinide,
                                            num_lab_procedures,
                                            num_procedures, num_medications, num_diagnoses, DaysSpent))

table(TrainDataFinal$readmitted)

#Smoting to upsample the minority category
smoted_data <- SMOTE(readmitted ~ ., TrainDataFinal, perc.over = 500, perc.under = 28)
table(smoted_data$readmitted)
str(smoted_data)
smoted_data$acetohexamide = NULL


#Separating Train into Train and validation
library(caret)
set.seed(9983)
datapart <- createDataPartition(smoted_data$readmitted, times = 1, p = 0.8, list = F)
train = smoted_data[datapart,]
validation = smoted_data[-datapart,]

#building the model on entire dataset
log_reg <- glm(readmitted ~ ., data = train, family = "binomial")
summary(log_reg)

#By default if no dataset is given, it takes training data

prob_train <- predict(log_reg, type = "response")
head(prob_train)

# to choose a threshold value, use package ROCR
library(ROCR)
pred <- prediction(prob_train, train$readmitted)

perf <- performance(pred, measure = "tpr", x.measure = "fpr")

#Plotting the ROC  Curve
plot(perf, col = rainbow(10), colorize = T,
     print.cutoffs.at = seq(0, 1, 0.05))

perf_auc <- performance(pred, measure = "auc")

# Access the auc score from the performance object

auc <- perf_auc@y.values[[1]]

print(auc)

#Train data predictions and confusion matrix
pred_class <- ifelse(prob_train > 0.6775, "Within30days", "NO")
table(train$readmitted, pred_class)
confusionMatrix(pred_class, train$readmitted, positive = "Within30days")

#Validation data predictions and confusion matrix
prob_validation <- predict(log_reg, validation, type = "response")
preds_valid <- ifelse(prob_validation > 0.6775, "Within30days", "NO")
table(validation$readmitted, preds_valid)

confusionMatrix(preds_valid, validation$readmitted, positive = "Within30days")


#Test data predictions and confusion matrix
prob_test <- predict(log_reg, TestDataFinal, type = "response")
preds_test <- ifelse(prob_test > 0.6775, "Within30days", "NO")

pred_test_df <- data.frame(preds_test)
table(pred_test_df$preds_test)

test_Predictions <- data.frame(TestData$patientID,preds_test)
colnames(test_Predictions) <- c('patientId','readmitted')

#Writing predictions to CSV
write.csv(test_Predictions, 'test_predictions.csv', row.names = FALSE)

getwd()
