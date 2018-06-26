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
na_count <-  data.frame(na_count)
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
Test_Hopitalization_Diag$medical_specialty <-  NULL

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
Train_Hopitalization_Diag$admission_source_id[Train_Hopitalization_Diag$admission_source_id== 17] <- NA

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

str(TrainData)
length(levels(TrainData$A1Cresult))

levels(TrainData[, 1])
colnames(TrainData)
totalCols <- ncol(TestData)
is.factor(TestData[, 1])

for (i in 1:totalCols) {
    if (is.factor(TestData[, i])) {
        if (length(levels(TrainData[, i])) != length(levels(TestData[, i]))) {
            print(i)
            print(' Not Matching')
        }
    }
}
levels(TrainData[, 39])
levels(TestData[, 39])

#write.csv(TrainData)
#write.csv(TestData)

colnames(TrainData)
smoted_data <- SMOTE(readmitted ~ ., TrainData, perc.over = 300, k = 3)

TrainDataFinal <- subset(smoted_data, select = c(race, gender, age, readmitted, num_lab_procedures,
                                          num_procedures, num_medications, num_diagnoses,
                                          max_glu_serum, A1Cresult, metformin, repaglinide,
                                          nateglinide, chlorpropamide, glimepiride, glipizide,
                                          glyburide, tolbutamide, pioglitazone, rosiglitazone,
                                          acarbose, miglitol, tolazamide, insulin,
                                          glyburide.metformin, glipizide.metformin,
                                          metformin.rosiglitazone, change, diabetesMed,
                                          admission_type_id, discharge_disposition_id,
                                          admission_source_id))

TestDataFinal <- subset(TestData, select = c(race, gender, age, num_lab_procedures,
                                          num_procedures, num_medications, num_diagnoses,
                                          max_glu_serum, A1Cresult, metformin, repaglinide,
                                          nateglinide, chlorpropamide, glimepiride, glipizide,
                                          glyburide, tolbutamide, pioglitazone, rosiglitazone,
                                          acarbose, miglitol, tolazamide, insulin,
                                          glyburide.metformin, glipizide.metformin,
                                          metformin.rosiglitazone, change, diabetesMed,
                                          admission_type_id, discharge_disposition_id,
                                          admission_source_id))

library(caret)
set.seed(9983)
? createDataPartition
datapart <- createDataPartition(TrainDataFinal$readmitted, times = 1, p = 0.8, list = F)
train = TrainDataFinal[datapart,]
validation = TrainDataFinal[-datapart,]


library(rpart)
DT_rpart_Reg <- rpart(readmitted ~ ., data = train)
print(DT_rpart_Reg)
printcp(DT_rpart_Reg)
plot(printcp(DT_rpart_Reg), type = 'b')

# Predict on Train and Test data
predTrain <- predict(DT_rpart_Reg, newdata = train, type = "class")
predValidation <- predict(DT_rpart_Reg, newdata = validation, type = "class")

#c. Error Metrics on train and test
trainCM = confusionMatrix(train$readmitted, predTrain, positive = "Within30days")
validationCM = confusionMatrix(validation$readmitted, predValidation, positive = "Within30days")

predTest <- predict(DT_rpart_Reg, newdata = TestDataFinal, type = "class")
predTest

#submision <- data.frame(TestData$patientID, predTest)
#install.packages('rpart.utils')
#install.packages('rattle')
library(rpart.utils)
library(rattle)
rpart.rules(DT_rpart_Reg)
asRules(DT_rpart_Reg)
print(asRules(DT_rpart_Reg))
rules <- asRules(DT_rpart_Reg)
out <- capture.output(summary(DT_rpart_Reg))
cat("Patterns", out, file = "patterns.txt", sep = "n", append = TRUE)