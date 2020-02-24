#Author Anthony
#Using data collected from the accelerometers from the Samsung Galaxy S 
#smartphone, work with the data and make a clean data set, outputting the
#resulting tidy data to a file named "tidy_data.txt".

#Need to create the dataframe and take data from the documents

library(dplyr)

#download zip and unzip it
zipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
  download.file(zipURL, zipFile, mode = "wb")
}

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(zipFile)
}

#creating tables of training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

#creating tables of test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

#creating features table
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

#creating activities table
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

#1.
#merging training and test
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

#saving memory space
rm(trainingSubjects, trainingValues, trainingActivity, testSubjects, testValues, testActivity)

#making the heading look better
colnames(humanActivity) <- c("subject", features[, 2], "activity")

#2.
# determine columns of data set to keep based on column name...
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
#columnsToKeep
humanActivity <- humanActivity[, columnsToKeep]

#3.
# replace activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, levels = activities[, 1], labels = activities[, 2])

#4.
# get column names
humanActivityCols <- colnames(humanActivity)
# remove special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# expand abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# correct typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# use new labels as column names
colnames(humanActivity) <- humanActivityCols

#5.
# group by subject and activity and summarise using mean
humanActivityMeans <- humanActivity %>% group_by(subject, activity) %>% summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)








