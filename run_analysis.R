library(dplyr)
library(dplyr)
#Read Data to R
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
variable_names <- read.table("./UCI HAR Dataset/features.txt")
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")
#Merge Train and Test data
X_merge <- rbind(X_train, X_test)
Y_merge <- rbind(Y_train, Y_test)
subject_merge <- rbind(subject_train, subject_test)
#Extracts the measurements on the mean and standard deviation
extracted_var <- variable_names[grep("mean\\(\\)|std\\(\\)",variable_names[,2]),]
X_merge <- X_merge[,extracted_var[,1]]
#Name the activities in the data set
colnames(Y_merge) <- "activity"
Y_merge$activitylabel <- factor(Y_merge$activity, labels = as.character(activity_labels[,2]))
activitylabel <- Y_merge[,-1]
#Labels the data set with descriptive variable names
colnames(X_merge) <- variable_names[extracted_var[,1],2]
#Creates a independent tidy data set with the average of each variable for each activity and each subject
colnames(subject_merge) <- "subject"
total <- cbind(X_merge, activitylabel, subject_merge)
total_mean <-
  +   total %>% 
  +   group_by(activitylabel, subject) %>% 
  +   summarize_each(funs(mean))
write.table(total_mean, file = "./UCI HAR Dataset/tidydata.txt", row.names = FALSE, col.names = TRUE)
