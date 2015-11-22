#######################################################################
#Step 1. Merges the training and the test sets to create one data set.#
#######################################################################

feature_names <- read.table("Dataset/features.txt")
activity_labels <- read.table("Dataset/activity_labels.txt")

x_train <- read.table("Dataset/train/X_train.txt", header=F)
subject_train <- read.table("Dataset/train/subject_train.txt")
y_train <- read.table("Dataset/train/y_train.txt")
train_data <- cbind(x_train, subject_train, y_train)
rm(x_train, subject_train, y_train)

x_test <- read.table("Dataset/test/X_test.txt")
subject_test <- read.table("Dataset/test/subject_test.txt")
y_test <- read.table("Dataset/test/y_test.txt")
test_data <- cbind(x_test, subject_test, y_test)
rm(x_test, subject_test, y_test)

mydata <- rbind(train_data, test_data)
rm(train_data, test_data)
names(mydata)<-c(t(feature_names[2]), "subject", "activity")
feature_names<-names(mydata)

#############################################################################
#Step 2. Extracts only the measurements on the mean and standard deviation  #
#for each measurement.                                                      #
#############################################################################

filtered1 <- grepl("mean|std|subject|activity", names(mydata), ignore.case=TRUE)& !grepl("meanFreq", names(mydata), ignore.case=TRUE)
filtered_data = mydata[, filtered1]
rm(mydata)

################################################################################
#Step 3. Uses descriptive activity names to name the activities in the data set#
################################################################################

for (i in 1:nrow(activity_labels)) {
    filtered_data$activity[filtered_data$activity == activity_labels[i, 1]] <- as.character(activity_labels[i, 2])
}

############################################################################
#Step 4. Appropriately labels the data set with descriptive variable names.#
############################################################################
filtered_labels <- feature_names[filtered1]

names(filtered_data) <- filtered_labels
###############################################################################
#Step 5. From the data set in step 4, creates a second, independent tidy data #
#set with the average of each variable for each activity and each subject.    #
###############################################################################

if(!("dplyr" %in% rownames(installed.packages()))){
    install.packages("dplyr")
    library(dplyr)
}else{
    library(dplyr)
}
if(!("tidyr" %in% rownames(installed.packages()))){
    install.packages("tidyr")
    library(tidyr)
}else{
    library(tidyr)
}

tidy_data <- tbl_df(filtered_data) %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean)) %>%
    gather(factor, mean, -activity, -subject)

write.table(tidy_data, file="tidy_data.txt", row.name=FALSE)