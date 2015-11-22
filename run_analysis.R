
## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# assuming that the packages "data.table" and "reshape2" are installed 

library("data.table")
library("reshape2")

# read in features (second column)
features <- read.table("UCI HAR Dataset/features.txt")[,2]

# read in the activity types (second column)
labels <- read.table("UCI HAR Dataset/activity_labels.txt")[,2]


# get all features containing 'mean' or 'std'
extract_features <- grepl("mean|std", features)


#######

loaddata <- function(x, features, labels, extract_features) {

        # Load and process data
        dir <-  paste(c("UCI HAR Dataset/",x), collapse="") 
        X <- read.table(paste(c(dir,"/X_",x,".txt"), collapse="")) 
        y <- read.table(paste(c(dir,"/y_",x,".txt"), collapse=""))
        subject <- read.table(paste(c(dir,"/subject_",x,".txt"), collapse=""))

        names(X) = features

        # Extract only the measurements on the mean and standard deviation for each measurement.
        X = X[,extract_features]

        # Load activity labels
        y[,2] = activity_labels[y[,1]]
        names(y) = c("Activity_ID", "Activity_Label")
        names(subject) = "subject"

        # Bind data
        test_data <- cbind(as.data.table(subject), y, X)

}

# call function to create datasets
test_data <- loaddata("test", features, labels, extract_features)
train_data <- loaddata("train", features, labels, extract_features)

# Merge datasets
merged_data = rbind(test_data, train_data)

# retain these three columns in the melted dataset
id_labels   = c("subject", "Activity_ID", "Activity_Label")

# melt all other columns
data_labels = setdiff(colnames(merged_data), id_labels)

# wide-to-long: create row for every column (de-normalize, e.g. melt)
melt_data      = melt(merged_data, id = id_labels, measure.vars = data_labels)

# long-to-wide (normalize it again), while applying mean
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

# export data to external file
write.table(tidy_data, file = "tidy_data.txt", row.names=FALSE)


