setwd("~/assignment 1/UCI HAR Dataset/test")

subject_test <- read.table("./subject_test.txt")
X_test <- read.table("./X_test.txt")
y_test <- read.table("./y_test.txt")


test_data <- cbind(subject_test,y_test,X_test)


setwd("~/assignment 1/UCI HAR Dataset/train")

subject_train <- read.table("./subject_train.txt")
X_train <- read.table("./X_train.txt")
y_train <- read.table("./y_train.txt")



subject <- rbind(subject_test,subject_train)
activity <- rbind(y_test,y_train)
features <- rbind(X_test,X_train)


setwd("~/assignment 1/UCI HAR Dataset")




featuresNames <- read.table("features.txt")
names(features)<- featuresNames$V2
featuresMean_std  <- grepl("(-std\\(\\)|-mean\\(\\))",featuresNames$V2)
features <- features[, which(featuresMean_std  == TRUE)]


names(subject)<-c("subject")
names(activity)<- c("activityNum")


activityLabels  <- read.table("./activity_labels.txt")
names(activityLabels)<-c("activityNum","activityName")


subject_activity <- cbind(subject, activity)
final_data <- cbind(subject_activity,features )
newFinal_data <- merge(activityLabels,final_data,by = "activityNum",all = TRUE)

library(reshape2)

molten <- melt(newFinal_data,id.vars=c("activityNum","activityName","subject"))
tidyData <- dcast(molten,subject + activityName ~ variable,mean)

