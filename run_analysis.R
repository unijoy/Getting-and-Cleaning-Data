#1.Merges the training and the test sets to create one data set.
library(data.table)
library(plyr)
# this is my workspace , you need to change to your workspace
setwd('/Users/unijoy/Desktop/coursera/getting and cleaning data/project')
dataPath <- "UCI HAR Dataset/"
# help function for reading file 
readProjectData <- function(filename){
    read.table(filename,stringsAsFactors = FALSE,header = FALSE)
}

activity_labels <- readProjectData(file.path(dataPath,"activity_labels.txt"))
features <- readProjectData(file.path(dataPath,"features.txt"))[,2]

# get train data
subject_train <- readProjectData(file.path(dataPath,"train/subject_train.txt"))
X_train <- readProjectData(file.path(dataPath,"train/X_train.txt"))
Y_train <- readProjectData(file.path(dataPath,"train/Y_train.txt"))
# get test data
subject_test <- readProjectData(file.path(dataPath,"test/subject_test.txt"))
X_test <- readProjectData(file.path(dataPath,"test/X_test.txt"))
Y_test <- readProjectData(file.path(dataPath,"test/y_test.txt"))
# merge two data set
d_train <- cbind(subject_train,Y_train,X_train)
rm(subject_train,Y_train,X_train)
d_test <- cbind(subject_test,Y_test,X_test)
rm(subject_test,Y_test,X_test)
da <- rbind(d_train,d_test)
rm(d_train,d_test)
names(da) <- c("subject","activity",tolower(features))
rm(features)
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
meanstdonly <- da[,grep("mean|std|subject|activity",names(da))]
#3.Uses descriptive activity names to name the activities in the data set
meanstdonly <- data.table(meanstdonly)
setkey(meanstdonly,activity)
names(activity_labels) <- c("activity","activitylabel")
activity_labels <- data.table(activity_labels)
setkey(activity_labels,activity)
meanstdonly<-merge(meanstdonly,activity_labels)
#4.Appropriately labels the data set with descriptive variable names. 
newnames <- make.names(names(meanstdonly))
newnames <- gsub("\\.\\.","",newnames)
setnames(meanstdonly,newnames) 
#5.From the data set in step 4, creates a second, 
#   independent tidy data set with the average of each variable 
#   for each activity and each subject.
tidyMelt <- melt(meanstdonly,id=c("subject","activity"),measure.vars = names(meanstdonly)[3:88])
tidyData <- dcast(tidyMelt,subject + activity ~ variable,mean)

write.table(tidyData,file="average_by_activity_subject.txt",row.names = FALSE)