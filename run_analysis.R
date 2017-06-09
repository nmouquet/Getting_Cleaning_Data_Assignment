#Getting and Cleaning Data Course Project
#Nicolas Mouquet, Jun 2017
#nicolas.mouquet@cnrs.fr

#DOWNLOAD THE DATA 

     fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
     download.file(fileUrl,destfile="data.zip")
     unzip("data.zip")

     features <-  read.table("./UCI HAR Dataset/features.txt")  
     
     subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")  
     X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
     Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
       
     subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
     X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
     Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")

     activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")


#Step 1 : Merges the training and the test sets to create one data set
# use the subject, Y and X train

     train <- cbind(subject_train,Y_train,X_train)
     colnames(train) <- c("subject","activity",as.character(features$V2))
     test <- cbind(subject_test,Y_test,X_test)
     colnames(test) <- c("subject","activity",as.character(features$V2))
     all <- rbind(train,test)

#Step 2 : Extract mean and standard deviation for each measurement

     mean_std <- c(grep('mean',features_names),grep('std',features_names))
     all_mean_std <- all[,c(1:2,mean_std+3)]

#Step 3 : Uses descriptive activity names to name the activities in the data set
     
     for(i in 1:length(all_mean_std$activity)) 
          {
          all_mean_std$activity[i]=as.character(activity_labels[activity_labels$V1==all_mean_std$activity[i],]$V2)
     }
     
#Step 4 : Appropriately labels the data set with descriptive variable names
# has already been done when preparing the first table 

     
#Step 5 : From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
     
     #split all_mean_std by subject and activity
          s <- split(all_mean_std, list(all_mean_std$subject,all_mean_std$activity))
     #compute the means
          z <- do.call(rbind,lapply(s,function (x) colMeans(x[,-c(1:3)])))
     #get two colums for the subject and activity labels 
          names <- do.call(rbind,strsplit(rownames(z),"[.]"))
          colnames(names) <- c("subject","activity")
     #merge to create the final tidy dataset 
          final <- cbind(names,z)
     #save the tidy dataset 
          write.table(final,"tidy_data.txt")



