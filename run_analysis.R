run_analysis <- function () {

        require(plyr)
        require(dplyr)
##Step 1
        ## Read Training dataset and test dataset and cobine them with rbind
        trainingSet <- read.table("./UCI/train/X_train.txt")
        testSet <- read.table("./UCI/test/X_test.txt")
        combinedDataset <- rbind(trainingSet, testSet)
        
##Step 2
        ## Read features and remove characters such as (, ), hyphen, comma, and other duplicate data
        features <- read.table("./UCI/features.txt")
        
        features[,2] <- as.character(gsub("\\(\\)","", as.matrix(features[,2])))
        features[,2] <- as.character(gsub(")","", as.matrix(features[,2])))
        features[,2] <- as.character(gsub("\\(","", as.matrix(features[,2])))
        features[,2] <- as.character(gsub("-","", as.matrix(features[,2])))
        features[,2] <- as.character(gsub(",","", as.matrix(features[,2])))
        features[,2] <- as.character(gsub("BodyBody","Body", as.matrix(features[,2])))
        
        ##Select only features with mean or std and drop all other columns
        colnames(combinedDataset) <- features[,2]
        cols <- grep('(mean|std)', colnames(combinedDataset))
        newDataset <- combinedDataset[, cols]
        

##step 3
        ## Read Subject training set and test set
        sub_train <- read.table("./UCI/train/subject_train.txt")
        sub_test <- read.table("./UCI/test/subject_test.txt")
        
        ##cbind the subjects to the main dataset
        subjects <- rbind(sub_train, sub_test)
        colnames(subjects) <- "subjects"
        newDataset <- cbind(newDataset, subjects)
                
        ## Read activity training set and test set
        act_train <- read.table("./UCI/train/y_train.txt")
        act_test <- read.table("./UCI/test/y_test.txt")
        
        ##Combine them to one data frame and add it to the main dataset
        act_all <- rbind(act_train, act_test)
        newDataset <- cbind(newDataset, act_all)
        
        ## Read descriptive activity labels and merge it based on V1 column
        act_label <- read.table("./UCI/activity_labels.txt")
        newDataset <- join(newDataset, act_label, by="V1")
        ##After joining column V1 becomes duplicate so drop it
        newDataset <- select(newDataset, -(V1))

##Step 4        
        ## Change column names to be more readable
        colnames(newDataset) <- c( "td_BodyAcc_mean_X"
                                   ,"td_BodyAcc_mean_Y"
                                   ,"td_BodyAcc_mean_Z"
                                   ,"td_BodyAcc_std_X"
                                   ,"td_BodyAcc_std_Y"
                                   ,"td_BodyAcc_std_Z"
                                   ,"td_GravityAcc_mean_X"
                                   ,"td_GravityAcc_mean_Y"
                                   ,"td_GravityAcc_mean_Z"
                                   ,"td_GravityAcc_std_X"
                                   ,"td_GravityAcc_std_Y"
                                   ,"td_GravityAcc_std_Z"
                                   ,"td_BodyAccJerk_mean_X"
                                   ,"td_BodyAccJerk_mean_Y"
                                   ,"td_BodyAccJerk_mean_Z"
                                   ,"td_BodyAccJerk_std_X"
                                   ,"td_BodyAccJerk_std_Y"
                                   ,"td_BodyAccJerk_std_Z"
                                   ,"td_BodyGyro_mean_X"
                                   ,"td_BodyGyro_mean_Y"
                                   ,"td_BodyGyro_mean_Z"
                                   ,"td_BodyGyro_std_X"
                                   ,"td_BodyGyro_std_Y"
                                   ,"td_BodyGyro_std_Z"
                                   ,"td_BodyGyroJerk_mean_X"
                                   ,"td_BodyGyroJerk_mean_Y"
                                   ,"td_BodyGyroJerk_mean_Z"
                                   ,"td_BodyGyroJerk_std_X"
                                   ,"td_BodyGyroJerk_std_Y"
                                   ,"td_BodyGyroJerk_std_Z"
                                   ,"td_BodyAccMag_mean"
                                   ,"td_BodyAccMag_std"
                                   ,"td_GravityAccMag_mean"
                                   ,"td_GravityAccMag_std"
                                   ,"td_BodyAccJerkMag_mean"
                                   ,"td_BodyAccJerkMag_std"
                                   ,"td_BodyGyroMag_mean"
                                   ,"td_BodyGyroMag_std"
                                   ,"td_BodyGyroJerkMag_mean"
                                   ,"td_BodyGyroJerkMag_std"
                                   ,"fd_BodyAcc_mean_X"
                                   ,"fd_BodyAcc_mean_Y"
                                   ,"fd_BodyAcc_mean_Z"
                                   ,"fd_BodyAcc_std_X"
                                   ,"fd_BodyAcc_std_Y"
                                   ,"fd_BodyAcc_std_Z"
                                   ,"fd_BodyAcc_mean_FreqX"
                                   ,"fd_BodyAcc_mean_FreqY"
                                   ,"fd_BodyAcc_mean_FreqZ"
                                   ,"fd_BodyAccJerk_mean_X"
                                   ,"fd_BodyAccJerk_mean_Y"
                                   ,"fd_BodyAccJerk_mean_Z"
                                   ,"fd_BodyAccJerk_std_X"
                                   ,"fd_BodyAccJerk_std_Y"
                                   ,"fd_BodyAccJerk_std_Z"
                                   ,"fd_BodyAccJerk_mean_FreqX"
                                   ,"fd_BodyAccJerk_mean_FreqY"
                                   ,"fd_BodyAccJerk_mean_FreqZ"
                                   ,"fd_BodyGyro_mean_X"
                                   ,"fd_BodyGyro_mean_Y"
                                   ,"fd_BodyGyro_mean_Z"
                                   ,"fd_BodyGyro_std_X"
                                   ,"fd_BodyGyro_std_Y"
                                   ,"fd_BodyGyro_std_Z"
                                   ,"fd_BodyGyro_mean_FreqX"
                                   ,"fd_BodyGyro_mean_FreqY"
                                   ,"fd_BodyGyro_mean_FreqZ"
                                   ,"fd_BodyAccMag_mean"
                                   ,"fd_BodyAccMag_std"
                                   ,"fd_BodyAccMag_mean_Freq"
                                   ,"fd_BodyAccJerkMag_mean"
                                   ,"fd_BodyAccJerkMag_std"
                                   ,"fd_BodyAccJerkMag_mean_Freq"
                                   ,"fd_BodyGyroMag_mean"
                                   ,"fd_BodyGyroMag_std"
                                   ,"fd_BodyGyroMag_mean_Freq"
                                   ,"fd_BodyGyroJerkMag_mean"
                                   ,"fd_BodyGyroJerkMag_std"
                                   ,"fd_BodyGyroJerkMag_mean_Freq"
                                   ,"subject"
                                   ,"activity")
##Step 5
        
        ##Summarize data by subject and activity
        tidied <- ddply(newDataset, .(subject, activity), numcolwise(mean))
        write.csv(tidied, "myTidyDataset.csv")
        
}