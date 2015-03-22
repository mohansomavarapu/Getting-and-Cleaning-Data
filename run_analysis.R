## An analysis functions which merges mutiple datasets and produces a new tidy dataset 

run_analysis <- function() {
        
        install.packages("plyr")
        
        lbrary(plyr)
                
        features_data <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)  ## Reading the features.txt file which contains column names for values for x_train and x_test data sets
        activity_labels <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE) ## Reading the activity labels.txt file
        colnames(activity_labels) <- c("Activity_Label","Activity_Name")

        words <- c('mean','std') 
        colsToKeep <- sub(',\\s','|',(toString(words))) ## generates a string which can be used for pattern matching which subsetting the required columns 
        
        ## Code for reading the training data set
        
        subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt",stringsAsFactors=FALSE,col.names="Subject")
        x_train <- read.table("UCI HAR Dataset/train/X_train.txt",stringsAsFactors=FALSE)
        colnames(x_train) <- features_data$V2
        x_meanstd <- grepl(colsToKeep,colnames(x_train)) ## To make sure only mean and std deviation related columns are read
        x_finaltrain <- x_train[,x_meanstd] 
        y_train <- read.table("UCI HAR Dataset/train/y_train.txt",stringsAsFactors=FALSE,col.names="Activity_Label")
        
        ## Generating a single data set for the entire training data
        train_data <- cbind(subject_train,y_train,x_finaltrain)
        
        ## Code for reading test data
        
        subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt",stringsAsFactors=FALSE,col.names="Subject")
        x_test <- read.table("UCI HAR Dataset/test/X_test.txt",stringsAsFactors=FALSE)
        colnames(x_test) <- features_data$V2
        x_meanstd <- grepl(colsToKeep,colnames(x_test))
        x_finaltest <- x_test[,x_meanstd]
        y_test <- read.table("UCI HAR Dataset/test/y_test.txt",stringsAsFactors=FALSE,col.names="Activity_Label")
        
        ## Generating a single data set for the entire test data
        test_data <- cbind(subject_test,y_test,x_finaltest)
        
        
        ## Combining the training and test data
        merged_data <- rbind(train_data,test_data)
        
        ## Replacing the activity Label columns values with descriptive values
        merged_data$Activity_Label <- as.factor(factor(merged_data$Activity_Label,levels=c(1,2,3,4,5,6),labels=activity_labels$Activity_Name))
        
        ## Calculating the mean of all the columns in the tidy data set grouped by Subject and Activity Label
        tidy_data <- ddply(merged_data,.(Subject,Activity_Label),numcolwise(mean))
        
        ## Writing the tidy_data to an output file
        write.table(tidy_data,file="tidy_data.txt",row.names=FALSE)
        
}
