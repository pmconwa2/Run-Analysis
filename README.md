## Course Project: Getting and Cleaning Data
## Submitted by Pete Conway
##
## This script extracts data collected from the accelerometers and gyroscopes
## from the Samsung Galaxy S smartphone used by 30 subjects performing 6
## activities. This script:
##              - Merges the training and test data sets into a single data set
##              - Extracts only the measurements related to mean and std
##              - Descriptively names each activity
##              - Appropriately labels the data set with descriptive variable names
##              - Constructs a second, independent tidy data set with the average 
##              - of each variable for each activity and each subject
##
## This README file contains the entire script with comments describing each
## action performed by the script.

## Make sure dplyr is initiated so we can utilize it
        library(dplyr)
        
## First, make sure a directory exists to store our data file
        if(!file.exists("data")) {dir.create("data")}
        
## Download the zip file into the data directory
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                      destfile = "./data/dataset.zip",
                      method = "curl")

## Unzip the file into the data directory
        unzip("./data/dataset.zip", exdir = "./data")
        
## Change our working directory so we can access our unzipped data
        setwd("./data/UCI HAR Dataset")
        
## After reading the README.txt file, the data files we are interested in
## are spread out among various directories.
## In the test directory (UCI HAR Dataset/test), we are interested in the
## following files:
##      subject_test.txt        y_test.txt      X_test.txt
##
## We need to read in the data from each of these files and assign them to
## variables.
        subject_test <- read.table("./test/subject_test.txt")
        y_test <- read.table("./test/y_test.txt")
        x_test <- read.table("./test/X_test.txt")
        
## In the train directory (UCI HAR Dataset/train), we are interested in the
## following files:
##      subject_train.txt       y_train.txt     X_train.txt
##
## Let's do the same thing we did for the files in the test directory.
        subject_train <- read.table("./train/subject_train.txt")
        y_train <- read.table("./train/y_train.txt")
        x_train <- read.table("./train/X_train.txt")
    
## The README.txt file provides some helpful information about our data.
## x_test and x_train each contain 561 variables that provide information
## on each of the 30 subjects in any of the 6 activities.
##
## The activity_labels.txt file describes each of the 6 activities the 
## subjects participated in. These activities correspond to the values (1-6)
## in the data sets, y_test and y_train.
## 
## The features.txt file provides 561 variable names for each of the 561 
## columns in x_test and x_train, which we will use shortly to change the 
## column names in those data sets.
##
## The features_info.txt file provides further information on the features
## variables.
## 
## subject_test and subject_train provide corresponding subject ids for each
## row in y_test & x_test and y_train & x_train, respectively
##
## Now that we know we have subject ids, activity labels, and test/train data,
## we can bind some of our data sets together. We are going to bind y_test to
## x_test as the leftmost column. We will also bind subject_test to the result
## of the previous step as the leftmost column. We will store the result of
## our bindings in a new data set called test.
## We also need to do this for our train data, storing the result in a new
## data set called train.
## Then, we can bind the train data under the test data to create one data set,
## storing the result in allData.
        test <- cbind(subject_test, y_test, x_test)
        train <- cbind(subject_train, y_train, x_train)
        allData <- rbind(test,train)

## We have combined our data, reducing 6 unwieldy data sets into 1 more
## manageable set.
##
## Our next step involves extracting mean and standard deviation variables
## from our data set. How can we do that when all of the columns are
## alphanumeric descriptors that give us no information on the actual variables?
## Well, we can use the features.txt file to rename our columns.
##
## First, let's read the features.txt file into our working directory
## and assign it to a variable called features
        features <- read.table("features.txt")
        
## Now let's rename the columns in allData. Don't forget that the first
## two columns are the subject ids and activity labels, respectively.
        colnames(allData) <- c("subject", "activity", features$V2)
        
## Now how do we select only the variables that pertain to mean and 
## standard deviation? We use grepl to find all columns with either "mean" or
## "std" and select() those columns in allData. We store the result in a 
## variable called allDataExt (all data extracted)
        allDataExt <- select(allData, c("subject", "activity", 
                                grep("[Mm]ean|[Ss]td", colnames(allData))))
        
## Our next step is to change the activity number values into actual 
## descriptive names. Remember what each activity number corresponds to:
##      1 - Walking
##      2 - Walking upstairs
##      3 - Walking downstairs
##      4 - Sitting
##      5 - Standing
##      6 - Laying
## These names seem descriptive enough for our purposes. Let's use sapply()
## to go through each value in the activity columns in test and train and 
## change them to descriptive names
        x = allDataExt$activity
        allDataExt$activity <- sapply(allDataExt$activity, function(x) 
                ifelse(x == 1, "walking", ifelse(x == 2, "walkingUpstairs", 
                ifelse(x == 3, "walkingDownstairs", ifelse(x == 4, "sitting", 
                ifelse(x == 5, "standing", ifelse(x == 6, "laying")))))))
        
## Our column names are descriptive but a bit messy. Let's replace some of
## the characters in the column names to make our data a bit more tidy. The
## only special characters we want in column names are ".". Any other special
## characters like "-", "()", etc need to be removed or replaced. We can 
## accomplish this task by using gsub()
        colnames(allDataExt) <- gsub("-", ".", colnames(allDataExt))
        colnames(allDataExt) <- gsub("\\()", "", colnames(allDataExt))
        colnames(allDataExt) <- gsub("^t", "time.", colnames(allDataExt))
        colnames(allDataExt) <- gsub("^f", "freq.", colnames(allDataExt))
        
## The next step is to create a second, tidy data set from allDataExt that takes
## the average of each variable for each activity and each subject. This means
## we need to group our data set by subject then by activity, and then we can
## go through each column to calculate the mean value for a specific subject
## performing a specific activity.
        allDataMeans <- group_by(allDataExt, subject, activity)
        allDataMeans <- summarize_each(allDataMeans, funs(mean))

## Now let's write our tidy data set into a file
        write.table(allDataMeans, file = "allDataMeans.txt", row.name = FALSE)