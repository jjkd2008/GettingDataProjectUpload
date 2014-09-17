## script run_analysis.R to process data from a smartphone
## the script is dependent upon having access to the following data set:
## getdata-projectfiles-UCI HAR Dataset.csv

## we perform 5 separate actions:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation 
## for each measurement. 
## 3. Create descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names. 
## 5. From the data set in step 4, creates a second, independent tidy data set 
## with the average of each variable for each activity and each subject.

## dependencies
## this code assumes that the plyr package has been installed
require(plyr)
require(dplyr)

## first check to make sure the proper files exists in the working directory, if not
## exit the script returning a message to please download the file
{
    if(!(file.exists("./activity_labels.txt") & 
             file.exists("./features.txt") &
             file.exists("./test/subject_test.txt") &
             file.exists("./test/X_test.txt") &
             file.exists("./test/Y_test.txt") &
             file.exists("./train/subject_train.txt") &
             file.exists("./train/X_train.txt") &
             file.exists("./train/Y_train.txt") ) ){
        print("At least one required file does not exist. \n")
        print("Please download from: \n")
        print("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip ")
        print("Unzip to the working directory where the run_analysis.R file is situated.")
        stop("Stopping...Goodbye!")
    }
    print("All files found...good to go")
    
## Project step 1:Merges the training and the test sets to create one data set.
    ## read in the datasets into appropriately named variables
    x_test<-read.table("./test/X_test.txt")
    y_test<-read.table("./test/Y_test.txt")
    x_train<-read.table("./train/X_train.txt")
    y_train<-read.table("./train/Y_train.txt")
    subject_test<-read.table("./test/subject_test.txt")
    subject_train<-read.table("./train/subject_train.txt")

    ## combine the x,y& subject datasets into full datasets
    x_full<-rbind(x_train,x_test)
    y_full<-rbind(y_train,y_test)
    subject_full<-rbind(subject_train,subject_test)
    
## Project step 2: Extract mean and standard deviation for each measurement
    ## first map the columns in the x_full data frame to the names in the
    ## features.txt file
    
    feature_names<-read.table("./features.txt",stringsAsFactors=FALSE,
                              col.names=c("VarNum","VarName"))
    ## use the VarName col in feature_names to name the variables in x_full
    names(x_full)<-feature_names$VarName
    
    ## filter the columns so that only those variables including "-mean", "-std"
    ## do not take the columns that include "Mean" in the Name prior to the demarcation
    ## for the statistic reported (ie, not listed as a function mean() and std())
    ## also strip the "meanFreq" columns

    x_filt<-select(x_full,contains("-mean"),contains("-std"),-contains("-meanFreq"))

    ## bind the filtered x data and the subject data together

    x_filt_sub<-cbind(x_filt,subject_full)
    
    
## Project step 3: Create activities with nice descriptive names for each activity
    
    ## map activity labels from the activity_labels.txt file to the items in y_full
    act_labs<-read.table("activity_labels.txt",stringsAsFactors=FALSE)
    mfr<-act_labs$V1
    mto<-act_labs$V2
    y_full$labels<-mapvalues(y_full$V1,from=mfr,to=mto)

    ## bind the activity label column to the filtered data + subject
    
    data_full<-cbind(x_filt_sub,y_full$labels)
## Project Step 4: Create descriptive variable names
    ## For full explanation of the variable names, please see the code book
    ## Note the var names for the x_test/x_train data was applied above prior
    ## to filtering

    ## name the subject column appropriately
    names(data_full)[67]<-"Subject"
    ## name the Activities column appropriately
    names(data_full)[68]<-"Activity"

    
##Project step 5: Create and write out a tidy data set with the following:
    ## Row for average for each variable for each activity and each subject
    
    ## use the aggregate function to find the mean (average) of each variable
    ## in the data set as grouped by acivity and subject

    tidy_data<-aggregate(.~Activity+Subject,data=data_full,mean)

    ## write the dataset out to a comma delimited file including a header as 
    ## first row in the file
    write.table(tidy_data,file="./tidy_data_set.txt",row.names=FALSE,sep=",")

    ## Note: should be able to read in with read.csv(file="./tidy_data_set.txt")
    
}