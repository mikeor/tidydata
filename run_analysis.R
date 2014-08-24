run_analysis <- function( wkDir="C:/Users/jieyi/Desktop/R Programming" ){
  
  # Code to work with 
  
  # 1. Merge the training and the test sets to create one data set.
  # 2. Extract only the measurements on the mean and std for each measurement. 
  # 3. Uses descriptive activity names to name the activities in the data set
  # 4. Appropriately labels the data set with descriptive variable names. 
  # 5. Creates a second, independent tidy data set with the average of each 
  #     variable for each activity and each subject. 
  
  setwd( wkDir)
  
  # Construct the test df
  test <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/test/X_test.txt")
    
  # Appropriately labels the data set with descriptive variable names. 
  colNames <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/features.txt" )
  colnames( test ) <- colNames$V2
    
  # Set dataSet train=1 test=2 as variable in test
  dataSet <- vector( "integer", nrow( test ) )
  dataSet[] <- 1
  test <- cbind( dataSet = dataSet, test )
    
  # Read in the activity features and add as a variable
  activity <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/test/y_test.txt")
  test <- cbind( activity = activity$V1, test )
    
  # Read in subject ids and add as a variable
  subject <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/test/subject_test.txt")
  test <- cbind( subject = subject$V1, test )
    
  # Now read in the train data set, similar to the above
  # Construct the train df
  
  train <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/train/X_train.txt")
    
  # Appropriately labels the data set with descriptive variable names. 
  colNames <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/features.txt" )
    
  colnames( train ) <- colNames$V2
    
  # Set dataSet train=1 train=2 as variable in /train
  dataSet <- vector( "integer", nrow( train ) )
  dataSet[] <- 2
  train <- cbind( dataSet = dataSet, train )
    
  # Read in the activity features and add as a variable
  activity <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/train/y_train.txt")
  train <- cbind( activity = activity$V1, train )
    
  # Read in subject ids and add as a variable
  subject <- read.table( "getdata_projectfiles_UCI HAR DatasetA/UCI HAR Dataset/train/subject_train.txt")
  train <- cbind( subject = subject$V1, train )
  
  # Merges the training and the test sets to create one data set.
  ds <- rbind( test, train )
    
  # Extracts only the measurements on the mean and standard deviation for each measurement.
  dataset <- ds[, grep( "subject|activity|dataSet|mean|std", colnames(ds) ) ]
  
  # Uses descriptive activity names to name the activities in the data set
  act_label <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING" )
  dataset$activity <- factor( dataset$activity,labels=act_label)
  
  # Use aggregate() to creates a second, independent tidy data set with the average of each variable for 
  # each activity and each subject.
  
  summary <- aggregate( dataset[,c(-1,-2,-3)], list(dataset$subject,dataset$activity), mean)
  colnames( summary )[1] <- "subject"
  colnames( summary )[2] <- "activity"
  
  write.table( summary, file="tidydata.txt",  row.name=FALSE )

}
