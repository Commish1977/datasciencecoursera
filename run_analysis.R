## Getting and Cleaning Data Peer Assessment
##  2015.02.22


## Step 1. Merges the training and the test sets to create one data set.
##    Assumption, working directory is R with training and test data in /data/w3 (for week 3 assessment)

	## Read in the Training data set, Training Labels, Training Subjects
	trainDataSet<-read.table("./data/w3/train/X_train.txt")
	trainLabelSet<-read.table("./data/w3/train/y_train.txt")
	trainSubjectDataSet<-read.table("./data/w3/train/subject_train.txt")

	## Read in the Test data set, Labels, Subjects
	testDataSet<-read.table("./data/w3/test/X_test.txt")
	testLabelSet<-read.table("./data/w3/test/y_test.txt") 
	testSubjectDataSet<-read.table("./data/w3/test/subject_test.txt")
	
	## Merge the Test and Training data set, Labels, Subjects
	mergeTrainAndTestData<-rbind(trainDataSet,testDataSet)
	mergeTrainAndTestLabel<-rbind(trainLabelSet, testLabelSet)
	mergeSubjectDataSet<-rbind(trainSubjectDataSet, testSubjectDataSet)


## Step2. Extracts only the measurements on the mean and standard deviation for each measurement. 
    ## read in the features.txt data set
	featureDataSet<-read.table("./data/w3/features.txt")
	## extract the indexes that either have 'mean' or 'std' in the 2nd column
	measurementIndexes <- grep("mean\\(\\)|std\\(\\)", featureDataSet[, 2])
	## those indexes align with the merged training and test data above, extract removing parenthesis
	indexedTrainAndTestData <- mergeTrainAndTestData[, measurementIndexes]
	names(indexedTrainAndTestData) <- gsub("\\(\\)", "", features[meanStdIndices, 2])
	
# Step3. Uses descriptive activity names to name the activities in the data set
	## read in the activity data set labels text file
	activityDataSet<- read.table("./data/w3/activity_labels.txt")

# Step4. Appropriately labels the data set with descriptive activity names. 
	## matching corresponding activity labels in the master label data set
	activityLabelSet <- activityDataSet[mergeTrainAndTestLabel[, 1], 2]
	mergeTrainAndTestLabel[, 1] <- activityLabelSet
	## name the activity and subjects before column bind for step5 reference
	names(mergeTrainAndTestLabel) <- "activity"
	names(mergeSubjectDataSet) <- "subject"
	completedDataSet <- cbind(mergeSubjectDataSet, mergeTrainAndTestLabel, indexedTrainAndTestData)

# Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
	## create the resultDataSets matrix, need the sizes first to initialize
	numOfColumns <- dim(completedDataSet)[2]
	numOfActivities <- dim(activityDataSet)[1]
	numOfSubjects <- length(table(mergeSubjectDataSet))
	resultDataSet <- matrix(NA, nrow=numOfSubjects*numOfActivities, ncol=numOfColumns) 
	resultDataSet <- as.data.frame(resultDataSet)
	## copy over column names to the new data set
	colnames(resultDataSet) <- colnames(completedDataSet)
	## set my index for the row I will be saving too
	currentRow <- 1
	## loop through each subject
	for(sbj in 1:numOfSubjects) {
		## loop through each activity for the subject
		for(act in 1:numOfActivities) {
			resultDataSet[currentRow, 1] <- sort(unique(mergeSubjectDataSet)[, 1])[sbj]
			resultDataSet[currentRow, 2] <- activityDataSet[act, 2]
			subjectMatch <- sbj == completedDataSet$subject
			activityMatch <- activityDataSet[act, 2] == completedDataSet$activity
			## Perform column means where subject and activity match 
			resultDataSet[currentRow, 3:numOfColumns] <- colMeans(completedDataSet[subjectMatch&activityMatch, 3:numOfColumns])
			## increase row index after saving data
			currentRow <- currentRow + 1
		}
	}
	## write the tidy data set result for submission
	write.table(resultDataSet, "tidy_data_result.txt")
