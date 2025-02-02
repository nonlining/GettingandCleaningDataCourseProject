# Getting and Cleaning Data Course Project

Review criteria
-----------------

1.The submitted data set is tidy.
2.The Github repo contains the required scripts.
3.GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
4.The README that explains the analysis files is clear and understandable.
5.The work submitted for this project is the work of the student who submitted it.

Here are the data for the project:

	https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called `run_analysis.R` that does the following. 

Objectives
-----------------

run_analysis.R performs the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each
   measurement.
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names. 
5. Creates a second, independent tidy data set with the average of each
   variable for each activity and each subject. 

Run script and result 
-----------------
1. run run_analysis.R
2. run_analysis will download and extract data for this project 
3. There are 2 files generated by running script
4. File, tidy.txt, is the result for merging the mean and standard deviation from training and test sets. This is the first required tidy data for this project.
5. File, tidy2.txt, is independent tidy data set with the average of each variable for each activity and each subject. This is the second requirement tidy data for this project.

