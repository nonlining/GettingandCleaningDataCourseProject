

dataDirectory <- "UCI HAR Dataset"

# this function for getting subject and activities from test and train sets
readSubActfile <- function(dataDirectory, filePath) {
  filePathTest <- paste(dataDirectory, "/test/", filePath, "_test.txt", sep="")
  filePathTrain <- paste(dataDirectory, "/train/", filePath, "_train.txt", sep="")
  data <- c(read.table(filePathTest)[,"V1"], read.table(filePathTrain)[,"V1"])
  data
}

# the function for read data from file and reassign data 

getTidyData <- function(dataDirectory) {
  
  featuresFilePath <- paste(dataDirectory, "/features.txt", sep="")
  features <- read.table(featuresFilePath)[,"V2"]

  meanIndex <- grep("mean\\(\\)", features)
  stdIndex <- grep("std\\(\\)", features)

  featureNamesMeanStd <-sort(c(meanIndex, stdIndex))

  rm(meanIndex)
  rm(stdIndex)

  features <- gsub("\\(", "", features)
  features <- gsub("\\)", "", features)

  # read from base data
  baseDatafilenameTest <- paste(dataDirectory, "/test/X_test.txt", sep="")
  baseDatafilenameTrain <- paste(dataDirectory, "/train/X_train.txt", sep="")

  cols_widths <- rep(-16, length(features))
  cols_widths[featureNamesMeanStd] <- 16
  
  baseDataTest <- read.fwf(file=baseDatafilenameTest, widths=cols_widths, col.names=features[featureNamesMeanStd])
  baseDataTrain <- read.fwf(file=baseDatafilenameTrain, widths=cols_widths, col.names=features[featureNamesMeanStd])
  
  baseData<-rbind(baseDataTest, baseDataTrain)
  
  # to lower case data names and remove all dot from variable names
  names(baseData) <-gsub("\\.", "",tolower(names(baseData)))
  
  # get subject and activities from files
  
  subject <- readSubActfile("UCI HAR Dataset", "subject")
  
  activitiesFilePath <- paste(dataDirectory, "/activity_labels.txt", sep="")

  actLabel <- read.table(activitiesFilePath)[,2]
  
  actLabel = tolower(sub("_","",actLabel))
  
  active = readSubActfile("UCI HAR Dataset", "y")

  activity <- actLabel[active]
  
  #merge subject, activity and baseData
  tData <- cbind(subject, activity, baseData)
  
  tData
}



#download and extract zip package from url

if (!file.exists(dataDirectory)) {
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  tempfile <- "./tempData.zip"
  download.file(url,tempfile)
  unzip(tempfile)
  unlink(tempfile)
}

tData <- getTidyData(dataDirectory)


#write first tidy data set
write.table(tData, "tidy.txt",row.names = F)

tData2 <- tData[,seq(3, length(names(tData)))]

newData = aggregate(tData2,by = list(tData$subject, tData$activity), FUN=mean)

newData = newData[order(newData$Group.1),]

colnames(newData)[colnames(newData)=="Group.1"] <- "subject"
colnames(newData)[colnames(newData)=="Group.2"] <- "activity"


#write second tidy data set
write.table(newData, "tidy2.txt",row.names = F)