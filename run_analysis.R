## Load libraries
library(dplyr)


## Download zip datasets
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "wearableTechDatasets.zip")
downloadDate <- date()
unzip(zipfile = "wearableTechDatasets.zip")


## Load activity labels and feature information
activitylabels <- read.csv("./UCI HAR Dataset/activity_labels.txt", header = FALSE, sep = "", col.names = c("activity", "activityDesc"))
features_info <- read.csv("./UCI HAR Dataset/features_info.txt", header = FALSE)
features <- read.csv("./UCI HAR Dataset/features.txt", header = FALSE, sep = "")
feat_colnames <- as.character(features$V2)


## Import and combine test and train datasets, and label the variables
datasets <- c("test", "train")
filePatterns <- c("subject", "[Yy]_", "[Xx]_")
for (i in 1:length(datasets)) {
    pathDir <- paste("./UCI HAR Dataset/", datasets[i], "/", sep="")
    for (j in 1:length(filePatterns)) {
        dataFile <- dir(pathDir, pattern = filePatterns[j])
        if (exists("dataSubset")==FALSE) {
            dataSubset <- read.csv(file=paste(pathDir, dataFile, sep=""), header = FALSE, sep="")
        } else {
            dataSubset <- cbind(dataSubset, read.csv(file=paste(pathDir, dataFile, sep=""), header = FALSE, sep=""))
        }
    }
    
    if (exists("mydata")==FALSE) {
        mydata <- dataSubset 
    } else {
        mydata <- rbind(dataSubset, mydata)
    }
    rm(dataSubset)
}
colnames(mydata) <- c("subject", "activity", feat_colnames)


## Filter only the variables on mean and standard deviation for each measurement
mean_cols <- grep("mean()", colnames(mydata), fixed = T)
std_cols <- grep("std()", colnames(mydata), fixed = T)
mydata_subset <- mydata[c(1,2,sort(c(mean_cols, std_cols)))]
mydata_subset <- merge(activitylabels, mydata_subset)



##-----------------------------------------------------------------------------------
## Check that you only have 30 subjects, and 6 different activites
# dim(mydata)
# unique(mydata["subject"])
# n_distinct(mydata["subject"])
# unique(mydata["activity"])
# n_distinct(mydata["activity"])


##-----------------------------------------------------------------------------------
## Create tidy dataset
mydata_tidy <- mydata_subset %>% group_by(subject, activity) %>% select(., 4:69) %>% summarise_all(., mean)


##-----------------------------------------------------------------------------------
## Write .txt file with tidy dataset
write.table(mydata_tidy, file="mydata.txt", row.names = F)

