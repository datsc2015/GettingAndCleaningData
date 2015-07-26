###Date:7/23/2015
###Modified by GWANG
###R script - run_analysis.R  for Course Getting and Cleaning Data
###Description: 
#1.Merges the training and the test sets to create one data set.
#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
#3.Uses descriptive activity names to name the activities in the data set
#4.Appropriately labels the data set with descriptive variable names. 
#5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

rm(list=ls())

#install.packages("RMySQL")
library(DBI)
library(RMySQL)
#ucscDb <- dbConnect(MySQL(),user="genome", host="genome-mysql.cse.ucsc.edu")
#result <- dbGetQuery(ucscDb, "show databases;")
#dbConnect(uscsDb)


#Need to set up home drive
setwd("C:\\Users\\GWANG1\\GetingandCleaningData")

#1.Merges the training and the test sets to create one data set.

trainx <- read.table("train/X_train.txt")
testx <- read.table("test/X_test.txt")

trainy <- read.table("train/y_train.txt")
testy <- read.table("test/y_test.txt")

trains <- read.table("train/subject_train.txt")
tests <- read.table("test/subject_test.txt")

x <-rbind(trainx, testx)
y <-rbind(trainy, testy)
s <-rbind(trains,tests )

act <-read.table("activity_labels.txt")
f <- read.table("features.txt")

#2.Extracts the mean and standard deviation for each measurement
# get the variables we want
# Get mean and std.
colms <- grep("-mean\\(\\)|-std\\(\\)", f[, 2]) 
length(colms)
#f <- f[,colms] 
#columns with Mean, Std
x <- x[, colms] 
names(x) <- f[colms, 2] 
names(x) <- gsub("\\(|\\)", "", names(x)) 
names(x) <- tolower(names(x)) 
dim(x)

#3.name the activities in the data set
act[,2] =gsub("_","",tolower(as.character(act[,2])))
y[,1] <-act[y[,],2]
names(y) <-"activity"
dim(y)


#4.labels the data set with descriptive variable names
names(s) <-"subject"
dat.clean <- cbind(s, y, x)
head(dat.clean)
#write.table(dat.clean , "all_clean_data.txt")

#5.creates a second, independent tidy data set with the average of each variable for each activity and each subject.
uniqsub <- unique(s)[,1]
numsub <- length(unique(s)[,1])
numact <- length(act[,1])
numc <- dim(dat.clean)[2]
tidy <- dat.clean[1:(numsub*numact),]
head(tidy)

r = 1 
for (s in 1:numsub) { 
    for (a in 1:numact) { 
            tidy[r, 1] = uniqsub[s] 
         tidy[r, 2] = act[a, 2] 
         cleanp <- dat.clean[tidy$subject==s & tidy$activity==act[a, 2], ] 
         tidy[r, 3:numc] = colMeans(cleanp[, 3:numc]) 
         r = r+1 
    } 
        } 
write.table(tidy, "dat.tidy.txt",row.name=FALSE) 
