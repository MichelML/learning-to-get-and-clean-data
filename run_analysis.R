##Prior note1: if you want the script to work appropriately,
#you will need to install the package "reshape2" for step 5.
#In order to run the script smoothly, users should install reshape2
#with the command install.packages("reshape2") before running the script.
#The reason I didn't put the "install.packages("reshape2") in the script
#is that if you already have it installed, it produces a query asking you
#that you must restart an R session before running the script, and we don't
#want that!
#I called the package from the library
#so you don't need to make it yourself.The script should
#run smoothly without any required additional task for the end user if reshape2
#is installed.
library(reshape2)
#Prior note2: Steps 1 to 5 have not been executed in the exact order
# proposed in the assignment. Viewers should however understand that the code
#will lead to the goal demanded: produce the tidy dataset of step 5.
#Enjoy the script!

#Pre-step:
#Here's how I downloaded the file and unzipped it.
#c3_P2.zip is just a name I chose in line with the course

file<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(file, destfile="c3_P2.zip")
unzip("c3_P2.zip")

#Step 1: Merging the training and test sets for observations variables only
#and labeling the data set with descriptive variable names

#extracting the data.set containing the names
#of the observations variables that will be used later 
features<-read.table("UCI HAR Dataset/features.txt", header=FALSE)
#retrieve only the vector for the variable names and exclude the row.name/number
feat2<-features[[2]]
#extracting the data set containing the observations variables for train-group
x_train<-read.table("UCI HAR Dataset/train/X_train.txt", header=FALSE, dec=".", numerals="no.loss")
#assign the names to each variables of the data set for observations variables
#of train group
colnames(x_train)<-feat2
##extracting the data set containing the observations variables for test-group
x_test<-read.table("UCI HAR Dataset/test/X_test.txt", header=FALSE, dec=".", numerals="no.loss")
##assign the names to each variables of the data set for observations variables
#of train group
colnames(x_test)<-feat2
#merge the train and test observation variables together by putting the 
#test data under the train data 
#see https://class.coursera.org/getdata-009/human_grading/view/courses/972587/assessments/3/submissions
#for more details
mergedData<-rbind(x_train,x_test) #Voila for step 1


#Step 2: Extracts only the measurements on the mean and standard 
#deviation for each measurement. 

#form a numerical vector of the column numbers for mean and standard deviation variables 
#only, that is those containing mean() or std() in the variable name
meanvectors<-grep("mean\\(\\)|std\\(\\)", feat2)
#use the vector previously created to form the dataset containing only
#the measurements on the mean and standard deviation for each measurement
mergedD1<-mergedData[meanvectors] #Voila for step 2


#Step 3: Merging the training and test sets for subjects and activities

#extracting the data set indicating which subject corresponds to each measurements
# of the observation variables of the dataset of step 2 (for train group and test group)
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt", header=FALSE, dec=".", numerals="no.loss")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt", header=FALSE, dec=".", numerals="no.loss")
#merge the train subjects data set and the test subjects dataset concordantly with 
#the data set created on step 2, that is, the test subjects under the train subjects
#see https://class.coursera.org/getdata-009/human_grading/view/courses/972587/assessments/3/submissions
#for more details
subject_tot<-rbind(subject_train,subject_test)
#extracting the data set indicating which activities corresponds to each measurements
# of the observation variables of the dataset of step 2 (for train group and test group)
y_train<-read.table("UCI HAR Dataset/train/y_train.txt", header=FALSE, dec=".", numerals="no.loss")
y_test<-read.table("UCI HAR Dataset/test/y_test.txt", header=FALSE, dec=".", numerals="no.loss")
#merge the data set for the activities of train group  and  test group concordantly with 
#the data set created on step 2, that is, the test subjects under the train subjects
#see https://class.coursera.org/getdata-009/human_grading/view/courses/972587/assessments/3/submissions
#for more details
y_tot<-rbind(y_train,y_test) #voilà for step 3

#step 4: Final activities and refinement
#4.1 merging the three data sets previously created (e.g. mergedD1, subject_tot, y_tot)
# in order to form the almost complete tidy dataset before step 5
#see https://class.coursera.org/getdata-009/forum/thread?thread_id=58#comment-369
#for more details
#4.2 Rename the subjects column and activity column, having no names for now
#4.3 Use descriptive activity names to name the activities in the data set


mergedDtot<-cbind(mergedD1,subject_tot,y_tot) #see 4.1 above
names(mergedDtot)[67]<-"Subject" #see 4.2 above
names(mergedDtot)[68]<-"Activity" #see 4.2 above
#4.3 finally set descriptive activity names/labels to the activities numerical level
mergedDtot$Activity<-factor(mergedDtot$Activity,
                            levels = c(1,2,3,4,5,6),
                            labels = c("walking","walking_upstairs","walking_downstairs","sitting","standing","laying"))

#Step 5: From the data set in step 4, creates a second, independent 
#tidy data set with the average of each variable for each activity and each subject.

#order the dataset by subject first and than by activities
finalset_step1<-mergedDtot[order(mergedDtot[,67], mergedDtot[,68]), ]
#Here is an important step using the package reshape2.
#melt put the data set in a "long format"  by using id variables.
#In other words, it sets the data.frame into a new data set format, where you will see
#each observations associated with your id variables on a different row.
#Anyway, it is just a prior step before the final "dcast" step.
finalset_step2<-melt(finalset_step1, id=c("Subject","Activity"))
#dcast "casts" a molten data frame into a new dataframe based using formulas
#that can take id variables and and aggregation. The results of the following
#line is the final tidy data set. See help(dcast) after installing reshape2 for more
#details
finalset<-dcast(finalset_step2, Subject + Activity ~ variable, fun.aggregate = mean, na.rm =TRUE)

#finally, create the text file into your main working directory, avoiding to include
#row.names
write.table(finalset, "tidy_finalset_means.txt", row.name=FALSE)
