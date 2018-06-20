library(data.table)
library(qdap)
library(dplyr)

#~~~~~read all data~~~~~#
x_train=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/train/X_train.txt")
y_train=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/train/Y_train.txt")
x_test=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/test/X_test.txt")
y_test=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/test/Y_test.txt")
subject_train=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/train/subject_train.txt")
subject_test=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/test/subject_test.txt")
features=read.table("C:/Users/ram.p/Documents/R/UCI HAR Dataset/features.txt")

#~~~~~creating variable names~~~~~#
feat=as.list(t(features[,2]))
feat[[562]]="subject"

#~~~~~Training data - Adding Subject as a column~~~~~#
train0=cbind(x_train,subject_train)

#~~~~~Training data - Adding names to variables~~~~~#
train1_data=rbind(feat,train0)

#~~~~~Training data - Adding y var~~~~~#
y_train_data=rbind("y_val",y_train)
fin_train=cbind(train1_data,y_train_data)

#~~~~~Test data - Adding Subject as a column~~~~~#
test0=cbind(x_test,subject_test)

#~~~~~Test data - Adding y var~~~~~#
fin_test=cbind(test0,y_test)

#~~~~~merged data~~~~~#
fin_data=rbind(fin_train,fin_test)

#~~~~~merged data - Identifying var with mean() and std()~~~~~#
colnames(fin_data)=fin_data[1,]
findata=fin_data[-1,]
cnames=names(findata)
mean_name=grepl("mean()",cnames)
std_name=grepl("std()",cnames)
misc_name1=grepl("subject",cnames)
misc_name2=grepl("y_val",cnames)
findata1=findata[,mean_name|std_name|misc_name1|misc_name2]

#~~~~~filtered data - organising var names~~~~~#
findata2=findata1
dummy=levels(factor(findata1[,81]))
alter=c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
findata2[,81]=mgsub(dummy,alter,findata2[,81])
findata2=rename(findata2,activity_name=y_val)
namesofdata=names(findata2)
namesofdata=tolower(namesofdata)
namesofdata=mgsub(c("-","_"),".",namesofdata)
namesofdata=mgsub("()","",namesofdata)
names(findata2)=namesofdata

#~~~~~Tidy Data~~~~~#
findata2[,1:79]=sapply(findata2[,1:79],as.numeric)
findata2

#~~~~~Independent Tidy data - Summarizing~~~~~#
findata2$activity.name <- as.factor(findata2$activity.name)
findata2$subject <- as.factor(findata2$subject)
final.melted <- melt(findata2, id = c("subject", "activity.name"))
final.melted.mean <- dcast(final.melted, subject + activity.name ~ variable, mean)
write.table(final.melted.mean,"independent_tidy.txt")


