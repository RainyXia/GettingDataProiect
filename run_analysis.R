#1
#读入数据
x_train <- read.table("train/X_train.txt")
dim(x_train)
subject_train <- read.table("train/subject_train.txt")
dim(subject_train)
y_train <- read.table("train/y_train.txt")
dim(y_train)
x_test <- read.table("test/X_test.txt")
dim(x_test)
subject_test <- read.table("test/subject_test.txt")
dim(subject_test)
y_test <- read.table('test/y_test.txt')
dim(y_test)
features <- read.table("features.txt")
dim(features)
#统一列名
names_vector <- features[,2]
names(x_train) <- names_vector
names(x_test) <- names_vector
names(subject_train) <- "subject"
names(subject_test) <- "subject"
names(y_train) <- "activity"
names(y_test) <- "activity"
#合并数据
train <- cbind(x_train,subject_train,y_train)
test <- cbind(x_test,subject_test,y_test)

dt <- rbind(train,test)
#2
#查找mean和std匹配项
grep1 <- grep("mean",names(dt),ignore.case=T)
grep2 <- grep("std",names(dt),ignore.case=T)

dt2 <- dt[,c(grep1,grep2)]
#去掉angle
grep3 <- grep("angle",names(dt2),ignore.case=T)
dt3 <- dt2[,-c(grep3)]
#3
class(dt$activity)
activity_labels <- read.table("activity_labels.txt",stringsAsFactors=F)
activity_levels <- activity_labels[,1]
activity_lables <- activity_labels[,2]
length(activity_labels)
#dt$activity <- factor(dt$activity,levels=activity_levels,labels=activity_labels)
dt$activity <- factor(dt$activity,levels=c(1,2,3,4,5,6),labels=c("WALKING","WALKING_UPSTAIRS",
                                                  "SITTING","STANDING","LAYING","LAYING"))
str(dt$activity)







#4
#step1中已完成
#5
dt_act <- split(dt[,1:561],dt$activity)
mean_act <- sapply(dt_act,colMeans)
library("reshape2")
mean_act <- as.data.frame(mean_act)
mean_act$mea <- rownames(mean_act)

melt_act <- melt(mean_act,id="mea",variable.name="a_s_value",value.name="mean")

dt_sub <- split(dt[,1:561],dt$subject)
mean_sub <- sapply(dt_sub,colMeans)
mean_sub <- as.data.frame(mean_sub)
mean_sub$mea  <- rownames(mean_sub)
melt_sub <- melt(mean_sub,id="mea",variable.name="a_s_value",value.name="mean")

#加一列标明是sub还是act
melt_act$a_s <- c(rep("activity",3366))
head(melt_act)
melt_sub$a_s <- c(rep("subject",16830))
head(melt_sub)
dt_melt <- rbind(melt_act,melt_sub)
write.table(dt_melt,file="tidydata.txt",row.name=FALSE)
