# NAME : mergeDataset
# INPUT : Name of the root directory that contains the files to merge
#   The directory should contains two subdirectories in which the 6 files are stored 
#      'train/X_train.txt': Training set.
#       ....
#      'test/X_test.txt'  : Test set.
#       ....
# OUTPUT : A dataframe  with the required data (Q1 of the project)
#
# COMMENTS :
#   Raw Data from files 
#
library(plyr)
library(dplyr)

MergeDataset <- function(directory) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    ##wd<-paste(directory,collapse=" ")
    if (!is.null(directory)) {
        setwd(directory)
    }
    else {
        printf("Invalid directory!")
        return(1)
    }
    print(directory);
    
    # The results of the training activity, The subjects who performed the training ,& the activity file
    trainFile<- "X_train.txt";
    subTrainFile<- "subject_train.txt";
    actTrainFile<- "y_train.txt";
    
    # The results of the test activity, The subjects who performed the tests ,& the activity file    
    testFile<- "X_test.txt";
    subTestFile<- "subject_test.txt";
    actTestFile<- "y_test.txt";
    
    setwd("test");
    
    #c=rep(16,561)
    # TOO SLOW AND GREEDY IN MEMORY
    #myTrSet = read.fwf(TrainFile, widths = c, numerals = c("no.loss"), header= FALSE, buffersize = 8177);
    #myTestSet = read.fwf(TestFile, widths = c, numerals = c("no.loss"), header= FALSE, buffersize = 8177);
    #myTestSet = read.fwf(TestFile, widths = c, numerals = c("no.loss"), header= FALSE);
    df <- data.frame(matrix(vector(), 0, 561))
#    myTestSet = read.csv(TestFile, sep=" ", header = FALSE);
    myTestSet <- read.csv(testFile, sep="", header = FALSE);
    for (i in seq(1:nrow(myTestSet))) {
        #print(i)
        t<-myTestSet[i,];
        #print(t);
        #c<-!is.na(t)
        #t[c]
        #t[!is.na(t)]
        df[i,] <-t[!is.na(t)]
    }

    myTestSub<-read.csv(subTestFile, sep="", header = FALSE);
    myTestAct<-read.csv(actTestFile, sep="", header = FALSE);

    setwd("../train");
    myTrainSet = read.csv(trainFile, sep="", header = FALSE);
    j<-nrow(myTestSet);
    for (i in seq(1:nrow(myTrainSet))) {
        #print(i)
        t<-myTrainSet[i,];
        #print(t);
        #c<-!is.na(t)
        #t[c]
        #t[!is.na(t)]
        k<-j+i
        df[k,] <-t[!is.na(t)]
        }

    myTrainSub<-read.csv(subTrainFile, sep="", header = FALSE);
    myTrainAct<-read.csv(actTrainFile, sep="", header = FALSE);
    # Add a new column at the end of the dataframe (position 562) : The subject Ids
    sub<-rbind(myTestSub,myTrainSub);
    df<-cbind(df,sub,deparse.level=0);
    colnames(df)[562] <- "SubjectId";
    # Add a new column at the end of the dataframe (position 563) : The Actvity Ids
    act<-rbind(myTestAct,myTrainAct);
    df<-cbind(df,act,deparse.level=0);
    colnames(df)[563] <- "Activities";

#    dflist=list(myTestSub,myTrainSub)
#    join_all(dflist)

    # BE CAREFUL TO DISPLY WHICH IS LIMITED TO 8 characters
    #  print(v[555]+v[561],digits=10)
    #mergedData <- merge(myTrSet, myTestSet)
    #dim(df)
    return(df)
    }

# NAME : GetColnameWithMeanAndStd
# INPUT : filename , directory
#   The name of the file to parse ("features.txt")
#   The directory where file is located
#
# OUTPUT : A list of names to select
#
# COMMENTS :
#
GetColnameWithMeanAndStd <- function(directory,filename="features.txt") {
    if (!is.null(directory)) {
        setwd(directory)
    }
    else {
        printf("Invalid directory!")
        return(1)
    }
    print(directory);
    #m<-sapply(df,mean);
    #sd<-sapply(df,sd);
    #fdf <- filter(df, df$X1 == m["X1"] & df$X2 == m["X2"]);
    
    feat<-read.csv(filename, sep="", header = FALSE);
    #     dim(feat)
    # Select the variable with Mean and Std
    #p<-sapply(feat, function(a) (grep("Mean|std", a, ignore.case=TRUE)))
    # create Variable in Data.frame
    p<-apply(feat["V2"],2, function(a) (grep("-mean()|-std()", a, ignore.case=TRUE)))
    #varname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    #varname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    #colname<-sapply(p, function(a) (paste(c("ds$", "X",a),collapse="")))
    colname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    return(colname)
    }

# NAME : ExtractMeanAndStd
# INPUT : df , colname 
#   df       Dataframe to parse 
#   colname  The name list of columns to select
#
# OUTPUT : A dataFrame with specific Data (Q2 of the Project)
#
# COMMENTS :
#
ExtractMeanAndStd <- function(df,colname) {
    vec<-as.vector(colname);
    # Add Subject & activity to the list
    sc<-c(vec,"SubjectId","Activities")
    df[,sc];
    }

# NAME : GetActivityName
# INPUT : filename , directory
#   The name of the file to parse ("activity_labels.txt")
#   The directory where file is located
#
# OUTPUT : A two columns matrix with Activity Id & Activity names
#
# COMMENTS :
#
GetActivityName <- function(directory,filename="activity_labels.txt") {
    if (!is.null(directory)) {
        setwd(directory)
    }
    else {
        printf("Invalid directory!")
        return(1)
    }
    print(directory);
    
    act<-read.csv(filename, sep="", header = FALSE);
    #     dim(feat)
    # Select the variable with Mean and Std
    #p<-sapply(feat, function(a) (grep("Mean|std", a, ignore.case=TRUE)))
    # create Variable in Data.frame
    #p<-apply(feat["V2"],2, function(a) (grep("mean|std", a, ignore.case=TRUE)))
    #varname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    #varname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    #colname<-sapply(p, function(a) (paste(c("ds$", "X",a),collapse="")))
    #actname<-sapply(p, function(a) (paste(c("X",a),collapse="")))
    return(act)
}


# NAME : RenameActivity
# INPUT : df , colname 
#   df      Dataframe to parse 
#   nameid  The list of id/names to use in translation
#
# OUTPUT : The dataframe modified
#
# COMMENTS :
#
RenameActivity <- function(df,nameid) {
    for (j in seq(1:nrow(df))) {
        #nameid[i,1],nameid[i,2]
        #df[,(df$Activities == nameid[i,1])] = nameid[i,2]
        for (i in seq(1:nrow(nameid))) {
            if(df$Activities[j] == nameid[i,1]) {
                df$Activities[j]<-as.character(nameid[i,2])
                }
            }
        #ds2[1,"Activities"]<-4
        #df <- as.data.frame(lapply(df, function(x){replace(x, (df$Activities == nameid[i,1]),nameid[i,2])})
        }
    return(df)
    }



# NAME : GetLabelName
# INPUT : filename , directory
#   The name of the file to parse ("features.txt")
#   The directory where file is located
#
# OUTPUT : A list of label names
#
# COMMENTS :
#
GetLabelName <- function(directory,filename="features.txt") {
    if (!is.null(directory)) {
        setwd(directory)
    }
    else {
        printf("Invalid directory!")
        return(1)
    }
    print(directory);
    
    feat<-read.csv(filename, sep="", header = FALSE);
    p<-apply(feat["V2"],2, function(a) {k<-grepl("-mean()|-std()", a, ignore.case=TRUE)} )
    
    collabel<-feat[p,"V2"]
    # Clean data
    collabel<-sub("BodyBody","Body",collabel)
    collabel<-sub("\\(\\)","",collabel)
    collabel<-gsub("-","_",collabel)
    collabel<-sub("^t","time",collabel)
    collabel<-sub("^f","freq",collabel)
    return(collabel)
}


# NAME : LabelDataframe
# INPUT : df , labelname, pname
#   The dataframe to modify
#   The List of names ordered by column (NewName)
#   The position of the parameter (OldName)
#
# OUTPUT : A dataframe
#
# COMMENTS :
#
LabelDataframe <- function(df,labelname,pname) {
    #ds4 <- rename(ds3, c("Activities" = "dptp","SubjectId" = "pm25tmean2" ))
    w<-c()
    wo<-c()
    for (i in seq(1:length(labelname))) {
        #oln<-paste(c("X",i),collapse="")
        oln<-as.character(pname[i])
        wo<-c(wo, oln)
        nn<-as.character(labelname[i])
        w<-c(w, nn)
        }
    names(w) = wo
    df<-plyr::rename(df, w)

    return(df)
}

# NAME : MakeTidyDs
# INPUT : df 
#   The dataframe to summarize
#
# OUTPUT : A dataframe
#
# COMMENTS :
#
MakeTidyDs <- function(df,labelname) {
    g2 <- group_by(df, SubjectId,Activities)
    var<-c()
    varname<-c()
#    for (i in seq(1:length(labelname))) {
#        #oln<-paste(c("X",i),collapse="")
#        oln<-as.character(pname[i])
#        wo<-c(wo, oln)
#        nn<-as.character(labelname[i])
#        w<-c(w, nn)
#    }
#i<-"timeBodyAcc_mean_X"
#td<-paste(c("mean(",i,")"),collapse="")
#var<-c(var, td)
#varname<-c(varname,"MeanX1")
#    names(var) = varname

#    ds<-summarize(g2,
#                  timeBodyAcc_mean_mean = mean(timeBodyAcc_mean_X),
#                  timeBodyAcc_mean_Y_mean = mean(timeBodyAcc_mean_Y)
#                  )


ds<-summarize(g2,
timeBodyAcc_mean_X_mean =mean(timeBodyAcc_mean_X),
timeBodyAcc_mean_Y_mean =mean(timeBodyAcc_mean_Y),
timeBodyAcc_mean_Z_mean =mean(timeBodyAcc_mean_Z),
timeBodyAcc_std_X_mean =mean(timeBodyAcc_std_X),
timeBodyAcc_std_Y_mean =mean(timeBodyAcc_std_Y),
timeBodyAcc_std_Z_mean =mean(timeBodyAcc_std_Z),
timeGravityAcc_mean_X_mean =mean(timeGravityAcc_mean_X),
timeGravityAcc_mean_Y_mean =mean(timeGravityAcc_mean_Y),
timeGravityAcc_mean_Z_mean =mean(timeGravityAcc_mean_Z),
timeGravityAcc_std_X_mean =mean(timeGravityAcc_std_X),
timeGravityAcc_std_Y_mean =mean(timeGravityAcc_std_Y),
timeGravityAcc_std_Z_mean =mean(timeGravityAcc_std_Z),
timeBodyAccJerk_mean_X_mean =mean(timeBodyAccJerk_mean_X),
timeBodyAccJerk_mean_Y_mean =mean(timeBodyAccJerk_mean_Y),
timeBodyAccJerk_mean_Z_mean =mean(timeBodyAccJerk_mean_Z),
timeBodyAccJerk_std_X_mean =mean(timeBodyAccJerk_std_X),
timeBodyAccJerk_std_Y_mean =mean(timeBodyAccJerk_std_Y),
timeBodyAccJerk_std_Z_mean =mean(timeBodyAccJerk_std_Z),
timeBodyGyro_mean_X_mean =mean(timeBodyGyro_mean_X),
timeBodyGyro_mean_Y_mean =mean(timeBodyGyro_mean_Y),
timeBodyGyro_mean_Z_mean =mean(timeBodyGyro_mean_Z),
timeBodyGyro_std_X_mean =mean(timeBodyGyro_std_X),
timeBodyGyro_std_Y_mean =mean(timeBodyGyro_std_Y),
timeBodyGyro_std_Z_mean =mean(timeBodyGyro_std_Z),
timeBodyGyroJerk_mean_X_mean =mean(timeBodyGyroJerk_mean_X),
timeBodyGyroJerk_mean_Y_mean =mean(timeBodyGyroJerk_mean_Y),
timeBodyGyroJerk_mean_Z_mean =mean(timeBodyGyroJerk_mean_Z),
timeBodyGyroJerk_std_X_mean =mean(timeBodyGyroJerk_std_X),
timeBodyGyroJerk_std_Y_mean =mean(timeBodyGyroJerk_std_Y),
timeBodyGyroJerk_std_Z_mean =mean(timeBodyGyroJerk_std_Z),
timeBodyAccMag_mean_mean =mean(timeBodyAccMag_mean),
timeBodyAccMag_std_mean =mean(timeBodyAccMag_std),
timeGravityAccMag_mean_mean =mean(timeGravityAccMag_mean),
timeGravityAccMag_std_mean =mean(timeGravityAccMag_std),
timeBodyAccJerkMag_mean_mean =mean(timeBodyAccJerkMag_mean),
timeBodyAccJerkMag_std_mean =mean(timeBodyAccJerkMag_std),
timeBodyGyroMag_mean_mean =mean(timeBodyGyroMag_mean),
timeBodyGyroMag_std_mean =mean(timeBodyGyroMag_std),
timeBodyGyroJerkMag_mean_mean =mean(timeBodyGyroJerkMag_mean),
timeBodyGyroJerkMag_std_mean =mean(timeBodyGyroJerkMag_std),
freqBodyAcc_mean_X_mean =mean(freqBodyAcc_mean_X),
freqBodyAcc_mean_Y_mean =mean(freqBodyAcc_mean_Y),
freqBodyAcc_mean_Z_mean =mean(freqBodyAcc_mean_Z),
freqBodyAcc_std_X_mean =mean(freqBodyAcc_std_X),
freqBodyAcc_std_Y_mean =mean(freqBodyAcc_std_Y),
freqBodyAcc_std_Z_mean =mean(freqBodyAcc_std_Z),
freqBodyAcc_meanFreq_X_mean =mean(freqBodyAcc_meanFreq_X),
freqBodyAcc_meanFreq_Y_mean =mean(freqBodyAcc_meanFreq_Y),
freqBodyAcc_meanFreq_Z_mean =mean(freqBodyAcc_meanFreq_Z),
freqBodyAccJerk_mean_X_mean =mean(freqBodyAccJerk_mean_X),
freqBodyAccJerk_mean_Y_mean =mean(freqBodyAccJerk_mean_Y),
freqBodyAccJerk_mean_Z_mean =mean(freqBodyAccJerk_mean_Z),
freqBodyAccJerk_std_X_mean =mean(freqBodyAccJerk_std_X),
freqBodyAccJerk_std_Y_mean =mean(freqBodyAccJerk_std_Y),
freqBodyAccJerk_std_Z_mean =mean(freqBodyAccJerk_std_Z),
freqBodyAccJerk_meanFreq_X_mean =mean(freqBodyAccJerk_meanFreq_X),
freqBodyAccJerk_meanFreq_Y_mean =mean(freqBodyAccJerk_meanFreq_Y),
freqBodyAccJerk_meanFreq_Z_mean =mean(freqBodyAccJerk_meanFreq_Z),
freqBodyGyro_mean_X_mean =mean(freqBodyGyro_mean_X),
freqBodyGyro_mean_Y_mean =mean(freqBodyGyro_mean_Y),
freqBodyGyro_mean_Z_mean =mean(freqBodyGyro_mean_Z),
freqBodyGyro_std_X_mean =mean(freqBodyGyro_std_X),
freqBodyGyro_std_Y_mean =mean(freqBodyGyro_std_Y),
freqBodyGyro_std_Z_mean =mean(freqBodyGyro_std_Z),
freqBodyGyro_meanFreq_X_mean =mean(freqBodyGyro_meanFreq_X),
freqBodyGyro_meanFreq_Y_mean =mean(freqBodyGyro_meanFreq_Y),
freqBodyGyro_meanFreq_Z_mean =mean(freqBodyGyro_meanFreq_Z),
freqBodyAccMag_mean_mean =mean(freqBodyAccMag_mean),
freqBodyAccMag_std_mean =mean(freqBodyAccMag_std),
freqBodyAccMag_meanFreq_mean =mean(freqBodyAccMag_meanFreq),
freqBodyAccJerkMag_mean_mean =mean(freqBodyAccJerkMag_mean),
freqBodyAccJerkMag_std_mean =mean(freqBodyAccJerkMag_std),
freqBodyAccJerkMag_meanFreq_mean =mean(freqBodyAccJerkMag_meanFreq),
freqBodyGyroMag_mean_mean =mean(freqBodyGyroMag_mean),
freqBodyGyroMag_std_mean =mean(freqBodyGyroMag_std),
freqBodyGyroMag_meanFreq_mean =mean(freqBodyGyroMag_meanFreq),
freqBodyGyroJerkMag_mean_mean =mean(freqBodyGyroJerkMag_mean),
freqBodyGyroJerkMag_std_mean =mean(freqBodyGyroJerkMag_std),
freqBodyGyroJerkMag_meanFreq_mean =mean(freqBodyGyroJerkMag_meanFreq)
)
#    ds<-summarize(g2, var)
    return(ds)
    }

wdp<-"C:/FORMATIONS/MOOC/COURSERA/GETTING&CLEANING DATA/PROJECT/UCI HAR Dataset"

# NAME : MakeProject
# INPUT : filename 
#   The output file name 
#
# OUTPUT : A file containing the tidy Data
#
# COMMENTS :
#
MakeProject <- function(filename) {
    
    ds<-MergeDataset(wdp)

    cn<-GetColnameWithMeanAndStd(wdp,"features.txt")
    ds2<-ExtractMeanAndStd(ds,cn)

    an<-GetActivityName(wdp,"activity_labels.txt")
    ds3<-RenameActivity(ds2,an)

    ln<-GetLabelName(wdp)
    ds4<-LabelDataframe(ds3,ln,cn)

    tds<-MakeTidyDs(ds4,ln)
    write.table(tds,file=filename, row.name=FALSE)
}

