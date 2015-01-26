# GetAndCleanProject
Project for MOOC "Getting and cleaning Data" from Coursera 

The Project is divided in six functions.

MergeDataset: To merge the 2 main datasets
GetColnameWithMeanAndStd: Which browse the file features.txt to extract the variables with mean() and std()
ExtractMeanAndStd: Which extract the columns from the merged dataset
GetActivityName :  Which get activity labels from activity_labels.txt
RenameActivity : Which rename the label id with a  name
GetLabelName: Which get the label names from the file features.txt for the variables with mean() and std() and clean them
LabelDataframe: Which rename the labels in a dataframe
MakeTidyDs: Which make the statistics required. The required file is generated with that function  
MakeProject: The main program which glues the various functions to generate the file 

To generate the data in a file tds.txt simply type MakeProject("tds.txt") in R 

The file is commented in order to have technical information about the various functions.

Todo :
- Improve the speed of the MergeDataset function
- Improve MakeTidyDs. The variables are static. An automatic Generation of the summarize commands should be implemented.
 

