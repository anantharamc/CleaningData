
Data Cleaning Project
==============================================================================
The original data set was taken from http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones . Using instructions from Coursera Project, the dataset has been modified.

### The project submission includes the following files:

- 'README.MD'
- 'ProjectCodeBook.pdf'
- 'run_analysis.R'

The output file MyTidyDataset.txt has been uploaded to Coursera Project submission site.

### How to run the program?

1. Copy run_analysis.R to working directory. Optionally copy Readme.MD and ProjectCodebook.MD to working directory.
2. Create a folder called "UCI" in the working directory
3. Unzip and Copy the input dataset provided by Coursera project instructions

The resulting file structure will be as shown below.

Working directory

--run_analysis.R   

--README.MD

--ProjectCodeBook.pdf

--UCI
  
-------test folder and its content

-------train folder and its contents

-------activity_labels.txt

-------features.txt

-------features_info.txt

-------README.txt

		
### Description of modifications

Step 1:
* Read Training dataset and test dataset and cobine them with rbind

Step 2:
* Read features and remove characters such as (, ), hyphen, comma, and other duplicate data. Note: removed paranthesis with \\(\\), Using gsub to replace strings
* Select only features with mean or std and drop all other columns. Used of grep to accomplish finding columns

Step 3:
* Read Subject training set and test set, cbind the subjects to the main dataset
* Read activity training set and test set, combine them to one data frame and add it to the main dataset
* Read descriptive activity labels and merge it based on V1 column
* After joining column V1 becomes duplicate so drop it

Step 4:
* Change column names to be more readable

Step 5:
* Summarize data by subject and activity, used ddply to make a tidy dataset


