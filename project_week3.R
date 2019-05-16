library(tidyverse)
library(dplyr)

# Get column names
col_names_wanted = read.table('./UCI HAR Dataset/features.txt')
col_names_wanted = col_names_wanted$V2

# Load data

# Task 1
# Train
x_train = read_table('./UCI HAR Dataset/train/X_train.txt',col_names = as.character(col_names_wanted) )
y_train = read_table('./UCI HAR Dataset/train/y_train.txt',col_names = c("Labels"))
subject_train = read_table('./UCI HAR Dataset/train/subject_train.txt',col_names = c("subject_index"))
# Merge train set together with hstack
combined_train = cbind(subject_train, x_train, y_train)

# Test
x_test = read_table('./UCI HAR Dataset/test/X_test.txt',col_names = as.character(col_names_wanted) )
y_test = read_table('./UCI HAR Dataset/test/y_test.txt',col_names = c("Labels"))
subjects_test = read_table('./UCI HAR Dataset/test/subject_test.txt',col_names = c("subject_index"))
# Merge Test together with hstack
combined_test = cbind(subjects_test, x_test, y_test)

# Merge train and test set
whole_dataset = rbind(combined_train,combined_test)

# Task 2 Extract mean and std of each column
# train mean and std
only_features = select(whole_dataset, -c('subject_index','Labels'))
feature_means = apply(only_features,2,mean)
feature_stds = apply(only_features,2,sd)

# Task 3 Assign Descriptive activity names
label_converter  = function(x)
{
    if (x == 1)
    {
        out = 'WALKING'
    }
    else if (x == 2)
    {
        out = 'WALKING_UPSTAIRS'
    }
    else if (x == 3)
    {
        out = 'WALKING_DOWNSTAIRS'
    }
    else if (x == 4)
    {
        out = 'SITTING'
    }
    else if (x == 5)
    {
        out = 'STANDING'
    }
    else if (x == 6)
    {
        out = 'LAYING'
    }
    
    out
}

whole_dataset_goodnames = whole_dataset %>% rowwise() %>%
        mutate( descriptive_label = label_converter(Labels))

select(whole_dataset_goodnames,descriptive_label)  # Just to check

# Task 5
# Groupby label names and find average of each variable for each subject 
# Infact there are only 30 subjects, so we should groupby label names and activity
df_no_original_labels = select(whole_dataset_goodnames,-c('Labels'))

wanted = df_no_original_labels %>% group_by(descriptive_label,subject_index) %>%
    summarise_all(funs(mean=mean(.,na.rm=TRUE)))

wanted
