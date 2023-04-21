**Introduction**
Since I have always struggled with managing my sleep cycle during my adult years, I understand how important sleep is for a person to perform day to day activities and wanted to gain deeper insights. The purpose of working on a sleep efficiency dataset is to better understand and quantify the quality of sleep, and how various factors can impact it, which in general, humans tend to ignore. With sleep being a critical component of overall health and well-being, the information gained from studying a
sleep efficiency dataset can help in the development of evidence-based interventions and treatments for sleep disorders and improve overall sleep health.
Also, by analyzing the patterns and trends in sleep efficiency data, researchers can identify potential risk factors for sleep problems, such as sleep disorders, and develop targeted strategies to improve sleep health.

**About the dataset**
Source: kaggle.com

The dataset contains information about a group of test subjects and their sleep patterns. Each test subject is identified by a unique "Subject ID" and their age and gender are also recorded. The "Bedtime" and "Wakeup time" features indicate when each subject goes to bed
and wakes up each day, and the "Sleep duration" feature records the total amount of time each subject slept inhours. The "Sleep efficiency" feature is a measure of the proportion of time spent in bed that is actually spent asleep. The "REM sleep percentage", "Deep sleep percentage", and "Light sleep percentage" features indicate the amount of time each subject spent in each stage of sleep. The "Awakenings" feature records the number of times each subject wakes up during the night. Additionally, the dataset includes information about each subject's caffeine and alcohol consumption in the 24 hours prior to bedtime, their smoking status, and their exercise frequency.
The dataset has 452 observations across both genders and varying age. The total columns are 15. Alcohol consumption is measured in oz and caffeine consumption in mg.

**DATA CLEANING**
First, we changed the datatype of the Gender and Smoking status columns to a categorical datatype for a better analysis using pivot tables.
From the columns "Bedtime" and "Wakeup time", we extracted the time, leaving out the date as it was not a required information for the analysis.
We then updated the name of the columns, removing any spaces between the words. For example, the name of the column "Wakeup time" was changed to "Wakeup_time".
An important step was to check the missing values, which was done using the naniar package. Since we wanted to have all the data from the columns and the
percentage of data missing was low, we removed the rows with missing values.
Also, for the "Age" column, there were only 1 observation for age 9 and age 10. As a result, we removed the observation for age 9 and for
better analysis we create a new column "age_range", starting from 10-20 till 60-70.

**FINAL DATASET**
The final dataset has 13 columns and 3 categorical variables.

The findings of the dataset analysis has been coded on to a shiny R app
