require(pryr)
require(ISLR)
require(boot)
library(data.table)
library(plyr)
Student <- read.table("Assignment 6 Dataset.txt", header = TRUE, sep = ",")

StudentAverage <- ddply(Student, "Sex", transform, Grade.Average = mean(Grade))
# Write a csv named Students_Gendered_Mean
write.table(StudentAverage, "Students_Gendered_Mean", sep = ",")

# Filter the original data set to include only data for which the student name contained the letter i.
i_students <- subset(Student, grepl("i", Student$Name, ignore.case = T))
# Write a csv named Student_Names_With_i
write.table(i_students, "Student_Names_With_i", sep = ",")

