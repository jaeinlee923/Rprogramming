VisitFrequency <- c(0.6, 0.3, 0.4, 0.4, 0.2, 0.6, 0.3, 0.4, 0.9, 0.2)
BP <- c(103, 87, 32, 42, 59, 109, 78, 205, 135, 176) 
First <- c(1, 1, 1, 1, 0, 0, 0, 0, NA, 1)
Second <- c(0, 0, 1, 1, 0, 0, 1, 1, 1, 1)
FinalDecision <- c(0, 1, 0, 1, 0, 1, 0, 1, 1, 1)

df <- data.frame(VisitFrequency, BP, First, Second, FinalDecision)
df
# Check for Missing Values
NaList <- colnames(df)[apply(df, 2, anyNA)]
NaList
which(is.na(df$First))
# First method: omit NA by removing the row containing NA
library(dplyr)
df_NA_omit <-na.omit(df)
# Check for NA values in df_NA_omit
which(is.na(df_NA_omit ))

df_NA_omit

# Second method: impute data using  impute()

library(Hmisc)
df_imputed <- df
df_imputed$First <- impute(df_imputed$First, fun = median)
df_imputed
# Third method: using MICE

library(mice)
df_mice <- mice(df, m = 5, method = "pmm")

df_mice_finished <- complete(df_mice, 1)
df_mice_finished

# Get the mean() of FinalDecision and BP
mean(df_mice_finished$FinalDecision)

mean(df_mice_finished$BP)


library(ggplot2)

ggplot(df_mice_finished, aes(x = VisitFrequency, fill = factor(FinalDecision))) + geom_histogram()

ggplot(df_mice_finished, aes(x = BP, fill = factor(FinalDecision))) + geom_histogram()

ggplot(df_mice_finished, aes(x = VisitFrequency, y = BP, fill = factor(FinalDecision))) + geom_boxplot() + labs(x = "VisitFrequency", y = "Blood Pressure Levels") 
