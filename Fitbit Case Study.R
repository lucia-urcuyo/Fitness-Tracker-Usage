install.packages('tidyverse')
library(tidyverse)

## Loading your CSV files
daily_activity <- read.csv("dailyActivity_merged.csv")
sleep_day <- read.csv("sleepDay_merged.csv")

## Take a look at the data in both tables
head(daily_activity)
head(sleep_day)

## Identify all of the columns in my tables
colnames(daily_activity)
colnames(sleep_day)

## How many unique participants are there in each dataframe? 
n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)

## How many observations are there in each dataframe?
nrow(daily_activity)
nrow(sleep_day)

## Take a look at summary statistics to some variables in each data frame

daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

sleep_day %>%  
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()


 ## Merging my two datasets together
combined_data <- merge(sleep_day, daily_activity, by="Id")

## How many participants are in this data set.
n_distinct(combined_data$Id)

## Add a column of total active hours, create columns with data in hours rather than mintues, 
## and remove the columns that I wont need for this analysis
combined_data2 <- mutate (
  combined_data, 
  TotalActiveMinutes = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes, 
  TotalHoursAsleep = TotalMinutesAsleep/60, 
  TotalActiveHours = TotalActiveMinutes/60,
  SleepDay = NULL,
  TotalSleepRecords = NULL,
  TotalTimeInBen = NULL,
  TotalDistance = NULL,
  LoggedActivitiesDistance = NULL,
  SedentaryActiveDistance = NULL,
)

## Plot the relationship between time asleep and active time
ggplot(data=combined_data, aes(x=TotalMinutesAsleep, y=VeryActiveMinutes)) + geom_point()

ggplot(data=combined_data2, aes(x=TotalHoursAsleep, y=TotalActiveHours))  + stat_binhex()

## Plot the relationship between calories burned and active hours
ggplot(data=combined_data2, aes(x=Calories, y=TotalActiveHours,)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, aes(col="salmon1"))

## Plot the relationship between active minutes and sedentary minutes
ggplot(data=combined_data2, aes(x=TotalActiveMinutes, y=SedentaryMinutes)) + geom_point()

## bin sleep, active hours, and calories
combined_data2 <- subset(combined_data2, TotalHoursAsleep<13)
binned_sleep <- cut(combined_data2$TotalHoursAsleep, c(0,1,2,3,4,5,6,7,8,9,10,11,12,13))

binned_active_hours <- cut(combined_data2$TotalActiveHours, c(0,1,2,3,4,5,6,7,8))

binned_calories <- cut(combined_data2$Calories, c(1000,1500,2000,2500,3000,3500,4000,4500,5000))

## create a new data frame with mutated data with binned sleep combined
combined_data3 <- mutate(combined_data2, binned_sleep, binned_active_hours)

## Create a boxplot comparing total active hours with hours asleep
boxplot(TotalActiveHours~binned_sleep, outline=FALSE,
        data=combined_data3,
        main="Sleep vs Active Hours",
        xlab="Total Sleep",
        ylab="Active Hours",
        col="light pink",
        border="grey"
)

## Remove data that contain less than 1,500 steps a dat, because it seems like the person removed 
## the fitness tracker and it causes data to be unreliable
combined_data3 <- subset(combined_data3, TotalSteps>1500)

## Create a boxplot comparing total steps with hours asleep
boxplot(TotalSteps~binned_sleep, outline=FALSE,
        data=combined_data3,
        main="Sleep vs Total Steps",
        xlab="Total Sleep",
        ylab="Total Steps",
        col="light pink",
        border="grey"
)

## Create a boxplot comparing total calories burned with hours asleep
boxplot(Calories~binned_sleep, outline=FALSE,
        data=combined_data3,
        main="Sleep vs Calories",
        xlab="Total Sleep",
        ylab="Calories Burned",
        col="light pink",
        border="grey"
)

## create a new data frame adding 4 new columns: total hours awake, active hours per hour 
## awake, calories burned per hor awake, and steps taken per hour awake
combined_data4 <- mutate(combined_data3, HoursAwake = 24-TotalHoursAsleep, ActiveHoursPerHourAwake = TotalActiveHours/HoursAwake, CaloriesPerHourAwake = Calories/HoursAwake, StepsPerHourAwake = TotalSteps/HoursAwake)

## Create a boxplot comparing total active hours per hour awake with hours asleep
boxplot(ActiveHoursPerHourAwake~binned_sleep, outline=FALSE,
        data=combined_data4,
        main="Sleep vs Average Active Hours",
        xlab="Total Sleep",
        ylab="Active Hours Per Hour Awake",
        col="salmon1",
        border="grey"
)

## Create a boxplot comparing steps taken per hour with hours asleep
boxplot(StepsPerHourAwake~binned_sleep, outline=FALSE,
        data=combined_data4,
        main="Sleep vs Average Steps",
        xlab="Total Sleep",
        ylab="Steps Per Hour Awake",
        col="light pink",
        border="grey"
)

## Create a boxplot comparing the calories burned per hour with hours asleep
boxplot(CaloriesPerHourAwake~binned_sleep, outline=FALSE,
        data=combined_data4,
        main="Sleep vs Average Calories",
        xlab="Total Sleep",
        ylab="Calories Burned Per Hour Awake",
        col="salmon1",
        border="grey"
)

## Create a boxplot comparing total steps and calories burned
boxplot(TotalSteps~binned_calories, outline=FALSE,
        data=combined_data4,
        main="Steps vs Calories",
        xlab="Total Calories Burned",
        ylab="Total Steps in a Day",
        col="light pink",
        border="grey"
)

## Create a boxplot comparing total active hours and calories burned
boxplot(TotalActiveHours~binned_calories, outline=FALSE,
        data=combined_data4,
        main="Active Hours vs Calories",
        xlab="Total Calories Burned",
        ylab="Total Hours Active in a Day",
        col="light pink",
        border="grey"
)

## Create a boxplot comparing total steps and total active hours
boxplot(TotalSteps~binned_active_hours, outline=FALSE,
        data=combined_data4,
        main="Total Steps vs Active Hours",
        xlab="Total Hours Active",
        ylab="Total Steps in a Day",
        col="light pink",
        border="grey"
)

## Create a graph comparing total steps and total calories burned
ggplot(data=combined_data4, aes(x=Calories, y=TotalSteps)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, aes(col="salmon1"))

## Create a histogram to show the frequency of how many hours people sleep at night
hist(combined_data4$TotalHoursAsleep,
     main="Total Hours of Sleep", 
     xlab="Hours of Sleep", 
     border="grey", 
     col="pink")

## Create a histogram to show the frequency of how many steps people take a day
hist(combined_data4$TotalSteps,
     col="pink",
     border="grey",
     main="Total Steps", 
     xlab="Number of Steps")

## Create a histogram to show the frequency of how many calories people burn a day
hist(combined_data4$Calories,
     main="Total Calories Burned", 
     xlab="Calories Burned", 
     border="grey", 
     col="pink")

## Calculate mean for calories, steps, sleep, and active time
CalMean <- mean(combined_data4$Calories)
StepsMean <- mean(combined_data4$TotalSteps)
SleepMean <- mean(combined_data4$TotalHoursAsleep)
ActiveTimeMean <- mean(combined_data4$TotalActiveHours)

## Create data frames to compare fitbit users with average American's daily habits
Factor <- c("Calories", "Steps", "Sleep", "Active Time")
AverageFitbitUsers <- c(CalMean, StepsMean, SleepMean, ActiveTimeMean)
AverageAmerica <- c("1910", "3500", "6.8", "0.3167")
avg_comparison <- data.frame(Factor, AverageFitbitUsers, AverageAmerica)

## Create separate data frames to compare fitbit users with average American's daily habits
avg_cal <- data.frame(FitbitUsers= CalMean, Americans = "1910")
avg_steps <- data.frame(FitbitUsers= StepsMean, Americans = "3500")
avg_sleep <- data.frame(FitbitUsers= SleepMean, Americans = "6.8")
avg_active_time<- data.frame(FitbitUsers= ActiveTimeMean, Americans = "0.3167")

## Convert data frames to matrix
m_avg_cal <- as.matrix(avg_cal)
m_avg_steps <- as.matrix(avg_steps)
m_avg_sleep <- as.matrix(avg_sleep)
m_avg_active_time <- as.matrix(avg_active_time)


## Create a barplot comparing calories burned daily between fitbit users and average Americans
barplot(m_avg_cal,
        main = "Calories Comparison",
        ylab = "Calories",
        names.arg = c("Fitbit Users", "Americans"),
        col = c("pink","grey"))

## Create a barplot comparing steps taken daily between fitbit users and average Americans
barplot(m_avg_steps,
        main = "Steps Comparison",
        ylab = "Steps",
        names.arg = c("Fitbit Users", "Americans"),
        col = c("pink","grey"))

## Create a barplot comparing sleep duration between fitbit users and average Americans
barplot(m_avg_sleep,
        main = "Sleep Comparison",
        ylab = "Hours of Sleep",
        names.arg = c("Fitbit Users", "Americans"),
        col = c("pink","grey"))

## Create a barplot comparing  active hours daily between fitbit users and average Americans
barplot(m_avg_active_time,
        main = "Active Hours Comparison",
        ylab = "Hours Active",
        names.arg = c("Fitbit Users", "Americans"),
        col = c("pink","grey"))

## Create a linear regression between calories and total hours asleep,
## total steps, and total active time.
linreg = lm(Calories~TotalHoursAsleep+TotalActiveHours+TotalSteps,data=combined_data4)
summary(linreg)

rm(list=ls()) 
