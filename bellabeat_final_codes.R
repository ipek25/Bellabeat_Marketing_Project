Sys.setlocale("LC_TIME", "English")
library(tidyverse)
library(lubridate)
# Importing the dataset
setwd("~/R_documents/Fitabase Data 4.12.16-5.12.16")
dailyactivity <- read.csv("dailyActivity_merged.csv")
sleep <- read.csv("sleepDay_merged.csv")
hourlysteps <- read.csv("hourlySteps_merged.csv")
                #Data Cleaning
#Dates are registered as strings. Changing data type to date and time
dailyactivity$ActivityDate<-as.Date(dailyactivity$ActivityDate, "%m/%d/%Y")
sleep$SleepDay<-as.Date(sleep$SleepDay, "%m/%d/%Y")
hourlysteps$ActivityHour <- 
  parse_date_time(hourlysteps$ActivityHour, "%m/%d/%Y %H:%M:%S %p")
#Removing duplicates (only sleep data have 3 duplicates)
dailyactivity_new <- distinct(dailyactivity)
sleep_new <- distinct(sleep)
hourlysteps_new <- distinct(hourlysteps)
                #Data manipulation
#Separating date and time
hourlysteps_new <-
  separate(hourlysteps_new, ActivityHour, c('activity_date', 'activity_hour'), " ")
#After separation date and time columns become string. To convert that
hourlysteps_new$activity_date<-as.Date(hourlysteps_new$activity_date, "%Y-%m-%d")
#Finding corresponding weekdays for dates
dailyactivity_new <- 
  mutate(dailyactivity_new, days=weekdays(dailyactivity_new$ActivityDate))
#Joining daily activity data with sleep data
sleep_new <- rename(sleep_new, ActivityDate=SleepDay)
fullJoinDf <- full_join(dailyactivity_new,sleep_new,by=c("Id", "ActivityDate"))
                #Analyse
        #Daily activity by intensity (in percentages)
#Finding people' who did not wear their devices's Total Distance=0
no_activity <- dailyactivity_new %>% 
  filter(TotalDistance == 0) 
count(no_activity)
#Finding out their frequency of not wearing these devices
no_activity %>% 
  group_by(Id) %>%  
  tally() %>%  
  arrange(-n)
#Deleting data where Total Distance =0
dailyactivity_new <- dailyactivity_new %>%
  filter(TotalDistance != 0)
#Evaluating respondent's daily performance
dailyactivity_new <- mutate(dailyactivity_new, totalminutes=SedentaryMinutes+LightlyActiveMinutes+
                              FairlyActiveMinutes+VeryActiveMinutes)
#Finding percentages
minutes <- data.frame(
  id = dailyactivity_new$Id,
  date= dailyactivity_new$ActivityDate,
  sedentary= dailyactivity_new$SedentaryMinutes/dailyactivity_new$totalminutes*100,
  light= dailyactivity_new$LightlyActiveMinutes/dailyactivity_new$totalminutes*100,
  fair= dailyactivity_new$FairlyActiveMinutes/dailyactivity_new$totalminutes*100,
  very= dailyactivity_new$VeryActiveMinutes/dailyactivity_new$totalminutes*100) 
#Finding average percentages
df2 <- data.frame(Percentage=c(mean(minutes$sedentary),
                               mean(minutes$light),
                               mean(minutes$fair),
                               mean(minutes$very)), 
                  Activity_Intensity = c("Sedentary",
                                         "Lightly Active",
                                         "Fairly Active",
                                         "Very Active"))
ggplot(data=df2) +
  geom_bar(mapping = aes(x=Percentage, y=Activity_Intensity, 
                         fill=Activity_Intensity), stat = "identity", width=0.5)+
  labs( y="Activity Intensity", 
          title = "Respondents' Daily Activity based on the Intensity")
      
            #Daily activity by Hour (in Total Steps)
#deleting 0 steps
no_activity_hourly <- hourlysteps_new %>% 
  select("Id", "activity_date", "StepTotal") %>% 
  group_by(Id, activity_date) %>% 
  summarise(sum_steps=sum(StepTotal)) %>% 
  filter(sum_steps==0)
no_activity_hourly %>% 
  count(Id)
hourlysteps_new <- merge(hourlysteps_new,no_activity_hourly,by=c("Id", "activity_date"),
                all=T) #merging two tables
hourlysteps_new[is.na(hourlysteps_new)] <- 0.1 #identifying the ones with more than 0 steps
hourlysteps_new <- filter(hourlysteps_new, sum_steps==0.1)

#Graph  

hourlysteps_new %>% 
  group_by(activity_hour) %>% 
  summarise(averagesteps=mean(StepTotal)) %>% 
  ggplot() +
  geom_bar(mapping = aes(x=activity_hour,y=averagesteps),stat='identity',
           fill="steelblue2") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  labs(x="Activity Hour", y="Number of Steps", 
       title = "Respondents Average Steps in a Day by Hours")

      #Average Total Steps by days of week
dailyactivity_new %>% 
  arrange(days) %>% 
  group_by(days) %>% 
  summarise(mean_steps=mean(TotalSteps)) %>% 
  ggplot() +
  geom_col(mapping = aes(x= reorder(days, +mean_steps), y=mean_steps), 
           fill="tomato") +
  labs(x= "Days", y="Average Steps", title="Respondents Average Steps in a Week")
      #Relationship between times in bed without sleep and calories
fullJoinDf %>% 
  drop_na() %>% 
  mutate(nosleep=TotalTimeInBed - TotalMinutesAsleep) %>% 
  ggplot(aes(x=Calories, y=nosleep)) +
  geom_point() +
  geom_smooth(color = "purple")
#Getting rid of outliers
fullJoinDf %>% 
  drop_na() %>% 
  mutate(nosleep=TotalTimeInBed - TotalMinutesAsleep) %>% 
  filter(nosleep<250) %>% 
  filter(Calories>1000) %>% 
  ggplot(aes(x=Calories, y=nosleep)) +
  geom_point() +
  geom_smooth(color = "purple")
