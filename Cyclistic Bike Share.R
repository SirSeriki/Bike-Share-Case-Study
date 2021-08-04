## Installing all necessary packages

install.packages("plyr")
install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("ggplot2")

## Installing the libraries 

library(tidyverse)
library(lubridate)
library(ggplot)
library(plyr)
library(dplyr)
library(here)
library(skimr)
library(janitor)
library(readr)
library(tidyr)



## Collect data
q4_2018 <- read.csv("Divvy_Trips_2018_Q4.csv")
q1_2019 <- read.csv("Divvy_Trips_2019_Q1.csv")
q3_2019 <- read.csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read.csv("Divvy_Trips_2019_Q4.csv")

#Wrangling 

## Checking columns names for consistency before combining. 
colnames(q4_2018)
colnames(q1_2019)
colnames(q3_2019)
colnames(q4_2019)

## Checking the data for structure
str(q4_2018)
str(q1_2019)
str(q3_2019)
str(q4_2019)

## Using the mutate() function to change the datatype of "bikeid" to character. This will make the aggregation work correctly.

q4_2018 <-  mutate(q4_2018, bikeid = as.character(bikeid))
q1_2019 <-  mutate(q3_2019, bikeid = as.character(bikeid))
q3_2019 <-  mutate(q3_2019, bikeid = as.character(bikeid)) 
q4_2019 <-  mutate(q4_2019, bikeid = as.character(bikeid))

## Combining all the individual quarters into one data frame

all_trips <- bind_rows(q4_2018, q1_2019, q3_2019, q4_2019)

## Removing birthyear and gender fields as they wouldn't be necessary for this analysis
all_trips <- all_trips %>%  
  select(-c( birthyear, gender))

## Inspecting the combined dataframe 

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Retrieve the dimension of the data frame.
head(all_trips,3)  #See the first 6 rows of data frame.  Also tail(qs_raw)
str(all_trips)  #See list of columns and data types.
summary(all_trips)  #Statistical summary of data.

## Number of issues spotted:In the "usertype" column, the names for members ("subscriber") and casual riders ("Customer") are not consistent.

## We need to create columns for the day, day of the week, and month (among other times) that give us more ways to group the data.

## We do not know how long each of the rides are. We will create a "trip_duration" column that finds the difference between the start and end times of the ride by subtracting the "end_time" column from the "start_time"

table(all_trips$usertype) #Creating a table of the 'usertype'

## Changing the 'Subscriber' to 'member' and 'Customer' to 'casual' for consistency and aggregation. 

all_trips <-  all_trips %>% 
  mutate(usertype = recode(usertype,"Subscriber" = "member"
                                   ,"Customer" = "casual"))
table(all_trips$usertype)


## Creating columns for the day, day of the week, and month that make it easier to group the data

all_trips$date <- as.Date(all_trips$start_time) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")
all_trips$ride_length <- difftime(all_trips$end_time,all_trips$start_time)

## In order to make sure that "ride_length" can be analyzed, We will check to make sure the column is numeric. If the variable isn't numeric, We will coerce the variable to numeric.

str(all_trips)
if(!is.numeric(all_trips$ride_length))
  all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
## The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative or ride_length bigger than 7 days ( all_trips$ride_length> 7*24*60  )
## We will create a new version of the datadframe (v2) since data is being removed
## Following method has many NA rows. NA rows and if 'from_station_name is NA that row is exclude


removed_data <-  all_trips %>% 
  filter( all_trips$from_station_name == "HQ QR" | all_trips$ride_length < 0)

all_trips_v2 <- all_trips %>% 
  filter(( all_trips$from_station_name != "HQ QR" | is.na(all_trips$from_station_name)) & all_trips$ride_length >= 0)

#Analysis
## Conducting descriptive analysis to find the dataframe's summary statistics

summary(all_trips_v2$ride_length)

## Next, we will compare members and casual users. Notice that on the average the metrics for causal riders outweighs those of member riders. 

  aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = mean)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = median)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = max)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype, FUN = min)
  aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)

 ## Ordering the the day of the week 
  
  all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels= c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
  
## Rerun the previous aggregate() call. Days of week are now ordered properly. 
  
 aggregate(all_trips_v2$ride_length ~ all_trips_v2$usertype + all_trips_v2$day_of_week, FUN = mean)
 
 #Visualization
 
 ## Rider Category
 
 ggplot(all_trips_v2,
        aes(x = usertype)) +
   geom_bar(fill = "cornflowerblue",
            color = "black") +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Rider category", title  = "Total Ride")
 
 ## Rider category based on dates
 
 ggplot(all_trips_v2,
        aes(x = day, 
            fill = usertype)) + 
   geom_bar(position = position_dodge(preserve = "single")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Date",y = "Ride Count", title  = "Rider category based on dates.")
 
## Dates don't have any impact on the riding pattern
 
# Proportion of rider category based on dates
 
 ggplot(all_trips_v2,
        aes(x = day, 
            fill = usertype)) + 
   geom_bar(position = "fill") +
   scale_y_continuous(labels = scales::percent) +
   labs(x = "Date",y = "Proportion", title  = "Rider category based on dates.")
 
## Daily rider analysis
 
 ggplot(all_trips_v2,
        aes(x = day_of_week, 
            fill = usertype)) + 
   geom_bar(position = "fill") +
   scale_y_continuous(labels = scales::percent) +
   labs(x = "Day",y = "Percentage", title  = "Daily Rider analysis")
 
 ## It seems more member riders ride on Tuesday and Wednesdays. While causal riders ride more on weekends.
 
 
## Monthly Rider analysis
 
 ggplot(all_trips_v2,
        aes(x = month,
            fill = usertype)) +
   geom_bar(position = position_dodge(preserve = "single")) +
   scale_y_continuous(labels = scales::comma) +
   labs(x = "Month",y = "Count", title  = "Monthly Rider analysis")
 
 ## Member riders contribute more to the overall monthly rides for at least half the year
 
## Weekdays and weekends Data Analysis
 weekday_data <- subset(all_trips_v2,day_of_week == "Monday" | day_of_week == "Tuesday" |day_of_week == "Wednesday" | day_of_week == "Thursday"| day_of_week == "Friday")
 weekend_data <- subset(all_trips_v2, day_of_week == "Saturday" |day_of_week == "Sunday")

 ## Weekday Data Analysis
 weekday_casual <- weekday_data %>%
   group_by(usertype) %>%
   dplyr::summarise(n = n()) %>%
   mutate(pct = n/sum(n), lbl = scales::percent(pct))
 
 weekday_casual
 
 ## Member riders constitute 81% of the weekday rides

## Weekend data Analysis
 
 weekend_casual <- weekend_data %>%
   group_by(usertype) %>%
   dplyr::summarise(n = n()) %>%
   mutate(pct = n/sum(n), lbl = scales::percent(pct))
 
 weekend_casual

 ## Member riders constitute 57% of the weekend riders and casual riders 43%, a significant increase in casual riders during the weekend compared to weekday.
 
## Top station preference for Members and Casual riders
 
 top_stn <- as.data.frame(sort(table(all_trips_v2$from_station_name), decreasing=TRUE) [2:11])
 
 ggplot(top_stn,
        aes(x = Var1, 
            y = Freq)) +
   geom_bar(stat="identity") +
   coord_flip() +
   labs(x = "Station",y = "Count", title  = "Top station as starting point")
 
 ##The top preferred station for casual and member riders is Canal st & Adams St
 
## Casual vs member riders top station preference 
 
 member_data <- all_trips_v2 %>%
   filter(all_trips_v2$usertype == "member")
 casual_data <- all_trips_v2 %>%
   filter(all_trips_v2$usertype == "casual")
 
 # As station name consist of blank fields i will delete that column
 member_top_stn <- as.data.frame(sort(table(member_data$from_station_name), decreasing=TRUE) [2:11])
 casual_top_stn <- as.data.frame(sort(table(casual_data$from_station_name), decreasing=TRUE) [2:11])

a <- ggplot(member_top_stn, 
            aes(x = Freq, y = Var1)) +
   geom_bar(stat="identity")
 
 b <- ggplot(member_top_stn, 
            aes(x = Freq, y = Var1)) +
   geom_bar(stat="identity") 
 
plots <- list(a,b)

layout <- rbind(c(1,2),c(2,2))

grid.arrange(grobs=plots,layout_matrix = layout,labels = c("Member", "Casual"), ncol = 2, nrow = 1)
 
 ##Inconclusive, need to fix code 

## Average ride time
plotdata <- all_trips_v2 %>%
   group_by(usertype) %>%
   dplyr::summarise(n = n(),
                    mean = mean(ride_length),
                    sd = sd(ride_length),
                    se = sd / sqrt(n))

ggplot(plotdata,
       aes(x=usertype,
           y = mean / 60)) +
   geom_bar(stat="identity",
            fill = "cornflowerblue",
            color = "black") +
   labs(x = "Rider Type", y = "Time (min)", title  = "Mean ride time")

 ## Casual riders have a longer ride time than member riders on the average 

## Average ride time per month

ggplot(all_trips_v2) +
   geom_col(mapping=aes(x=month, y=ride_length, fill=usertype)) +
   scale_y_continuous(labels = scales::comma)

 ## Casual riders ride for longer lengths monthly on the average 

## Percentage of Ride by Rider type 

plotdata <- all_trips_v2 %>%
   group_by(day_of_week, usertype) %>%
   dplyr::summarise(n = n()) %>%
   mutate(pct = n/sum(n), lbl = scales::percent(pct))

ggplot(plotdata, 
       aes(x = factor(day_of_week,
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday")),
           y = pct,
           fill = usertype)) + 
   geom_bar(stat = "identity",
            position = "fill") +
   geom_text(aes(label = lbl), 
             size = 3, 
             position = position_stack(vjust = 0.5)) +
   scale_fill_brewer(palette = "Set2") +
   labs(y = "Percent", 
        fill = "Rider Type",
        x = "Day",
        title = "Percentage of Ride by Rider type") +
   theme_minimal()

## Proportion of Trip duration based on Rider type
plotdata <- all_trips_v2 %>%
   group_by(day_of_week, usertype) %>%
   dplyr::summarise(trip_time = sum(ride_length)) %>%
   mutate(pct = as.numeric(trip_time)/sum(as.numeric(trip_time)), lbl = scales::percent(pct))

ggplot(plotdata, 
       aes(x = factor(day_of_week,
                      levels = c("Sunday", "Monday", "Tuesday", "Wednesday",
                                 "Thursday", "Friday", "Saturday")),
           y = pct,
           fill = usertype)) + 
   geom_bar(stat = "identity",
            position = "fill") +
   scale_y_continuous(breaks = seq(0, 1, .2),
   ) +
   geom_text(aes(label = lbl), 
             size = 3, 
             position = position_stack(vjust = 0.5)) +
   scale_fill_brewer(palette = "Set1") +
   labs(y = "Percent", 
        fill = "Rider type",
        x = "Day",
        title = "Proportion of Trip duration based on Rider type") +
   theme_minimal()
 ## The last 2 visualizations denote that although Member riders are higher the proportion of the trip duration seems to be pretty high for casual members
 
# Key Takeways

## Dates don't have any impact on the riding pattern
## It seems more member riders ride on Tuesday and Wednesdays. While causal riders ride more on weekends.
## The months of July, August and September record the highest number of rides, with August seeing the higest rides.
## Member riders contribute more to the overall monthly rides for at least half the year
## Member riders constitute 81% of the weekday rides, while casual riders constitute a total of 43% of the weekend rides, a significant increase in casual riders during the weekend compared to weekday
## The top preferred station for casual and member riders is Canal st & Adams St
##  Casual riders have a longer ride time than member riders on the average even though the proportion of member riders is higher. 
 
# Recommendations

## As per analysis July, August and September record the highest number of rides, with August seeing the highest rides. We will recommend that stakeholders run campaigns starting from July until September. Starting with a giveaway promotion at Canal st 7 Adams st station on a Friday, as this is most preffered station by casual riders and one of the days with the highest number of Casual riders.

## Also, based on our analysis Casual riders constitute the longest duration of rides, digital billboards should be installed along riding routes between stations showing the benefits available to member riders.


#Exporting the "all_trips_v2" dataset to an external folder for visualization in Tableau
write.table(all_trips_v2,file = "C:/Users/Work Station/Desktop/Google Data Analytics Professional/Portfolio Project/Dataset/CSV/all_trips_v4.csv", row.names=F, sep = ",")



