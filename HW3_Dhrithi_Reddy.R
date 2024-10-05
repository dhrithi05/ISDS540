#1) installing and loading nycflight13 package
install.packages("nycflights13")
install.packages("dplyr")
library(dplyr)
library(nycflights13)

View(flights)

#to see more details on the flights dataframes
?flights

#2.a)How many flights out of NYC airports in 2013 had an arrival delay of three or more hours?
delayed_arr_NYC = flights %>%
  filter(arr_delay>=180) 

delayed_arr_NYC   
#using count() to print the number of flights     
print(count(delayed_arr_NYC))

#3897 flights out of NYC airports in 2013 had an arrival delay of three or more hours.

#2.b) How many flights out of NYC airports in 2013 departed in spring semester (i.e. the months January, February, March, April, and May
flight_dep_spring = flights%>%
  filter(month<6) %>%

flight_dep_spring

#printing using count() to get the number of flights in variable flight_dep_spring
print(count(flight_dep_spring))

#137915 flights out of NYC airports in 2013 departed in spring semester (i.e. the months January, February, March, April, and May)? Hint: use filter(). Type your R code and answer below.


#2.c) How many flights out of NYC airports in 2013 were operated by United or American airlines? 
UA_AA_flights = flights %>%
  filter(carrier == "UA"| carrier == "AA")
UA_AA_flights

#getting number of flights using count()
print(count(UA_AA_flights))

#91394 flights were out of NYC airports in 2013 were operated by United or American airlines



#2.d)List the top 5 airlines (by name, not carrier code) that had the highest delay time of any one flight leaving a NYC airport in 2013.
#using airlines data from the nycflights13 data frame
View(airlines)
#airlines contains airline names and their carrier codes
#using join() to add airlines data and flights data so that it contains both details
flights_and_airlines= flights %>%
  left_join(airlines, by = "carrier") 
  
#grouping by 'carrier' and arranging in descending order of departure delay time
highest_delays = flights_and_airlines %>%
  group_by(carrier) %>%
  arrange(desc(dep_delay)) %>%
  select(carrier,name,dep_delay)

#Printing the top 5 airlines that had highest delay time
print(highest_delays[1:5,])

#2.e)How many flights out of NYC airports in 2013 flew to the 3 major DC-area airports: Ronald Reagan Washington National Airport, Washington Dulles International Airport, or Baltimore/Washington International Thurgood Marshall Airport? Hint: use filter()and note that the corresponding dest codes for these three airports are “DCA”, “IAD”, and “BWI”, respectively.  Type your R code and answer below.
dca_iad_bwi_flights_fall <- flights %>% 
  filter(dest == "DCA" | dest == "IAD"|dest == "BWI") 
View(btv_sea_flights_fall)

#Number of flights out using count()
print(count(dca_iad_bwi_flights_fall))  

# 17186 flights flew out of  NYC airports in 2013 flew to the 3 major DC-area airports


#2.f)Report some summary statistics of the arr_delay variable in the flights data frame by considering multiple summary functions at once in the same summarize() code

arr_delay_summary <- flights %>%
  summarize(
    min = min(arr_delay, na.rm = TRUE),
    q1 = quantile(arr_delay, 0.25, na.rm = TRUE),
    median = quantile(arr_delay, 0.5, na.rm = TRUE),
    q3 = quantile(arr_delay, 0.75, na.rm = TRUE),
    max = max(arr_delay, na.rm = TRUE),
    mean = mean(arr_delay, na.rm = TRUE),
    sd = sd(arr_delay, na.rm = TRUE),
    missing = sum(is.na(arr_delay))
  )

#summary statistics for arr_delay variable:
print(arr_delay_summary)

#2.g)Compute the mean arrival delay for each carrier in each month using the carrier, month and arr_delay variables in the flights data frame
#in other words, we would like to compute the mean arrival delay for each carrier split by month. 
#Save the result in a new data frame called summary_monthly_dep_delay

summary_monthly_dep_delay <- flights %>%
     group_by(carrier, month) %>%
     summarize( mean(arr_delay,na.rm=TRUE) )

View(summary_monthly_dep_delay)

#2.h)Compute a new variable speed (in miles per hour) using the air_time and distance variables in the flights data frame. For the fastest flight in the dataset, what was its speed and what were the origin and destination airport codes

#adding new column speed to flights and storing it in another dataframe so as to not edit the original

flights_speed <- flights %>%
  mutate(speed=(distance/(air_time)*60 )) %>%
  arrange(desc(speed)) %>%
  select(origin,dest,speed)

View(flights_speed)
  
#Since we have arranged in descending order of speed, the fastest flight will be in the first row
#printing the first row from flights_speed

print(flights_speed[1,])

#2.i) Of all the flights in 2013 departing from NYC airports, list the top 3 destinations (destination airport codes) with the highest mean arrival delay.

top_dest <- flights %>%
  group_by(dest) %>%
  summarize(mean_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(mean_arr_delay)) 

#printing top 3 destinations in 2013 departing from NYC airports with highest mean arrival delay
 
print(top_dest[1:3,])

