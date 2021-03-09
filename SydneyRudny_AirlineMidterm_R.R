#Sydney Rudny
#Airline Midterm
#October 22nd, 2020

#import all libraries needed
library(ggthemes)
library(ggplot2)
library(dplyr)
library(plyr)
library(readr)


#downloads data all together from file
airlineData <- list.files(path = "/Users/rudny/Documents/Fall 2020/IS 470/Midterm Project/AirlineData",
                          pattern = "*.csv", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows
#downloads csv carrier table
lookup <- read.csv("/Users/rudny/Documents/Fall 2020/IS 470/Midterm Project/L_UNIQUE_CARRIERS.csv")

#merge lookup table with data
joinedData <- merge(airlineData, lookup, by.x = "OP_UNIQUE_CARRIER", 
                   by.y = "Code", all.x = TRUE, all.y = FALSE)

#mutates a new data frame so original stays in tact
airlineData2 <- joinedData %>% mutate(flight_date = as.Date(joinedData$FL_DATE, format = "%m/%d/%Y"))
#parse out the day,month,and year so they are separate values
airlineData2 <- airlineData2 %>% mutate(flight_month = format(airlineData2$flight_date,"%m"))
airlineData2 <- airlineData2 %>% mutate(flight_day = format(airlineData2$flight_date,"%d"))
airlineData2 <- airlineData2 %>% mutate(flight_year = format(airlineData2$flight_date,"%Y"))
#got rid of two columns not needed
airlineData2[2] <- NULL
airlineData2[17] <- NULL

#general statistical analysis to see what the data is like

summary(airlineData$DEP_DELAY)
mean(airlineData$DEP_DELAY, na.rm = T)
median(airlineData$DEP_DELAY, na.rm = T)
quantile(airlineData$DEP_DELAY, na.rm = T)
sapply(airlineData2, class)



#creates two data frames with filtered results by year
d2019 <- airlineData2 %>% filter(flight_year == "2019")
d2020 <- airlineData2 %>% filter(flight_year == "2020")


#from the previous df, two new aggregate sum frames are made
cancel19 <- aggregate(CANCELLED ~ Description, d2019, sum)
cancel20 <- aggregate(CANCELLED ~ Description, d2020, sum)
#bar plot showing the sum of canceled flights for carrier by year
ggplot(cancel19,aes(CANCELLED,Description)) + geom_bar(stat = "identity")+ xlab("Number of Cancelations") + ylab("Carrier")+ggtitle("Cancels by Carrier (2019)")+theme_economist() 
ggplot(cancel20,aes(CANCELLED,Description)) + geom_bar(stat = "identity")+ xlab("Number of Cancelations") + ylab("Carrier")+ggtitle("Cancels by Carrier (2020)")+theme_economist() 

#creates data frames of march data in each year
march19 <- airlineData2 %>% filter(flight_year == "2019" & flight_month == "03")
march20 <- airlineData2 %>% filter(flight_year == "2020" & flight_month == "03")
#create aggregated data frame of march canceled sum
marchCX19 <- aggregate(CANCELLED ~ Description, march19,sum)
marchCX20 <- aggregate(CANCELLED ~ Description, march20, sum)
#graph showing flights canceled by carrier in March (show covid effects)
ggplot(marchCX19,aes(CANCELLED,Description)) + geom_bar(stat = "identity")+ xlab("Number of Cancelations")+ ylab("Carriers")+ggtitle("Number of Carrier Cancels in March 2019") + theme_economist()
ggplot(marchCX20,aes(CANCELLED,Description)) + geom_bar(stat = "identity")+ xlab("Number of Cancelations")+ ylab("Carriers")+ggtitle("Number of Carrier Cancels in March 2020") +theme_economist()



#arrival vs departure delay plot 2019 vs 2020
#creates delay df with filters of a dep of at most 30 min early and at most 5 hours delay (get rid of outliers)
delay <- airlineData2 %>% filter(DEP_DELAY > -30 & DEP_DELAY < 300 & ARR_DELAY<300 & ARR_DELAY > -30 & flight_year == "2019")
ggplot(delay,aes(DEP_DELAY,ARR_DELAY))+geom_hex()+xlab("Depart Delay in Minutes")+ylab("Arrival Delay in Minutes")+ggtitle("Do flights make up departure delays en route(2019)?")

delay20 <- airlineData2 %>% filter(DEP_DELAY > -30 & DEP_DELAY < 300 & ARR_DELAY<300 & ARR_DELAY > -30 & flight_year == "2020" & flight_day == "01")
ggplot(delay20,aes(DEP_DELAY,ARR_DELAY))+geom_hex()+xlab("Depart Delay in Minutes")+ylab("Arrival Delay in Minutes")+ggtitle("Do flights make up departure delays en route (2020)?")

#plots the year data for the count of carrier flights (carrier code used for ease of reading label)
ggplot(d2019,aes(OP_UNIQUE_CARRIER)) + geom_bar() + xlab("Carrier") + ylab("Number of Flights")+ggtitle("Number of Flights by Carrier in 2019")+theme_economist()
ggplot(d2020,aes(OP_UNIQUE_CARRIER)) + geom_bar() + xlab("Carrier") + ylab("Number of Flights")+ggtitle("Number of Flights by Carrier in 2020")+theme_economist()

#creates 2 df for the two years filtering arrival delay data
arr19<- d2019 %>% filter(ARR_DELAY > 20 & ARR_DELAY < 300 )
arr20<- d2020 %>% filter(ARR_DELAY > 20 & ARR_DELAY < 300 )
#creates a boxplot of the spread of delay times for each carrier
ggplot(arr19,aes(ARR_DELAY,Description)) + geom_boxplot()+xlab("Arrival Delay in Minutes")+ylab("Carrier")+ggtitle("Average Arrival Delay by Carrier (2019)")+theme_economist()
ggplot(arr20,aes(ARR_DELAY,Description)) + geom_boxplot()+xlab("Arrival Delay in Minutes")+ylab("Carrier")+ggtitle("Average Arrival Delay by Carrier (2020)")+theme_economist()

#filters the data into two new df to pull out popular airlines only
popularA20<-d2020 %>% filter(Description == "Southwest Airlines Co." | Description == "American Airlines Inc." | Description=="United Air Lines Inc." | Description=="Delta Air Lines Inc.")
popularA19<-d2019 %>% filter(Description == "Southwest Airlines Co." | Description == "American Airlines Inc." | Description=="United Air Lines Inc." | Description=="Delta Air Lines Inc.")
#plots a bar chart of the number of flights each of these popular carriers had in the first quarter of 2019 and 2020
ggplot(popularA20,aes(Description)) + geom_bar()+xlab("Carrier")+ylab("Number of Flights")+ggtitle("Popular Carriers Flights ")+ theme_economist()
ggplot(popularA19,aes(Description)) + geom_bar()+xlab("Carrier")+ylab("Number of Flights")+ggtitle("Popular Carriers Flights ")+theme_economist()




