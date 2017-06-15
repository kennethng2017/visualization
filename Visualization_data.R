library(dplyr)
library(ggplot2)
library(tidyr)



air_traffic <- read.table("Air_Traffic_Passenger_Statistics_2014_2016.csv", header =TRUE, sep = ",")

names(air_traffic) <- gsub('[.]', '_', names(air_traffic))

hair_t <- head(air_traffic, n = 15)

tair_t <- tail(air_traffic, n = 15)

names(air_traffic)[2] <- c("Date")

#convert "Activity_Period___Calc" into a date
air_traffic$Date<- as.Date(air_traffic$Date, "%m/%d/%Y")

#create a column for year
air_traffic <- separate(air_traffic, "Date", c("Year", "Month", "Day"), sep = "-", remove = FALSE)

#total passenger per month by year.

air_year <- air_traffic %>%
  group_by(Year, Month) %>%
  summarise(totalpassenger = sum(Passenger_Count)) %>%
  as.data.frame()
            
air_year$Month <- as.integer(air_year$Month)

air_year$Year <- as.factor(air_year$Year)
            
# plot total passenger per month by year.
ggplot(data = air_year, aes(x= Month, y = totalpassenger, color = Year)) +
    geom_line() +
    xlab("Date") +
    ylab("Total Passenger")
            
            
#total passenger per month by airline.
            
air_airline <- air_traffic %>%
  group_by(Year, Month, Operating_Airline) %>%
 summarise(totalpassenger = sum(Passenger_Count)) %>%
  as.data.frame()

air_airline$Date <- paste(air_airline$Year, air_airline$Month, 1, sep = "-")
air_airline$Date <- as.Date(air_airline$Date)                        
air_airline$Month <- as.numeric(air_airline$Month)
                                                    
# plot total passenger per month by airline
ggplot(data = air_airline, aes(x = Date, y= totalpassenger, color = Operating_Airline)) +
  geom_line()+
       xlab("Date") +
      ylab("Total Passenger")
                                                    
                                                    
                                                    
Hometrain <- read.table("trainhome.csv", header =TRUE, sep = ",")
                                                    
#plot SalePrice to see if it is normal
ggplot(data = Hometrain, aes(x = SalePrice)) +
  geom_histogram()
                                                    
#plot sale price by 1stFlrSF, fill with fullbath
ggplot(data = Hometrain, aes(x = X1stFlrSF, y = SalePrice, fill = FullBath)) +
      geom_point(na.rm = TRUE)
                                                    
#plot sale price by 2ndFlrSF, fill with fullbath
ggplot(data = Hometrain, aes(x = X2ndFlrSF, y = SalePrice, fill = FullBath)) +
      geom_point()

#plot sale price by TotalBemtSF, fill with fullbath
ggplot(data = Hometrain, aes(x = TotalBsmtSF, y = SalePrice, fill = FullBath)) +
  geom_point()
                                                    
                                                    
#boxplot for yrsold
ggplot(data = Hometrain, aes(x = MSZoning, y = SalePrice)) +
 geom_boxplot()
                                                    
                                                    
save(air_traffic, Hometrain, file = "complete_data.Rdata")                                                    
