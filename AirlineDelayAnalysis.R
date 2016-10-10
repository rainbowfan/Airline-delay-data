###AirlineDelay Data Analysis
###Data exploration and data visualization 
###data mining by SQL, data analysis and data visualization done by R
library(DBI)
library(RSQLite)
library(ggplot2)

delay.con = dbConnect(RSQLite::SQLite(), dbname = "/Users/hongfan/AirlineDelay.sqlite3")

#The number of 1987 flights
dbGetQuery(delay.con, "SELECT COUNT(*), Year FROM AirlineDelay 
           WHERE Year=1987")

#The number of flights in each year from 1987 to 2008
dbGetQuery(delay.con, 
           "SELECT COUNT(*), Year FROM AirlineDelay GROUP BY Year")

#The number of Saturday flights
dbGetQuery(delay.con,
           "SELECT COUNT(*), DayofWeek FROM AirlineDelay
           WHERE DayofWeek=6")

#Which airline has the most flights
dbGetQuery(delay.con,
           "SELECT COUNT(*), UniqueCarrier FROM AirlineDelay
            GROUP BY UniqueCarrier
            ORDER BY COUNT(*) DESC Limit 1")

#Compute the number of flights for each originating airport and airline carrier (two-way frequency table).
#Show only the rows and columns for the 20 airports with the largest number of flights, and the 10 airline 
#carriers with the most flights. 
dbGetQuery(delay.con,
           "SELECT UniqueCarrier, Origin, count(*) FROM AirlineDelay
            WHERE UniqueCarrier IN (SELECT UniqueCarrier FROM AirlineDelay
                                    GROUP BY UniqueCarrier
                                    ORDER BY COUNT(*) DESC Limit 10)
                  AND
                  Origin IN (SELECT Origin FROM AirlineDelay
                             GROUP BY Origin
                             ORDER BY COUNT(*) DESC Limit 20)
            GROUP BY UniqueCarrier, Origin")
                  
#Is the mean delay in November different from the mean delay in December
dbGetQuery(delay.con,
           "SELECT AVG(ArrDelay) FROM AirlineDelay
            WHERE Month=11 OR Month=12
            GROUP BY Month")

#Total number of flights in 2008?
dbGetQuery(delay.con, "SELECT COUNT(*), Year FROM AirlineDelay 
                       WHERE Year=2008")

#For flights data in 2008, which is a better measure for characterizing the center
#of the distribution for overall delay-mean, median, or mode?
data_2008 = dbGetQuery(delay.con, "SELECT * FROM AirlineDelay 
                                   WHERE Year=2008")
names(data_2008)
class(data_2008)
hist(data_2008$ArrDelay, xlab="Delay(minutes)", prob=TRUE) #skewed to the right
quantile(data_2008$ArrDelay, seq(.9, 1, by = 0.01), na.rm = TRUE)
median(data_2008$ArrDelay, na.rm = TRUE) #median is a good measure of the center of this distribution

#What is the mean and standard deviation of the arrival delays for all United Airlines (UA) flights
#on the weenkend out of SFO from 1987 to 2008
dat_UA = dbGetQuery(delay.con, "SELECT Year, ArrDelay FROM AirlineDelay
                                WHERE UniqueCarrier = 'UA' AND (DayOfWeek = 6|7) AND Origin = 'SFO'")
tapply(dat_UA$ArrDelay, dat_UA$Year, mean, na.rm = TRUE)
tapply(dat_UA$ArrDelay, dat_UA$Year, sd, na.rm = TRUE)

#Display the number of flights in 2008 for each airport on a single plot so we can quickly compare them.
tmp1 = dotplot(sort(table(c(data_2008$Origin, data_2008$Dest))))
tmp2 = sort(table(c(data_2008$Origin, data_2008$Dest)))
tmp2 = tmp2[tmp2 > quantile(tmp2, .80)]
figure1 = dotplot(tmp2, main = "Total Flights by Airport in 2008")

fname = "TotalFlights_Airport.png"
png(filename = fname, width = 490, height = 759)
print(figure1)
dev.off()

#Are there many more flights on weekdays relative to Saturday and Sunday in 2008? 
#What day of the week has the most number of delayed flights
tbl = table(data_2008$DayOfWeek)
names(tbl) = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
figure2 = dotplot(tbl, main = "Total Number of Flights by Day in 2008")
fname = "TotalFlights_Day08.png"
png(filename = fname, width = 512, height = 485)
print(figure2)
dev.off()

#What day of the week has the largest median overall delay
sort(tapply(data_2008$ArrDelay, data_2008$DayOfWeek, median, na.rm = TRUE))

#Consider the 10 airports with the most number of flights. For this subset of the data, find the 
#routes (origin-destination pair) which have the worst median delay.
tmp3 = sort(table(c(data_2008$Origin, data_2008$Dest)), decreasing = TRUE)[1:10]
name = names(tmp3)
tmp4 = subset(data_2008, Origin %in% name | Dest %in% name)
tmp5 = tapply(tmp4$ArrDelay, tmp4[, c("Origin", "Dest")], median, na.rm = TRUE)
ind = arrayInd(which.max(tmp5), dim(tmp5))
c(rownames(tmp5)[ind[1]], colnames(tmp5)[ind[2]]) #GUC ATL

#For flights originating from SFO, what are the 5 most popular destination airports
sfo = subset(data_2008, Origin == "SFO")
ind_des = names(sort(table(sfo$Dest), decreasing = TRUE))[1:5]
#"LAX" "LAS" "JFK" "SAN" "ORD"

#Graphically display any relationship between the distance of the flight and the overall delay in 2008
plot(ArrDelay ~ Distance, data_2008)
#exclude extreme delays
with(data_2008[data_2008$ArrDelay < 180, ], smoothScatter(Distance, ArrDelay)) 
#zoom in and exclude extreme delays
figure3 = with(data_2008[data_2008$ArrDelay < 180 & data_2008$ArrDelay > -100, ], smoothScatter(Distance, ArrDelay, main="Arrival Delay vs Distance"))
fname = "ArriveDelay_Distance08.png"
png(filename = fname, width = 512, height = 483)
print(figure3)
dev.off()
#The figure above suggests in general the distribution of delay does not change with distance.

#What are the worst hours to fly in 2008 in terms of experienceing delays
hour = floor(data_2008$CRSDepTime/100)
md = tapply(data_2008$ArrDelay, hour, median, na.rm = TRUE)
q75 = tapply(data_2008$ArrDelay, hour, quantile, .75, na.rm = TRUE)
q85 = tapply(data_2008$ArrDelay, hour, quantile, .85, na.rm = TRUE)
q95 = tapply(data_2008$ArrDelay, hour, quantile, .95, na.rm = TRUE)
figure4 = matplot(as.integer(names(md)), cbind(md, q75, q85, q95), type = "l",
                  xlab = "Hour of day", ylab = "Delay (minutes)", main = "Delay vs Hour of Day")
legend('topleft', legend = c("median", "0.75", "0.85", "0.95"), col = c(1:4), lty = 1, cex = 0.8)
fname = "Delay_Hr08.png"
png(filename = fname, width = 512, height = 483)
print(figure4)
dev.off()
#The figure shows there is little variation in the median and no particular hour stands out as being 
#very unusual. The morning hours are the best in terms of the median. The 99th quantitle shows a lot 
#more variability.

#Does the distribution of delays depend on the time of day?
hour = floor(data_2008$CRSDepTime/100)
densityplot(~ ArrDelay, data_2008, groups = hour, plot.points = FALSE, auto.key = list(space = "right"))
#The figure above shows distributions are very similar for each hour 
#exclude flights that were less than 3 hours delayed. 
data_2008$hour = hour
densityplot(~ArrDelay, data_2008, groups = hour, subset = (ArrDelay <= 180), plot.points = FALSE, 
            auto.key = list(space = "right", title = "Time of Day", cex.title = 1), main = "Distribution of Delays on Time of Day", 
            xlab = "Arrival Delays") 
#Distributions are quite similar

#Do planes leaving late tend to make up time?
makeup = data_2008$DepDelay - data_2008$ArrDelay
densityplot(~makeup, groups = data_2008$ArrDelay > 0, plot.points = FALSE, 
            auto.key = list(columns = 2, text=c("Not Delayed","Delayed")),
            main = "Comparisons between Delayed and Un-delayed Flights by Make-up Time",
            xlab = "Make Up", xlim = c(-300, 100))
#the plot shows the time the plane made up for delayed flights is generally smaller than that of flights that depart on time or early.
