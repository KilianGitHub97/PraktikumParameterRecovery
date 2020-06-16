install.packages("data.table")
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(foreach)
library(doParallel)
# Load File ---------------------------------------------------------------

input_url <- "https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv"

flights <- fread(input_url)

head(flights)

glimpse(flights)

# Converting data.frame to data.table -------------------------------------
df001 <- data.frame(
  R1 = 1:6,
  R2 = 7:12,
  R3 = 13:18,
  ID = c("a", "a", "b", "b", "b", "c")
)

class(df001) #data.frame

df002 <- setDT(df001) #convert data.frame to data.table: setDT()

class(df002)#data.table


# basics ------------------------------------------------------------------

##   R:                 i                 j        by
## SQL:  where | order by   select | update  group by
##Take DT, subset/reorder rows using i, then calculate j, grouped by by

df003 <- flights[origin == "LGA" & month == 6] #data.table with only the selected values

df004 <- flights[1:5] #get the first five rows of flights

df005 <- flights[,dest] #get the column dest as a vector

df006 <- flights[, list(dest)] #get column dest as a data.table (somehow equivalent to dplyr::transmute()), more than one column is also possible

df007 <- flights[,list(destination = dest)] #rename column to "destination"

df008 <- flights[order(air_time, -distance)] #Sort flights first by column air_time in ascending order, and then by distance in descending order


# computations with data.table  -------------------------------------------

df009 <- flights[, sum((dep_delay + arr_delay) != 0)] #compute how many times the flights had any delay

df010 <- flights[dest != "LAX" & hour == 17, list(Mean_delay = mean(arr_delay))] #subset dest and hour and then calculate the mean of arrival_delay

df011 <- flights[dest == "LAX", length(dest)] #count number of flights with dest = LAX. 

df012 <- flights[, .N] #rows of data.table (somehow equivalent to dplyr::n())


# some more selecting -----------------------------------------------------

df013 <- flights[,c("arr_delay", "dep_delay")] #subset by variable name

df013.5 <- flights[, list(arr_delay, dep_delay)] # subset by variable name


VarVector <- c("arr_delay", "dep_delay")

df014 <- flights[, ..VarVector] #subset columns saved in an external vector

df014.5 <- flights[, VarVector, with = FALSE] #subset columns saved in an external verctor

df015 <- flights[, -c("arr_delay", "dep_delay")] #select all columns but "arr_delay and "dep_delay"

df015.25 <- flights[, !c("arr_delay", "dep_delay")] #select all columns but "arr_delay and "dep_delay"

df015.5 <- flights[, -..VarVector] #select all columns but "arr_delay and "dep_delay" (equivalent to dplyr::select)

df015.75 <- flights[, -VarVector, with = FALSE] #select all columns but "arr_delay and "dep_delay"

df016 <- flights[, year:dep_delay] #select all columns between year and dep_delay


# Aggregation with data.table ---------------------------------------------

df017 <- flights[, list(Total_Flights = (.N)), by = "hour"] #number of rows sorted by counts // by accepts double or character
df017 <- df017[order(hour)] 

ggplot(data = df017 , mapping = aes(x = hour, y = N)) + geom_line(color = "red") +theme_minimal() #lineplot with the number of flights PER hour

df018 <- flights[origin == "JFK", list(Total_Flights = (.N)), by = "hour"] #number of flights by hour that came from New York
df018 <- df018[order(-hour)]
df018.5 <- flights[origin == "JFK", list(Total_Flights = (.N)), by = "hour"][order(-hour)] #if we don't want to do write another line, we can simply chain two operations together: [...][...]

registerDoParallel(cores = 4)
foreach(i = 1:10, .packages = c("data.table", "tidyverse")) %dopar% { #flights PER hour PER day PER month
  df019 <- flights[month == i,list(Total_Flights = (.N)), by = list(hour, day)] #number of flights by hour, day and month
  graph <- ggplot(data = df019 , mapping = aes(x = hour, y = Total_Flights)) + ggtitle("Month",i) + geom_line(color = "red") + facet_wrap(~day) +theme_minimal() #lineplot with the number of flights PER hour PER day PER month
  print(graph)
}

df020 <- flights[, list(Total_Flights = (.N)), keyby = "hour"] # order directly with keyby

df021 <- flights[, list(air_time, distance), by = list(distance > 1000)][order(distance)] # TRUE values fulfill the critera of by =


# .SD ---------------------------------------------------------------------

df022 <- df002[, {print(.SD); print(.BY)}, by = ID] #print all elements from df002 separated by ID // instead of print, one can use head()

df022.5 <- flights[, {print(.SD); print(.BY)}, by = hour] #print all elements from flights separated by hour

operations <- list(min, max, mean, sd)
for (j in operations) { #finding out min, max, mean and sd
  df023 <- flights[, -c("carrier", "origin", "dest")][, lapply(.SD, j), by = hour][order(hour)] #first delete all character columns, then compute the means of the remaining columns by hour
  #Since lapply() returns a list, there is no need to wrap it with an additional list()
  print(df023)
}

df024 <- flights[, -c("carrier", "origin", "dest")][, lapply(.SD, mean), keyby = hour, .SDcols = c("dep_delay", "arr_delay", "air_time", "distance")] #means only of the wanted columns