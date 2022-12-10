# data analysis and visualization
# author: shuo zhang, group628llc


# string operation
year = 2014
is.character(year)
as.character(year)
year
year = as.character(year)
year
is.character(year)

#count the number of characters
nchar(year) #length is for a vector

#combine several character variables into one string
first = 'The'
second = 'Chemical'
third = 'Statistician'
my_name = paste(first, second, third, sep = ' ')
my_name
my_name1 = paste(first, second, third, sep = ',')
my_name1

# split 1 string into many shorter strings
strsplit(my_name, split = ' ') # not a vector 
parts = unlist(strsplit(my_name, split = ' ')) # to a vector
parts
parts[1]

# extract a portion of a string
y = "GGACTCTAAATCCGTACTATCGTCATCGTTTTTCCT"
y[1] # character has no index, not like vector
substr(y, 1, 1) 
substr(y, 12, 12) 
substr(y, nchar(y),  nchar(y)) 
substr(y, 4, nchar(y)) # starting point: 4, ending point: last word
# please print the last character in this string
vector=c()
vector=append(vector,substr(y, 1, 1))
vector=append(vector,substr(y, 4, 4))
vector

# determine if a pattern of text exists in a character variable
x = 'ATCG'
grep('A', x) # position 
grep('a', x) 
grep('B', x) # not found
x1 = 'AATCG'
grep('A', x1) # first encounter of a match

#true/false
grepl('A', x1)
grepl('B', x1)

y = 'GGACTCTAAATCCGTACTATCGTCATCGTTTTTCCT'
z1='atcggg'
z = 'CTATCGGGTAGCT'
grepl(x,y)
grepl(x, c(y, z))
grepl(x, c(y, z1)) # case sensitive

grep(x,y)
grep(x,z1)
grep(x,z)

#change lowercase to uppercase
z2=toupper(z1)
grepl(x, c(y, z2))

#replace parts of strings
x='zhang'
gsub('z', 'c', x)
# replace all the symbols to whitespace in string y
y='z,h!ang'
# hint: use nested function or two variables
gsub('!', ' ',gsub(',', ' ', y))
# delete all the symbols
gsub('!', '',gsub(',', '', y))

# vector operation
#  add a element to specific position of a vector
probes <- rep(TRUE, 15)
probes
append(probes, FALSE, after=5)
probes
probes <- append(probes, FALSE, after=5)
probes
probes <- append(probes, FALSE, after=15)
probes

#only return the first encounter of a match in a vector
x <- sample(1:10)
x
match(4,x)
match(c(4,8),x)
x=c(1,3,5)
match(c(4,8),x) # not found: NA (not available)

# true/false
x <- sample(1:4,10,replace=TRUE)
x
x %in% c(2,4)
match(c(2,4),x)
which(x %in% c(2,4)) # index of true


# replace parts of strings in a vector 
str1= c("no abstract available", "A", "A", "B", "no abstract available")
#install.packages('stringr')
library(stringr)
str3 <- str_replace(str1, "no", "yes")
str3

# calculate the frequency of each element of a vector
numbers <- c(4,23,4,23,5,43,54,56,657,67,67,435,
             453,435,324,34,456,56,567,65,34,435)
length(numbers)
table(numbers)
max(table(numbers)) # max frequency
table(numbers) == max(table(numbers)) # which number has max frequency: true
# use multiple functions together
which(table(numbers) == max(table(numbers))) # index of this number
names(which(table(numbers) == max(table(numbers)))) # this number but character
as.numeric(names(which(table(numbers) == max(table(numbers))))) # change to numeric
# sort a vector or factor into ascending or descending order
sort(table(numbers),decreasing = TRUE) 
sort(table(numbers),decreasing = FALSE) 

# data manipulation
#install.packages('nycflights13')
library(nycflights13)
# explore the data set
dim(flights)
head(flights)
summary(flights)
str(flights)
View(flights)
flights$flight=as.character(flights$flight) # change num to chr

#install.packages('dplyr')
library(dplyr)
# select the data of january 1th 
flights[(flights$month==1) &(flights$day==1),]
#select a subset of rows based on condiitons of two columns
set1=filter(flights, month == 1, day == 1)
summary(set1)

# select columns of year, month, day
flights[,c('year', 'month', 'day')]
select(flights, year, month, day)

# sort data frame by year, month, day
arrange(flights, year, month, day)# from the smallest to the largest
arrange(flights, year, month, desc(day)) # from the largest to the smallest: desc()

# add 2 new columns named gain (arr_delay-dep_delay) and speed (distance/air_time*60)
flights$gain=flights$arr_delay-flights$dep_delay
flights$speed=flights$distance/flights$air_time*60
summary(flights)
set2=mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
summary(set2)

# compute aggregation values
summarise(flights,
          avg_delay = mean(dep_delay, na.rm = TRUE))

# calculate the mean value of delay time in feburary and march
# multiple operations: %>%, save memory
set3=flights%>%
  filter(month %in% c(2,3))%>% #select month 2,3
  group_by(month)%>% #convert to 2 groups
  summarise(avg_delay = mean(dep_delay, na.rm = TRUE))%>% # calculate mean value
  arrange(desc(avg_delay)) # sort df by mean value
set3
# data merge: merge two data frames (datasets) vertically, they have one same column name
df1 = data.frame(CustomerId = c(1:6), Product = rep("Toaster", 6))
df2 = data.frame(CustomerId = c(2,6,7), Product =rep("Fridge", 3))
df1
df2
rbind(df1,df2)


# data merge: merge two data frames (datasets) horizontally, they have one same column name
df1 = data.frame(CustomerId = c(1:6), Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2,6,7), State = c(rep("Alabama", 2), rep("Ohio", 1)))
df1
df2
inner_join(df1, df2, by='CustomerId')
left_join(df1, df2, by='CustomerId')
right_join(df1, df2, by='CustomerId')
full_join(df1, df2, by='CustomerId')

merge(x=df1, y=df2, by='CustomerId') # inner_join
merge(x=df1, y=df2, by='CustomerId', all.x = TRUE) # left_join
merge(x=df1, y=df2, by='CustomerId', all.y=TRUE) # right_join
merge(x=df1, y=df2, by='CustomerId', all = TRUE) # full_join

# ggplot2
setwd("~/Desktop/harmony plus kaggle 623/lession2")
housing=read.csv('landdata-states.csv', header=TRUE, stringsAsFactors = FALSE)
# explore the data set
dim(housing)
head(housing)
str(housing)
summary(housing)
colnames(housing)=c( "State", "region","Date","Home_Value" ,"Structure_Cost" ,
                     "Land_Value" ,"Land_Share_Pct","Home_Price_Index",
                     "Land_Price_Index","Year","Qrtr")
head(housing)
# how many states in this data set
length(unique(housing$State))

#install.packages('ggplot2')
library(ggplot2)
# scatter 
# compare the home values in MA and TX and how they change with time
ggplot(data=filter(housing, State %in% c("MA", "TX")), 
       aes(x=Date,
           y=Home_Value,
           color=State))+
  geom_point()+
  xlab("Date")+
  ylab("Home Value") +
  ggtitle("MA vs TX")

# change the every point from the default open circles to filled triangles (pch=17).

ggplot(data=filter(housing, State %in% c("MA", "TX")), 
       aes(x=Date,
           y=Home_Value,
           color=State))+
  geom_point(pch=17)+
  xlab("Date") +
  ylab("Home Value") +
  ggtitle("MA vs TX")

# relationship between land value and structure cost at the first quarter of 2001
hp2001Q1 <- subset(housing, Date == 2001.25) # filter
ggplot(hp2001Q1,
       aes(y = Structure_Cost, x = Land_Value)) +
  geom_point()
# change land value to log scale
ggplot(hp2001Q1,
       aes(y = Structure_Cost, x = log(Land_Value))) +
  geom_point()
# add trend
ggplot(hp2001Q1, aes(x = log(Land_Value), y = Structure_Cost))+
  geom_point()+
  geom_smooth()

# line chart
# compare the home values across all states and how they change with time
ggplot(housing, aes(x = Date, y = Home_Value, color = State))+
  geom_line()
ggplot(housing, aes(x = Date, y = Home_Value))+
  geom_line() +
  facet_wrap(~State, ncol = 10) # create separate graphs in 10 columns 

# histogram
# the distribution of home value
ggplot(housing, aes(x = Home_Value)) +
  geom_histogram(stat = "bin", binwidth=4000)


# bar chart
# which state has the highest average home value?
df=housing%>%
  group_by(State)%>%
  summarise(avg=mean(Home_Value))
df

ggplot(df, aes(x=reorder(State,desc(avg)), y=avg))+
  geom_bar(stat="identity") # change the color of the bar to your favorate color

ggplot(df, aes(x=reorder(State,desc(avg)), y=avg))+
  geom_bar(stat="identity",fill='blue')
# box plot
# the statistical summary of home value at each state
# include max,min, medium, 25%, 75%

ggplot(housing, aes(x=State, y=Home_Value))+
  geom_boxplot()

fac <- with(housing, reorder(State, Home_Value, median, order = TRUE))
housing$State <- factor(housing$State, levels = levels(fac))
ggplot(housing, aes(x=State, y=Home_Value))+
  geom_boxplot()


