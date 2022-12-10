# introduciton to R
# author: shuo zhang, group628.llc  

# set working directory
getwd() # if not set, the default is root directory
setwd("~/Desktop") # change working directory
getwd()

# show current working directory

# simple value and expression
# assign a number to a variable 
a=1
a<-1
# print a's value
print(a)
a
# assign a string to a varibale 
b='hello'
# return b's value
b
# anything that is enclosed by double-quotes ("") or single-quotes('') is a string
c='1'
c
c="1"
c
a==c
class(a)==class(c)

# function
# math calculation
1+2 #plus
1-2 #minus
2*3 #times
2^3 # 2 to the 3rd power
5/2 # divided by
# 5 divided by 2 equals to one with a remainder of one
5%/%2# quotient：2
5%%2 # reminder: 1
log(100,10)# base is 10
log10(100)
# if change base to 2
log(4,2)
log2(4)
x<-pi/2 #pi over 2
sin(x)

# check the data type
class(1)
class(1.0)
class('hello') 
class('1.0')

# logical expression
1==1
1<1
1==1 & 1<1 # and, when both true, return true
1==1|1<1 # or, as long as one ture, retrun true
!(1<1) # not equal: opposite
class(1==1) 

# change the data type of variables
x="helllo"
class(x)
x=1
x
class(x)
y=as.character(x)
y
class(y)
x
y='1.5'
x=as.numeric(y)
x
x=as.integer(y)
x # only take the interger part, no rounding
y # all these functions would not change the value or data type of the original variable
class(y) # immutable function

# vector
# numeric vector
x=c(1,2,4)
x
# categorical vector 
y=c('ab', 'bc', 'cd')
y
# if mixed data type: change numeric to character
y=c('ab', 1) # not regular
y
y=c('ab', '1') # regular

# create numeric/categorical vectors
seq(1,10) # start point is 1, end point is 10
seq(1,10, by=2) # interval is 2
seq(1,10, length=5)# equal distributed, total length is 5

rnorm(10) # mean 0, standard deviation:1
runif(10) # min 0, max 1

set.seed(1)
rnorm(10)
set.seed(2)
rnorm(10)

sample(c('a', 'b', 'c'), size=10, replace = TRUE) # the probability of picking a,b,c is equal
sample(c('a', 'b', 'c'), size=3, replace = FALSE) # maximum size is the length of the vector 

rep(0,10) 
rep(c(1,2,3), 4) # repeat the vector 4 times
rep(c(1,2,3), each=4) # repeat each value 4 times

# vector index: starting from 1
x=c(1,2,4)
x
x[1] # single element
x[c(2,3)] # multiple elements into a vector
x[c(-1, -3)] # delete elements
# one condition
x>1
x[x>1] 
x[!(x>3)]#x<=3
x[!(x>0)]#x<=0
# two conditions
x[x>1&x<4] 
x[x>1|x<4]

# modify a vector
x[1]=2
x
length(x)

# vector manipulation
# math caculation
# same length
c(1,2,3)+c(4,5,6)
# a vector and a value
c(1,2,3)+1
# different lengths
c(1,2,3)+c(2,3) # c(1,2,3)+c(2,3,2)
c(1,2,3)-c(4,5,6)
c(1,2,3)*c(4,5,6)
c(1,2,3)/c(4,5,6)

# aggregation function of a vector
x=c(1,2,4)
x
sum(x)
max(x)
min(x)
mean(x)
sd(x)
summary(x)

# data frame
# create a data frane
Name=c('John', 'Amy', 'Emma','Emma', 'Cindy', 'Lucy', 'Cindy')
Age=c(10,10,11,12,13,14,15)
df=data.frame(Name, Age, row.names=seq(1,7))
df
# add a new column
df$State=c('NY', 'NJ', 'CA','NY', 'WI', 'TX', 'MD')
df
# explore the data frame
colnames(df)
rownames(df)
dim(df)# 3 columns, 7 rows
str(df) # factor represents levels
df$Name=as.factor(df$Name) # change  character to factor
str(df)
summary(df)

# select row/column
# based on row/column index
df[1] # first col: data frame
df[,1]# first col: vector
df[1,] # first row: data frame
df[1,1]# first row, first col: a value

df$Name # vecotr
df['Name'] # data frame

# condition
# people information with age over 10
df$Age>10
df[df$Age>10,]
# people name with age over 10
df[df$Age>10,]$Name
df[df$Age>10,]['Name']
#people information with age over 10 and live in CA
df[(df$Age>10) & (df$State=='CA'),]

#view data frame 
head(df) #first 6 rows
tail(df) # last 6 rows
View(df)

# data frame manipulation
order(df$Age, decreasing=TRUE)
df[order(df$Age, decreasing=TRUE),]
df
df1=df[order(df$Age, decreasing=TRUE),] # FALSE
df1
# store/read data
write.csv(df1, file='df1.csv', row.names = FALSE)
getwd()
df1=read.csv('df1.csv')
df1
str(df1) # name saved as character
df=read.csv('df1.csv', stringsAsFactors = FALSE)
df
str(df)

# conditional loop
# two loops
num=3
if (num%%2==0){
  cat(num, 'is even')
}else{
  cat(num, 'is odd')
}
# three loops
num=3
if(num==0){
  cat(num, 'is 0')
}else if(num%%2==0){
  cat(num,'is even')
  }else{
  cat(num, 'is odd')
  }
num=4
if(num==0){
  cat(num, 'is 0')
}else if(num%%2==0){
  cat(num,'is even')
}else{
  cat(num, 'is odd')
}
num=0
if(num==0){
  cat(num, 'is 0')
}else if(num%%2==0){
  cat(num,'is even')
}else{
  cat(num, 'is odd')
}

# use function 
num=3
ifelse(num%%2==0, 'even', 'odd')
ifelse(num==0, '0', ifelse(num%%2==0, 'even', 'odd')) # nested function

# for loop
1:10 #seq(1,10)
for (i in 1:10){
  print(i)
}
# summarize the even values between 1 to 100
sum=0
for (i in 1:100){
  if (i%%2==0){
    sum=sum+i
  }
}
sum
# summarize the odd values between 1 to 100
sum=0
for (i in 1:100){
  if (i%%2!=0){
    sum=sum+i
  }
}
sum

# while loop
# one condition
i=1
while(i<11){
  print(i)
  i=i+1
}

# two conditions
i=3
while (i < 6 && i>2) {
  print(i)
  i = i+1
}
# or:  ||
# not equal: !=
i=3
while (i < 6  && i!=5) {
  print(i)
  i = i+1
}

# summarize the even values between 1 to 100
i=1
sum=0
while (i<101){
  if (i%%2==0){
    sum=sum+i
  }
  i=i+1
}
sum

# practice
# Question 1:
#Each new term in the Fibonacci sequence is generated by adding the previous two 
# terms. Starting with 1 and 2, the first 10 Fibonacci numbers are:
#  1, 2, 3, 5, 8, 13, 21, 34, 55, 89,...
# Consider all the Fibonacci numbers whose values do not exceed four million. 
# Find the sum of even-valued terms among them.
i = 2
x = 1:2
sum = 0 
while (x[i] < 4e6){
  if (x[i] %% 2 == 0){
    sum = sum + x[i]
  }
  x[i+1] = x[i] + x[i-1]
  i = i + 1
}
sum

# Question 2:
# Write some R code to generate a vector with the following elements, 
# without using loops .
# "aa" "ba" "ca" "da" "ea" "ab" "bb" "cb" "db" "eb" "ac" "bc" "cc" "dc" "ec"
# "ad" "bd" "cd" "dd" "ed" "ae" "be" "ce" "de" "ee"

list<-c("a","b","c","d","e")
list1<-rep(list,5)
list1
list2<-rep(list,each=5)
list2
list3<-paste0(list1,list2)
list3





