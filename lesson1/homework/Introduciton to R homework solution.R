#homework
# question 1: 
getwd()
# create a vector that includes all the integer from 20 to 40
x=seq(20,40)
x
# what is the integer whose index is 10?
x[10]
# what are the integers whose indexes are 5, 7, 9?
x[c(5,7,9)]
# what are the integers whose value is bigger than 25?
x[x>25]
# what are the mean and standard deviation of this vector?
mean(x)
sd(x)
# question 2:
# create a data frame:
fruits=c('apple', 'banana', 'pear', 'strawberry', 'blueberry', 'peach')
prices=c(2,1,2,6,10,5)
sales=c(70,90,100,50,60,80)
df=data.frame(fruits, prices, sales)
df
# what is the dimension of this data frame?
dim(df)
# what are the data types of each column?
str(df)
# whose price is bigger than or equal to 4
df[df['prices']>=4,]['fruits']
# whose sale is between 50 to 80?
df[(df['sales']>=50)&(df['sales']<=80),]['fruits']
#save this data frame as csv
setwd("/Users/shuozhang/Desktop/kaggle")
write.csv(df, file='df.csv', row.names = FALSE)





