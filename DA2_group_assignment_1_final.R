#Business Analytics part-time
#Group Number 31
## Peter, Paziczki
## Yetkin, Cagdas
## Niranjan, Nikitha

#Questions 1 and 2:

#rm(list=ls()) #in case I want to clean my Global Environment
#rm(list=ls())

df <- read.csv("C:/CEU/DA2/class 1/amazon_compare.csv") #original dataset to keep as it is
amzDF <- df # I will play with this one called amzDF
str(amzDF) #it shows 3992 observations and 19 variables. 11 of them are factors, 3 numeric and 5 integer
summary(amzDF)

# Summary is showing more insights. Especially the NAs. example, there are 1310 NAs in price_online variable.
# price column doesnt have any NA.
# Let's drop the observations where price_online is not available. We should have 3992 - 1310 = 2682 obs

library(data.table)
amzDF <- data.table(amzDF)
amzDF <- amzDF[price_online > 0]
str(amzDF)

#Indeed we can see that we dropped the NAs in price_online column
#A double check with summary function confirms this:

summary(amzDF)

#Let's have a closer look to price and price_online variables

summary(amzDF[, 'price', with = FALSE]) 
summary(amzDF[, 'price_online', with = FALSE]) 

#summary function reveals that there is a strange price 146514 for a product.
#most probably it is a measurement error, system error.
#Now we will drop the observations greater than 99th percentile to get rid of the outliers.
#When we do that we will get rid of the strangely high price of 146524 USD. Seeing that it is an Automatic Toilet Bowl Cleaner makes it even funier.
#This strategy can help us to eliminate noise from the signal.

#Lets sort it first:
amzDF$price <- sort(amzDF$price)
round((nrow(amzDF)*(99/100)), 0) #gives result of 2655. It means observations from 2656 to the end are in 99th percentile
# which means we will eliminate 26 observations from the dataset
amzDF <- amzDF[1:round((nrow(amzDF)*(99/100)), 0), , with = TRUE] #now we have only 2655 observations

## TODO: Make an argument on why should we keep them in the sample. Make another argument on why should we drop them.
## The outliers should be removed, because the distort the other values. Outliers can be mistakes, measure errors, etc.
## Depending on the business objective it might be beneficial to keep the outliers, but in this case this must be a measurement error.

# Question 3: Random Number for seed(31)

set.seed(31)
#normal distribution
#sample size=10000, mu=5, sigma=3
id_rand <- rnorm(10000, 5, 3)
sort(id_rand)[1:1000] #sorting and then selecting the first 1000 observations from id_rand

# Question 4: Make summary statistics (mean, standard deviations, quartiles, percentiles) 
# of the variables price and online price. Are there any differences between these statistics?

measures <- amzDF[, list(
  avg_price        = round(mean(price),2),
  std_price        = round(sd(price),2),
  avg_online_price = round(mean(price_online),2),
  std_online_price = round(sd(price_online),2)
)]

#Online prices have a higher mean and stdev. We can see them in the measures table we have just created.
#However, the shape of the spread is still a mystery. A box plot can give us clear 
#insights regarding the quartiles and the median:

boxplot(amzDF$price, amzDF$price_online, 
        data = amzDF, 
        xlab = 'Price and Online Price',
        ylab = 'Price')
#The plot is showing very similar medians.. or are they the same?
#to see also the actual numbers:

summary(amzDF$price)
summary(amzDF$price_online)

#They are really the same... IQRs also very much similar Q3 - Q1..

IQR_price <- IQR(amzDF$price)
IQR_price_online <- IQR(amzDF$price_online)

#IQRs also very much similar Q3 - Q1.. Price IQR is 79.21 and Price_Online IQR is 80.6
#Average online price is higher than average price. And Online Price stdev is much higher than
#stdev of price. This tells us online price is more dispersed. This is visible from the
#box plot also.

hist(amzDF$price,
     xlab = 'price',
     ylab = 'freq.',
     main = 'Histogram of Price')
hist(amzDF$price_online,
     xlab = 'online price',
     ylab = 'freq.',
     main = 'Histogram of Online Price')

## Question 5: Create variables two variables in your dataset: i) variance of the price variable, and ii)
## it?s standard deviation (using the formula included in your Handout and Slides).
## How do they compare to the variance and standard deviations you got using Built-in commands (either in R or Stata).

## Let's create a formula to calculate the variance using the handout.
## I am loading the prices into "x", don't forget, we removed the observations where the online price was not avaiable, so
x <- amzDF$price # we have 2655 observations

## I am creating a function to calculate the squared differences of the observations from the mean, let's call it SqDiff
SqDiff <- function(x) (x - mean(x))^2

## Now I need to sum these values and divide them by the number of variables
VarByHandout <- sum(SqDiff(x))/length(x)
## This gives us the variance of the data, it is 6490.12

## Now we need to calculate the standard deviation, which is the root of the variance
StdDevByHandout <- (sum(SqDiff(x))/length(x))^0.5 # it is 80.56061

## Let's check the variance and the standard deviation of price using the built-in tools
VarByR <- var(x) # The variance is 6492.457
StDevByR <- sd(x) # The standard deviation is 80.57579

## It is very interesting, the formula from the handout and the built-in tool give slightly different values.
## The reason for that is the difference between "population standard deviation" and "sample standard deviation". The formula included in the
## handout is the formula of the population standard deviation, there is "n" in the nominator. But R uses the sample standard deviation where the
## nominator is "n - 1".

## Question 6: Create a dummy (indicator) variable in case the price is greater than the online price. (Non mandatory! Extra point)

length(amzDF$price) # So we have 2655 observations.

## Let's create a dummy (indicator) variable for the observations, where pice is greater than the online price.
amzDF$DummyInd <- ifelse(amzDF$price > amzDF$price_online, 1, 0)
## there are 1327 observations, where the above mentioned condition is true, and there are 1328 observations, where it was false.
table(amzDF$DummyInd)

## Question 7: Create a variable that is the difference between the price and online price. Call this variable diff_price.

## Create a dummy variable taking value one if the diff_price is positive. How does this variable compare to the one above?
## (Non mandatory! Extra point).

## I am adding a column to the dataset, it will contain the difference of price from online price
amzDF$diff_price <- amzDF$price - amzDF$price_online
data.table(amzDF)
## I can see there are both positive and negative values. Now I need to create a dummy variable that takes the value one,if the difference is
## positive, and zero where difference is zero or negative.
amzDF$DummyVar <- ifelse(amzDF$diff_price > 0, 1, 0)
data.table(amzDF)

## The last part of question 7 was: How does this variable compare to the one above?
## We have the same results in both cases, to be precise, we have the same amount of ones and zeros. If the price is greater than the online price,
## it will be marked as 1. If the preice is greater than the online price, the difference of price from online price will be positive, that si to
## be marked with ones. So the result is the same in both cases.

## Question 8: How often do you observe a positive value for the diff_price? How often do you observe no price difference between
## online and offline prices?

amzDF[diff_price > 0, .N] # there are 1327 observations where the diff_price is larger than zero, in these cases the value of DummyVar is 1.
amzDF[diff_price < 0, .N] # there are 1319 observations where the diff_price is less than zero, these are the cases, when price is less than
## the online price.
amzDF[diff_price == 0, .N] # there are 9 observations where the diff_price is exactly equal to zero, with other words, when there is no
## difference between the prices.

## Question 9: What is the probability of observing a positive price difference if the good category is Electronics? What is the probability
## of observing a zero price difference for the category Home and Appliances?

## Probability of observing a positive price difference if the good category is Electronics:
length(which(amzDF$diff_price>0 & amzDF$category=='Electronics'))/length(amzDF$category=='Electronics') ## probability is 0.3013183 %

## Probability of observing a zero price difference for the category Home and Appliances:
length(which(amzDF$diff_price==0 & amzDF$category=='Home and Appliances'))/length(amzDF$category=='Home and Appliances') ## zero probability

## Question 10: Create a scatterplot with price and online price. Interpret it. Create a scatterplot with price and
## amazon price. Interpret it. Do you see any difference between these two scatterplots? What are
## they telling us about the correlation between these variables? (Non mandatory for PART TIME STUDENTS!).

plot(amzDF$price ~ amzDF$price_online)
cor(amzDF$price, amzDF$price_online) #correlation is -0.26

#The plot is not showing a significant correlation at all.
#A very low price can have a very high online price and vice versa
#This requires further analysis why such phenomena is occuring

plot(amzDF$price ~ amzDF$price_amazon)
cor(amzDF$price, amzDF$price_amazon) #correlation is -0.21

#The plot is not showing a significant correlation at all.
#A very low price can have a very high online price and vice versa
#This requires further analysis why such phenomena is occuring

## Question 11: Make box plots and histograms for the price difference over the categories Electronics and
## Pharmacy and Health. What can you tell us about variance in this variable? In which plot, histogram or boxplot, is easier to see the quartiles
## of the distribution?

## box plots and histograms for the price difference over the categories Electronics

elec <- amzDF[which(amzDF$category=='Electronics'),]
hist(elec$diff_price)
boxplot(elec$diff_price)
var((elec$diff_price)) # variance is 18687.11

#Pharmacy and Health.

pharmacy <- amzDF[which(amzDF$category=='Pharmacy and Health'),]

hist(pharmacy$diff_price)
boxplot(pharmacy$diff_price)
var(pharmacy$diff_price) # variance is 43.81546

## The variance is much higher in case Electronics than in Pharmacy and Health.
## In boxplot it is much easier to see quartiles, because they are marked specifically on the boxplot.
