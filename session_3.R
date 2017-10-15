rm(list=ls())
hotels <- read.csv('http://bit.ly/CEU-R-hotels-2017')

library(data.table)
hotels <- data.table(hotels)
setDT(hotels)

## TODO compute the required budget (EUR) to try all hotels

str(hotels)
hotels$price_EUR <- hotels$price_HUF / 310 # this is the point where we created price_EUR
str(hotels)
sum(hotels$price_EUR) # 939813.7 EUR to try all the rooms

## daroczi megoldas
str(hotels)
hotels[, sum(price_HUF) / 310]
## or
hotels$price_EUR <- hotels$price_HUF / 310
hotels[, sum(price_EUR)]

## DT way of creating new variables
hotels[, price_EUR := price_HUF / 310] # := the data assignment operator, we should prefer this one, because it is short an elegant

## TODO categorize hotels into 3 bucket based on price => pricecat

pricecat <- table(cut(hotels$price_HUF, breaks = 3))
pricecat

## daroczi megoldas
hotels[, pricecat := cut(price_EUR, 3, dig.lab = 8)]
str(hotels)

## the number of observations in the buckets
hotels[, .N, by = pricecat]

## providing break points by my own request
hotels[, pricecat := cut(price_EUR, c(0, 60, 120, Inf))]
hotels[, .N, by = pricecat]

## TODO get better break point
boxplot(hotels)
summary(hotels$price_EUR) # it shows me, where sould I put the breaking points in code above

hotels[, pricecat := cut(price_EUR, c(0, 60, 120, Inf),
                         labels = c('cheap', 'average', 'expensive'))] # giving labels to rows
hotels[, .N, by = pricecat]

## TODO mean, sd on price_EUR
avg_price <- mean(hotels$price_EUR)
sd_price <- sd(hotels$price_EUR)

## daroczi megoldas
hotels[, mean(price_EUR)]
hotels[, sd(price_EUR)]

## creating confidence intervalls
hotels[, pricecat := cut(price_EUR, c(0,
                                      avg_price - sd_price,
                                      avg_price + sd_price,
                                      Inf))] # it is better to have variables, having hard coded number is not good, don't do that
## if we reloaded the data with changes, we might need to change the breakiing points, but in the case above we don't need to do that,
## we have formulas to compute it
hotels[, .N, by = pricecat]
hist(hotels$price_EUR)

## TODO compute the average price per city
hotels[, list(avg_price = mean(price_EUR)), by = city] # computing the average price
## this data set is not stored anywhere, it is just printed here
## so let's store it
price_per_city <- hotels[, list(avg_price = mean(price_EUR)), by = city]

## another approach using the assignment operator
hotels[, price_avg := mean(price_EUR), by = city]
str(hotels) # we can see, 175 is repeating in many rows
hotels

hotels[, price_Sd := sd(price_EUR), by = city]
str(hotels)

## now we can use the new variables
hotels[, pricecat := cut(price_EUR,
                         c(0,
                           price_avg - sd_price, # this is a vector of 7000+ observations
                           price_avg + sd_price,
                           Inf),
                         c('below avg', 'avg', 'above avg'))]
## the code above was not functioning, because the breaking points were not unique, there were vectors in the code, let's do it right
hotels[, pricecat := cut(price_EUR,
                         c(0,
                           price_avg[1] - sd_price[1], # this is a vector of 7000+ observations
                           price_avg[1] + sd_price[1],
                           Inf),
                         c('below avg', 'avg', 'above avg')),
       by = city]
hotels[, .N, by = pricecat]

str(hotels$price_avg[1])
str(hotels)

## TODO city types => small, big
## citytype variables is to be used

hotels[, list(hotels = .N), by = city] # number of hotels per city
hotels[, .N, by = city]
hotels[, hotels := .N, by = city] # we creaed "hotels" variable
str(hotels)

hotels[, citytype := cut(hotels, 2, labels = c('small', 'big'))]
hotels[, .N, by = citytype] # frequency table by citytype

hotels[, .N, by = list(citytype, pricecat)] # providing a list by variabla names
hotels[, .N, by = list(city = citytype, price = pricecat)] # I can rename the columns as well

## Let's take it further
hotels[, .N, by = list(city = citytype, price = pricecat)][order(price, city)] # we are ordering the obs by price and city
## don't forget, we can use names we have just created above
## let's change the order, see what happens
hotels[, .N, by = list(city = citytype, price = pricecat)][order(city, price)] # we can change the order
## important note, the order command does not change the order in the data set, it is only a temporary thing for the time being plotted
## so the order in data set is untouched. If I overwrote the data table, then it would be permanent

## TODO number of hotels per price cat and city type => % in city tpye
price_per_type <- hotels[, .N, by = list(city = citytype, price = pricecat)][order(city, price)]
price_per_type
## TODO add a new column called P => % per city type
3600 / (3600 + 439 + 131) * 100
price_per_type[, P := N / sum(N) * 100, by = city]
price_per_type
price_per_type[, P := round (N / sum(N) * 100,2), by = city] # ♦rounding the percentages
price_per_type

## TODO min, avg, max of price
hotels[, list(
  min_price = min(price_EUR),
  avg_price = mean(price_EUR),
  max_price = max(price_EUR)
), by = city]

hotels[, list(
  min_price = round(min(price_EUR), 2),
  avg_....
)]


## TODO compute the average price, rating, stars per city in a new dataset
hotels[, (price_avg = mean(price_EUR)),
          (rating_avg = mean(rating)),
          (stars_avg = mean(stars, na.rm = TRUE)), by = city]

## check the same on rating and stars
hotels[, lapply(.SD, mean), by = city, .SDcols = c('price_EUR', 'rating', 'stars')]

## TODO check the same on rating and stars
hotels[, lapply(.SD, mean), by = city, .SDcols = c('price_EUR', 'rating', 'stars')] # there are a lot of NAs
hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, .SDcols = c('price_EUR', 'rating', 'stars')]

## save data for later use
write.csv(hotels, 'hotels-with-two-new-factors.csv')

## =====================================================================================================================

## load new data
hotels <- read.csv('http://bit.ly/CEU-R-hotels-2017-v2')
hotels <- data.table(hotels)
setDT(hotels)

## read.csv => fread
hotels <- fread('http://bit.ly/CEU-R-hotels-2017-v2')

library(ggplot2)

ggplot(hotels, aes(x = pricecat)) + geom_bar() # aes defines what and geom defines how I want show, we are combining layers with +
ggplot(hotels, aes(x = pricecat)) + geom_bar() + theme_bw() # background is white instead of gray
ggplot(hotels, aes(x = pricecat)) + geom_bar(colour = 'blue', fill = 'orange') + theme_bw()
## ggplot is returning ggplot objects
p <- ggplot(hotels, aes(x = pricecat)) + geom_bar() # iam loading it to a variable
p
p + theme_bw()

## coordinate transformations
library(scales)
p + scale_y_log10() # now i have the axe on a log scale
p + scale_y_sqrt()
p + scale_y_reverse()
p + coord_flip()

## geoms => scatter
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() # it is a scatterplot, but does not make sense
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point(alpha = 0.1)
ggplot(hotels, aes(x = price_EUR, rating)) + geom_hex()
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() + geom_smooth()
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() + geom_smooth(method = 'lm') # you can see the confidence interval, that is the
## grey thing around the line

## install.packages('hexbin')
## ?geom_hex

## TODO preice_EUR + stars
str(hotels)
ggplot(hotels, aes(stars, price_EUR)) + geom_boxplot() # that is not okay, let's tell ggplot that star is a factor, so we will handle them as
# categories
ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot()
str(hotels)

## facet
## how to create multiple plots

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(~citytype) # big is on the left, small is on right, let's change that
## ggplot renders by the alphabet
setorder(hotels, citytype)
hotels[, citytype := factor(citytype, labels = c('small', 'big'))]
str(hotels)

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(pricecat ~ citytype)

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(~pricecat)

## stacked bar charts

ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar()
ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar(position = 'fill') # this is the distribution in the categories
ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar(position = 'dodge') # it puts them next to each other

## TODO rating

ggplot(hotels, aes(rating)) + geom_bar() # it is not good to use barchart for continuous variables, it should be used for categorial variables
ggplot(hotels, aes(rating)) + geom_histogram() # histogram is much better to plot continuous variables, it is good for numeric variables
ggplot(hotels, aes(rating)) + geom_histogram(binwidth = 0.1)

## discrete variable types: nominal, ordinal
## nominal: school name, gender, hair colour
## ordinal: city type (small vs big), grade 

## numeric, continuous variables: interval, ratio scale
## interval: financial year, date of birth (1982 and 1992 => diff 10 yrs), celsius degree
## nominal: age (10 of 20), distance, height => 100 km twice as much as 50 km

ggplot(hotels, aes(rating)) + geom_density()
ggplot(hotels, aes(rating, fill = pricecat)) + geom_density(alpha = 0.3) + theme_bw()

install.packages('ggthemes')
library(ggthemes)
p <- ggplot(hotels, aes(rating, fill = pricecat)) + geom_density(alpha = 0.25)
p + themes_economist() + scale_fill_economist()
p + themes_stata() + scale_fill_stata()

library(RColorBrewer)
p + scale_fill_brewer(palette = 'Greens')

## TODO plot a barplot on the number of hotels per city type
## TODO plot a histogram on the prices in EUR
## TODO plot a histogram on the prices in EUR split by city type
## TODO plot a boxplot on the prices in EUR split by city type
## TODO plot a scatterplot on the prices in EUR and the distance from city center
## TODO add a model to the previous plot
## TODO plot a boxplot on the prices in EUR split by cat(rating)

library(ggplot2)
library(ggthemes)

## TODO plot a barplot on the number of hotels per city type
ggplot(hotels[, .N, by = citytype], aes(N) + geom_bar(stats = 'identity')
ggplot(hotels, aes(citytype)) + geom_bar()

## TODO plot a histogram on the prices in EUR
ggplot(hotels, aes(price_EUR)) + geom_histogram()
## TODO plot a histogram on the prices in EUR split by city type
ggplot(hotels, aes(price_EUR)) + geom_histogram() + facet_wrap(~citytype)
## TODO plot a boxplot on the prices in EUR split by city type
ggplot(hotels, aes(price_EUR)) + geom_boxplot
## TODO plot a scatterplot on the prices in EUR and the distance from city center
ggplot(hotels, aes(dist_center_km, price_EUR)) + geom_point()
## TODO add a model to the previous plot
ggplot(hotels, aes(dist_center_km, price_EUR)) + geom_point() + geom_smooth(method = 'lm')
## TODO plot a boxplot on the prices in EUR split by cat(rating)
ggplot(hotels, aes(ratingcat, price_EUR)) + geom_boxplot()
