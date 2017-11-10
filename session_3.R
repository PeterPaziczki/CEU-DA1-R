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
hotels[, price_EUR := price_HUF / 310] # := the data assignment operator, we should prefer this one, because it is short and elegant
str(hotels)

## TODO categorize hotels into 3 bucket based on price => pricecat

pricecat <- table(cut(hotels$price_HUF, breaks = 3)) 
pricecat

## daroczi megoldas
hotels[, pricecat := cut(price_EUR, 3, dig.lab = 8)] ## created a factor with 3 levels, each level was 8 digits long
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

hotels[, citytype := cut(hotels, 2, labels = c('small', 'big'))] # with the cut command I tell that I want my data to be broken into buckets
## with number, in this case 2, I can tell, howm many bucket I need
hotels[, .N, by = citytype] # frequency table by citytype

hotels[, .N, by = list(citytype, pricecat)] # providing a list by multiple variabla names, using by for multiple variables
hotels[, .N, by = list(city = citytype, price = pricecat)] # I can rename the columns as well
## when using the by, we can rename the column names

## Let's take it further
hotels[, .N, by = list(city = citytype, price = pricecat)][order(price, city)] # we are ordering the obs by price and city
## don't forget, we can use names we have just created above, let's also mention, that ordering is not permanent, see more explanation a
## a few line below
## let's change the order, see what happens
hotels[, .N, by = list(city = citytype, price = pricecat)][order(city, price)] # we can change the order
## important note, the order command does not change the order in the data set, it is only a temporary thing for the time being plotted
## so the order in data set is untouched. If I overwrote the data table, then it would be permanent

## TODO number of hotels per price cat and city type => % in city tpye
price_per_type <- hotels[, .N, by = list(city = citytype, price = pricecat)][order(city, price)]
price_per_type
## TODO add a new column called P => % per city type
126 / (126 + 2759 + 215) * 100
price_per_type[, P := N / sum(N) * 100, by = city]
price_per_type
price_per_type[, P := round (N / sum(N) * 100,2), by = city] # rounding the percentages, having 2 decimal places
price_per_type

## TODO min, avg, max of price
hotels[, list(
  min_price = min(price_EUR),
  avg_price = mean(price_EUR),
  max_price = max(price_EUR)
), by = city]

## TODO rounding up to 2 decimal places
hotels[, list(
  min_price = round(min(price_EUR), 2),
  avg_price = round(mean(price_EUR), 2),
  max_price = round(max(price_EUR), 2)
), by = city]
## or for fun rounding up to cardinals
hotels[, list(
  min_price = round(min(price_EUR)),
  avg_price = round(mean(price_EUR)),
  max_price = round(max(price_EUR))
), by = city]

## TODO compute the average price, rating, stars per city in a new dataset
hotels[, .(price_avg = mean(price_EUR),
          rating_avg = mean(rating),
          stars_avg = mean(stars)), by = city]
## too many NAs, with na.rm = TRUE we exclude the missing values
hotels[, .(price_avg = mean(price_EUR),
           rating_avg = mean(rating, na.rm = TRUE),
           stars_avg = mean(stars, na.rm = TRUE)), by = city]

## check the same on rating and stars
hotels[, lapply(.SD, mean), by = city, .SDcols = c('price_EUR', 'rating', 'stars')] # lapply means that we we want take the mean of all the columns
## that are mentioned after .SDcols and want to compute the mean by city, .SD stands for subset of data.table
## https://campus.datacamp.com/courses/data-table-data-manipulation-r-tutorial/chapter-two-datatable-yeoman?ex=4 is a tutorial video
## about the lapply command, basically .SD holds the values of all columns and with lapply we can loop a function through all the
## columns in .SD except the one specified in "by"
## lapply returns a list
## .SDcols specifies the columns of the data set that are included in .SD
## Using .SDcols comes in handy if you have too many columns and you want to perform a particular operation on a subset of the 
## olumns (apart from the grouping variable columns). Using .SDcols allows you to apply a function to all rows of a data.table
?lapply

## TODO check the same on rating and stars
hotels[, lapply(.SD, mean), by = city, .SDcols = c('price_EUR', 'rating', 'stars')] # there are a lot of NAs
hotels[, lapply(.SD, mean, na.rm = TRUE), by = city, .SDcols = c('price_EUR', 'rating', 'stars')] # excluding NAs

## save data for later use
write.csv(hotels, 'hotels-with-two-new-factors.csv')

## =====================================================================================================================

## load new data
hotels <- read.csv('http://bit.ly/CEU-R-hotels-2017-v2')
hotels <- data.table(hotels) # storing as a data.table object
setDT(hotels)
?setDT

## read.csv => fread - we are doing the same as above but faster
hotels <- fread('http://bit.ly/CEU-R-hotels-2017-v2')

## the R implementation of the Grammar of Graphics
library(ggplot2)

## barplot with ggplot: you specify a dataset, then define an aesthetic and geom
ggplot(hotels, aes(x = pricecat)) + geom_bar() # aes defines what and geom defines how I want show, we are combining layers with +
ggplot(hotels, aes(x = pricecat)) + geom_bar() + theme_bw() # background is white instead of gray
ggplot(hotels, aes(x = pricecat)) + geom_bar(colour = 'blue', fill = 'orange') + theme_bw() # the bar has a blue border and
## is filled with orange

## ggplot returns ggplot objects which we can stote for future use
p <- ggplot(hotels, aes(x = pricecat)) + geom_bar() # i am loading it to a variable
p
p + theme_bw() # I can add layers to the object
p + theme_bw() + theme(legend.position = "top")

## coordinate transformations
library(scales)
p + scale_y_log10() # now i have the y axe on a log scale
p + scale_y_sqrt() # or square
p + scale_y_reverse() # I can mirror it a way that the y axis shows in the opposite direction
p + coord_flip() # I can even rotate the chart with 90 degrees

## geoms => scatter
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() # it is a scatterplot, but does not make sense, too many points overlapping each other
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point(alpha = 0.1) # to understand the density we can play with opacity
ggplot(hotels, aes(x = price_EUR, rating)) + geom_hex() # plots hexagons with opcaity changing
ggplot(hotels, aes(x = price_EUR, rating)) + geom_hex() + theme(legend.position = "top") # I can change the position of the legend
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() + geom_smooth() # gives/drwas a smooth curve over the observations
ggplot(hotels, aes(x = price_EUR, rating)) + geom_point() + geom_smooth(method = 'lm') # you can see the confidence interval, that is the
## grey thing around the line, and it draws a line instead of a smooth curve

## install.packages('hexbin')
## ?geom_hex

## TODO preice_EUR + stars
str(hotels)
ggplot(hotels, aes(stars, price_EUR)) + geom_boxplot() # that is not okay, let's tell ggplot that star is a factor, so we will handle them as
# categories, but it does not change the variable in the data set, it only tells ggplot to handle stars as factor variable
ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() # now there will be as many bars as many star variable there are
str(hotels)

## facet
## how to create multiple plots, breaking down to subplots

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(~citytype) # big is on the left, small is on right, let's change that
## ggplot renders by the alphabet / fix ordering
setorder(hotels, citytype)
hotels[, citytype := factor(citytype, levels = c('small', 'big'))] # something is nor right here, the text small is on the left now, but
## the plots did not change .... it has to be sorted out
str(hotels)

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(pricecat ~ citytype) # wraps a 1D sequence of panels into 2D

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_grid(pricecat ~ citytype) # forms a matrix of panels

ggplot(hotels, aes(factor(stars), price_EUR)) + geom_boxplot() + facet_wrap(~pricecat)

## stacked bar charts / clustered bar charts

ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar() # pricecat will be mapped to the x axis and wilb be split by citytype
ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar(position = 'fill') # this is the distribution in the categories, it gives
## the proportion, the percentages
ggplot(hotels, aes(pricecat, fill = citytype)) + geom_bar(position = 'dodge') # it puts them next to each other, it groups them, it
## creates a grouped bar chart

## TODO rating

ggplot(hotels, aes(rating)) + geom_bar() # it is not good to use barchart for continuous variables, it should be used for categorial variables
ggplot(hotels, aes(rating)) + geom_histogram() # histogram is much better to plot continuous variables, it is good for numeric variables
ggplot(hotels, aes(rating)) + geom_histogram(binwidth = 1)
ggplot(hotels, aes(rating)) + geom_histogram(binwidth = 0.1) # playing with bindwidth, changing the resolution

## discrete variable types: nominal, ordinal
## nominal: school name, gender, hair colour
## ordinal: city type (small vs big), grade 

## numeric, continuous variables: interval, ratio scale
## interval: financial year, date of birth (1982 and 1992 => diff 10 yrs), celsius degree
## nominal: age (10 of 20), distance, height => 100 km twice as much as 50 km

ggplot(hotels, aes(rating)) + geom_density()
ggplot(hotels, aes(rating, fill = pricecat)) + geom_density(alpha = 0.3) + theme_bw() # plotting multiple density charts by pricecat
ggplot(hotels, aes(rating, fill = citytype)) + geom_density(alpha = 0.3) + theme_bw()

## Themes
install.packages('ggthemes')
library(ggthemes)
p <- ggplot(hotels, aes(rating, fill = pricecat)) + geom_density(alpha = 0.25)
p
p + theme_economist() + scale_fill_economist()
p + theme_stata() + scale_fill_stata()
p + theme_excel() + scale_fill_excel()
p + theme_wsj() + scale_fill_wsj('colors6', '')
p + theme_gdocs() + scale_fill_gdocs()

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
ggplot(hotels, aes(citytype)) + geom_bar()
## more difficult approach with custom summary
ggplot(hotels[, .N, by = citytype], aes(citytype, y = N)) + geom_bar(stat = 'identity')
## geom_bar uses stat_count by default, meaning it counts the number of cases at each x position
## but stat_identity means it leaves the data as is

## TODO plot a histogram on the prices in EUR
ggplot(hotels, aes(price_EUR)) + geom_histogram()

## TODO plot a histogram on the prices in EUR split by city type
ggplot(hotels, aes(price_EUR)) + geom_histogram() + facet_wrap(~citytype)

## TODO plot a boxplot on the prices in EUR split by city type
ggplot(hotels, aes(citytype, price_EUR)) + geom_boxplot()

## TODO plot a scatterplot on the prices in EUR and the distance from city center
ggplot(hotels, aes(dist_center_km, price_EUR)) + geom_point()

## TODO add a model to the previous plot
ggplot(hotels, aes(dist_center_km, price_EUR)) + geom_point() + geom_smooth(method = 'lm')

## TODO plot a boxplot on the prices in EUR split by cat(rating)
hotels[, ratingcat := cut(rating, 5)]
ggplot(hotels, aes(ratingcat, price_EUR)) + geom_boxplot() # ratingcat is on x, and price_EUR will be on y axis

## datacamp week 5 assignment - last task: a complex example
## Compute the average price per cut quality and store the resulting data.table object in the dsummary variable.
## The column names should be cut and price.
dsummary <- dt[,list(price = mean(price)), by = cut]

## Reorder the factor levels of cut in this new object by the related price, so that the cut quality categories
## in the plot (see next item) will be ordered by the average price.
dsummary[, cut := factor(cut, levels = dsummary[order(price), cut])]

ggplot(dsummary, aes(cut,price)) + geom_bar(stat = 'identity')
