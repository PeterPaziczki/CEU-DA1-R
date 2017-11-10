rm(list=ls())
library(data.table)
hotels <- fread('http://bit.ly/CEU-R-hotels-2017-v2', encoding = 'UTF-8')
str(hotels)

## creating a new variable to fix city column (it won't be part of the exam)
x <- 'Budapest, Hungary'
x # it is a string
## we will learn how to parse things from a string
## let's split the string -> split, regexp
strsplit(x, ',') # first argument is the string I would split, the ',' is the part where I split the string and drop the comma as well
?strsplit
str(strsplit(x, ',')) # it returns a list with one element, that element is a character vector, where we have two strings

## for matrices we use [row, col]
## or list[i] it returns a named list
## or list[[i]] -> content of the list element
strsplit(x, ',')[[1]] # it will filter for the first element and it returns two strings, a character vector
strsplit(x, ',')[[1]][1] # filtering for only Budapest
strsplit(x, ', ')[[1]][2] # filtering for Hungary, and nor I added a space after the comma, so there will be no space before Hungary

## TODO create a country column using the above approach
str(hotels)
## hotels$country <- strsplit(hotels$city, ', ')[[1]][2] it is WRONG, it will add Netherlands to all the observations
str(hotels)
table(hotels$country)

hotels[, country := strsplit(city, ', ')[[1]][2], by = city] # it is GOOD
table(hotels$country)

## regular expressions
## . -> any char
## * -> any number of
## .* -> any number of any char
sub('.*, ','', x)
## gsub
?sub

hotels[, country := sub('.*,','', city), by = city]
str(hotels$country)
table(hotels$country)

## TODO create a now column/rename: citycountry w/ city
## TODO fix city column (by removing country)

## my solution: hotels$citycountry <- hotels$city

## Daroczi solution
hotels[, citycountry := city]
hotels[, city := strsplit(city, ', ')[[1]][1], by = city]
str(hotels)
## there is another solution, which is about removing everything after comma

## TODO count the number of cities with hotels in Hungary
table(hotels$country)
table(hotels$city)
str(hotels[country == ' Hungary', .N, by = city]) # there is a space before the country name
str(hotels[1])
hotels[country == ' Hungary', .N, by = city]
hotels[country == ' Hungary', .N, by = city][, .N]
hotels[country == ' Hungary', length(unique(city))]

## TODO count the number of cities with hotels in Germany
hotels[country == ' Germany', length(unique(city))]
hotels[country == ' Germany', .N, by = city][, .N]
hotels[country == ' Germany', .N, by = city]

## TODO count the average number of hotels per city
hotels[, .N, by = city][, mean(N)]
## mean(hotels[, .N, by = city]$N) it is another solution

## TODO count the average number of hotels per city per country
hotels_per_city <- hotels[, .N, by = list(country, city)]
hotels_per_city
hotels_per_city[, list(avg_n_hotels = mean(N)), by = country]
## there is a one line solution
hotels[, .N, by = list(country, city)][, mean(N), by = country]

## TODO compute the percentage of national hotels per city
hotels_per_city[order(country)] # just reordering the rows
hotels_per_city[, P := round(N / sum(N) * 100,2), by = country] # megadja hogy adott varosban talalhato hotelek
## hany szazalekat adjak az orszagban talalhato hoteleknek
hotels_per_city

## TODO compute the avg price (HUF) in Hungary for hotels w/ rating > 4.5
hotels[country == ' Hungary' & rating > 4.5][, mean(price_HUF)]
## Daroczi solution
hotels[country == ' Hungary' & rating > 4.5, mean(price_HUF)] # returns the same solution

## TODO histogram (ggplot) on prices of Hungarian hotels /w rating > 4.5
library(ggplot2)
ggplot(hotels[country == ' Hungary' & rating > 4.5], aes(price_HUF)) + geom_histogram()
## it was not nice enough, we should finetune it
library(scales)
ggplot(hotels[country == ' Hungary' & rating > 4.5], aes(price_HUF)) + 
  geom_histogram(binwidth = 25000) + xlab('') + ylab ('') + 
  ggtitle('Number of hotels', subtitle = 'Budapest, Hungary') + 
  scale_x_continuous(labels = dollar_format(suffix = 'Ft', prefix = ''))

## EXAM - 29th Oct 9am
## 100 minutes long exam is planned (60 mintutes should be fine)
## data.table and ggplot will be in the focus of the exam

## merging external data into our data

install.packages('XML') # we need quote here, because R does not know yet that it is a object
library(XML) # now R knows it is an object to load, no need to have quote

?readHTMLTable

gdp <- readHTMLTable(readLines('https://bit.ly/CEU-R-gdp'), which = 3)
str(readHTMLTable(readLines('https://bit.ly/CEU-R-gdp'))) # we can see there are 8 different tables, we need the 3rd one
head(gdp)
str(gdp)
## I want to merge it to my data ... how to do that?
## in country/territory table there are some stran
gdp$`Country/Territory`

gdp <- data.table(gdp)
gdp[, country := iconv(`Country/Territory`, to = 'ASCII', sub = '')] # i am getting rid of special characters, i am cleaning the data
## it is very important, we need to use back quote (alt gr + 7)
head(gdp)
gdp[, gdp := as.numeric(sub(',', '', `Int$`))] # removing comma and replacing it with space

hotels <- fread('http://bit.ly/CEU-R-hotels-2017-v2', encoding = 'UTF-8')
hotels[, country := strsplit(city, ', ')[[1]][2], by = city]
hotels[, citycountry := city]
hotels[, city := strsplit(country, ', ')[[1]][1], by = citycountry]

str(hotels)
str(gdp)

countries <- hotels[, unique(country)]
countries %in% gdp$country

merge(hotels, gdp, by = 'country') # key
hotels[1]
str(merge(hotels, gdp, by = 'country'))
str(hotels)

hotels <- merge(hotels, gdp[, list(country, gdp)], by = 'country')
str(hotels)

## TODO compute avg price and avg GDP per capita per country
hotels[, list(price = mean(price_HUF), gdp = gdp), by = country]
## what is wrong here?
## mean(price_HUF) returned a number
## but gdp = gdp returned as many poitn as many observations we have, so we should have the first of the values or have the mean of it:
hotels[, list(price = mean(price_HUF), gdp = gdp[1]), by = country]
## or
country_stats <- hotels[, list(price = mean(price_HUF), gdp = mean(gdp)), by = country]


## TODO analyze the association
cor(country_stats[, list(price, gdp)])
ggplot(country_stats, aes(gdp, price)) + geom_point()
ggplot(country_stats, aes(gdp, price)) + geom_point() + geom_smooth()
ggplot(country_stats, aes(gdp, price)) + geom_point() + geom_smooth(method = 'lm')
ggplot(country_stats, aes(gdp, price)) + geom_text() # label is missing, let's add it
ggplot(country_stats, aes(gdp, price, label = country)) + geom_text() + geom_smooth() # instead of plotting point we can plot
## the labels as text on the plot
ggplot(country_stats, aes(gdp, price)) + geom_text(aes(label = country)) + geom_smooth()

hotels[, max(price_HUF), by = country][order(V1)]

## let's remove outliers -> where avg price is larger than 70k
cor(country_stats[price < 70000, list(price, gdp)])
ggplot(country_stats[price < 70000], aes(gdp, price)) + geom_text(aes(label = country)) + geom_smooth()

## geocoding

x <- 'Budapest, Hungary'
install.packages('ggmap')
library(ggmap)
geocode(x, source = 'dsk') # dsk = data science toolkit, they pay to google for having 2500 queries a day (it is 10k USD a year)
geocode('Central Europen University')
geocode(' Budapest, Eszek street 20')
?geocode

## hotels[, lon := geocode(citycountry, source = 'dsk')$lon] if we run it, it would take a while, because it should run 7200 times
## let's have a smarter approach, check the geocode 52 times

geocodes <- hotels[, .N, by = citycountry]
geocodes
## we could write a loop
for (i in 1:3){
  print(i)
}

for (i in 1:nrow(geocodes)){
  print(i)
}

for (i in 1:nrow(geocodes)){
  print(geocodes[i, citycountry])
}

for (i in 1:nrow(geocodes)){
 g <- geocode(geocodes[i, citycountry], source = 'dsk')
 geocodes[i, lon := g$lon]
 geocodes[i, lat := g$lat]
}
head(geocodes)

geocodes <- fread('tthp://bit.ly/CEU-R-hotels-2017-geocodes')

ggplot(geocodes, aes(lon, lat, size = N)) + geom_point()

worldmap <- map_data('world')
str(worldmap)

ggplot() + geom_polygon(data = worldmap, aes(long, lat, group = group)) +
  geom_point(data = geocodes, aes(lon, lat, size = N50))

europe <- get_map(location = 'Berlin', zoom = 4, source = 'stamen', maptype = 'toner') # there are different map types
## map types with terrain, routes, border, roads, ...
europe <- get_map(location = 'Berlin', zoom = 4, source = 'osm') ## open street map