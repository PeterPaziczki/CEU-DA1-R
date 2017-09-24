h <- c(174, 170, 160) # vector
w <- c(90, 80, 70)

cor(h, w)
plot(h, w)

df <- data.frame(weight = w, height = h) # creating a dataframe object
df

str(df)

# df [rows, colSums] it is a data frame, a matrix
df[1, 1] # 1. sor 1. oszlop eleme
df[2, 2] # 2. sor 2. oszlopban lévő elem
df[, 1] # a teljes 1. sor
df[1, ] # a teljes 1. oszlop
df$weight # creating a vector
df$weight[2] # filtering for the second item in the vector
df[2, 1]
cor(df)
plot(df)

## TODO BMI
## body mess index = weight / height(m)^2

df$bmi <- df$weight / (df$height / 100) ^ 2 # it will ad a new column to the dataframe, bmi is not a new variable, it is just a column
df
summary(df$bmi)
df <- read.csv('http://bit.ly/CEU-R-heights')
df
str(df)

df$heightCm <- df$heightIn * 2.54
df$weightKg <- df$weightLb * 0.4535923
df$bmi <- df$weightKg / (df$heightCm / 100) ^ 2
head(df)
head(df, 3) # gives back the first 3 row
str(df)

## TODO analyze bmi

mean(df[, 8])
median(df[, 8])
min(df[, 8])
summary(df$bmi) # a summary paranccsal lehet gyorsan analizálni, ez ad min-t, medinánt, stb

min(df$bmi)
max(df$bmi)
range(df$bmi)
diff(range(df$bmi))
sum(df$weightKg)
length(df$bmi)
dim(df)
nrow(df)
ncol(df)

## érdekesség, töröltem a két oszlopot, de a bmi-t így is megkaptam
str(df)
df$weightKg <- NULL
df$heightCm <- NULL
str(df)

## lets do some visualization
hist(df$bmi)
abline(v=c(18, 25), col = 'red') # v stands for vertical
plot(density(df$bmi)) # calling a density function on bmi
boxplot(df$bmi)
boxplot(bmi ~ sex, df) # gender-től függően lett szétbontva, külön fiúknak, külön lányoknak
## a ~ függvénnyel azt csinálom, hogy a ~ jel előtt lévő értékeket szétbontom a ~ után lévő oszlop értékei alapján, vagyis ha a ~ után gender van,
## akkor két lehetséges értéket vehet fel. de ha olyan adat lenne ott, ami 4 értéket vehet fel, akkor 4 különböző részre bontaná

install.packages('beanplot') # installing the package
library(beanplot) # after installing i need to load the package, with this command i am doing that, i am loading it to the library
beanplot (bmi ~ sex, df) # beanplot is better than boxplot in a sence

## a lenti két megjelenítés közti különbséget akartuk megmutatni, a lényege az, hogy a beanplot egy elég powerful vizualizációs mód
boxplot(
  rbeta(1e3, 0.1, 0.1),
  runif(1e3) * 2 - 0.5,
  rnorm(1e3, .5, 0.75)
)

beanplot(
  rbeta(1e3, 0.1, 0.1),
  runif(1e3) * 2 - 0.5,
  rnorm(1e3, .5, 0.75)
)

## violin plot

pie(table(df$sex)) # frequency chart on gender
barplot(table(df$sex)) # same just visualized with a barchart
dotchart(table(df$sex))
dotchart(table(df$sex), xlim = c(0, 150))

pairs(df) # quick way to check different variables

install.packages('GGally')
library(GGally)
ggpairs(df) # bitang jó cucc vizualizációhoz, korrelációt is számol

install.packages('pairsD3')
library(pairsD3) # R -> JSON -> JS
pairsD3(df)

## intro to stats

## traditional statitstical tests
t.test(heightIn ~ sex, data = df) # t.test futtatása, p-value az mondja meg, hogy a különbség szignifikáns-e vagy sem, ebben az esetben nem jelentős
## vagyis a magasság nincs jelentős különbség a nemek függvényében
t.test(weightLb ~ sex, data = df)
t.test(bmi ~ sex, data = df) # p-value = 0.7, ami azt jelenti, hogy a gender-től függően komoly eltérés van a bmi index-ben
summary(aov(heightIn ~ sex, data = df))
summary(aov(weightLb ~ sex, data = df))
summary(aov(bmi ~ sex, data = df))
TukeyHSD(aov(heightIn ~ sex, data = df))
TukeyHSD(aov(weightLb ~ sex, data = df))

## new data set

## hotels.com

df <- read.csv('http://bit.ly/CEU-R-hotels-2017')
df
str(df)
hotels <- df
df

## price -> plot, summary, frequency table
summary(df)
summary(hotels$price_HUF)
barplot(df$price_HUF)
hist(hotels$price_HUF)
beanplot(df$price_HUF)
boxplot(df$price_HUF)

table(hotels$city)

table(hotels$price_HUF) # szólt, hogy túlléptük a mejeleníthető elemek számát, ílyenkor az alábbit tehetem
table(cut(hotels$price_HUF, breaks = 10)) # így már másképp néz ki, a cut és breaks parancsok segítségével meg tudtam adni, hogy hány bucket-ra
## ossza szét az összes adatot
pie(table(cut(hotels$price_HUF, breaks = 10))) # persze ezt simán tudom pl pie chart-ban ábrázolni

dotchart(table(cut(hotels$price_HUF,
                   breaks = 20,
                   dig.lab = 8)))

## creating a new variable
hotels$price_HUF_cat <- cut(hotels$price_HUF, breaks = 20,dig.lab = 8)
tab_price_HUF_cat <- table(hotels$price_HUF_cat)
dotchart(tab_price_HUF_cat)

str(hotels)
max(hotels$price_HUF)
which.max(hotels$price_HUF)
hotels[1374,]
hotels[which.max(hotels$price_HUF),]
 
x <- which.max(hotels$price_HUF)
hotels[x,]

hotels[which.min(hotels$price_HUF),]

## TODO list all places where pay more than 100,000 HUF

pricey <- hotels[hotels$price_HUF > 100000,] # we generated a vector of true and false values
str(pricey)
table(pricey$city)
## or i can ...
which(hotels$price_HUF > 100000) # azokat az emeleket adja meg, amelyben az ár nagyobb mint 100,000

## list the places where you would pay more than 100,000 & rating < 3
## option 1
pricey <- hotels[hotels$price_HUF > 100000,]
pricey[pricey$rating < 3,]
pricey$rating # there are a few NAs ...
pricey[which(pricey$rating < 3),]
## option 2
hotels[which(hotels$price_HUF > 100000 & hotels$rating < 3),]

## unjuk, hogy 
install.packages('data.table')
library(data.table)
hotels <- data.table(hotels)
str(hotels)

## dt[i] <-> df[i,]
hotels[1]
hotels[price_HUF > 100000]
hotels[price_HUF > 100000 & rating < 3]

## TODO hotels: price < 10000 & rating > 4
## TODO list the cities of these

goodchoice <- hotels[price_HUF < 10000 & rating > 4]
str(goodchoice)
goodchoice$city

## darczi megoldásai:
good <- hotels[price_HUF < 10000 & rating > 4, city]
str(good)
hotels[price_HUF < 10000 & rating > 4, length(city)] ## number of cases, where it is true (a city can occur more times)
hotels[price_HUF < 10000 & rating > 4, unique(city)] ## cities only, one city can occur more times in the above mentioned row, but only once in this
hotels[price_HUF < 10000 & rating > 4, length(unique(city))] # number of unique cities
## df[row, columns] 4, "city"
## dt[i, j] j => R expression (R expression is an R command)

hotels[price_HUF < 10000 & rating > 4 & city == 'Budapest, Hungary'] # konkrét városra tudok keresni

hotels[price_HUF < 10000, .N] # number of rows where is is true
hotels[price_HUF < 10000, mean(stars)] # mean of stars where price is less than 10000
## there are a few hotels that NAs
hotels[price_HUF > 250000, mean(stars, na.rm = TRUE)] # na.rm = TRUE - if the missing values (the NAs) should be removed before evaluating the values
## whit this I am telling R to remove the items where there is a missing value
hotels[price_HUF > 250000, mean(rating, na.rm = TRUE)]
hotels[price_HUF > 250000, table(stars)]

## dt[i, j, by = ...]
hotels[price_HUF > 250000, mean(stars, na.rm = TRUE), by = city]
str(hotels)
hotels[price_HUF > 250000 & city == 'Naples, Italy']
?by

## TODO compute avg price per number of stars
hotels[, mean(price_HUF, na.rm = TRUE), by = stars]
## adjunk valami nevet az oszlop-nak:
hotels[, list(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
## hogy ne kelljen a list szót kiírni, elég egy pontot tenni a helyére
hotels[, .(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
# rendezzük szépen sorba az elemeket
price_per_stars <- hotels[, list(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
price_per_stars
setorder(price_per_stars, stars)
price_per_stars # now it is ordered by the number of stars
## we can switch the order by using: -
setorder(price_per_stars, -stars)
price_per_stars

## gyanús, hogy 1.5 csillag esetén valamiért nagyon magas az átlag
hotels[stars == 1.5 & price_HUF > 100000] # szilveszterkor, pontosabban az év utolsó napján érvényes árak vannak itt
## lehet, hogy csak ezek reptér melletti helyek és ezért ilyen drágák ott a szállások

## 
hotels[, list(avg_prive = mean(price_HUF),
              avg_stars = mean(stars, na.rm = TRUE),
              .N, ## number of rows where it is true
              with5stars = sum(stars == 5) ## stars == 5 it will return true or false values
              ), by = city]

sum(c(1, 2, 5, 5, 3) == 5)
