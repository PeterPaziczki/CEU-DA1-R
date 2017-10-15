h <- c(174, 170, 160) # creating a vector
w <- c(90, 80, 70) # creating another vector

cor(h, w) # calculating correlation, it is 0.97...
plot(h, w)

df <- data.frame(weight = w, height = h) # creating a dataframe object with column names, every variable is a column
df

str(df)

# df [rows, colSums] it is a data frame, a matrix, the number of variables is equal to the number of columns, 1st column is weight, 2nd is height
df[1, 1] # 1. sor 1. oszlop eleme, vagyis 90
df[2, 2] # 2. sor 2. oszlop eleme, vagyis 170
df[3,1] # ez pedig 70
df[4,1] # ez N/A mert nincs 4. sor
df[, 1] # a teljes 1. oszlop
df[1, ] # a teljes 1. sor
df$weight # creating a vector
df$weight[2] # filtering for the second item in the vector
df[2, 1]
cor(df) # calculating correlation among vectors in a data frame / matrix
plot(df)

## TODO BMI
## body mess index = weight / height(m)^2

df$bmi <- df$weight / (df$height / 100) ^ 2 # it will ad a new column to the dataframe, bmi is not a new variable, it is just a column
## we created the df$bmi vector
df ## now there is the new bmi column
summary(df$bmi) # a simple box plot like summary of the vector
df <- read.csv('http://bit.ly/CEU-R-heights')
df
str(df)

df$heightCm <- df$heightIn * 2.54 # adding a new column to the dataset, also created a new a vector: df$heightCm, we converted inch to cm
df$weightKg <- df$weightLb * 0.4535923 # did the same thing just with weight
df$bmi <- df$weightKg / (df$heightCm / 100) ^ 2 # calculated bmi again, and created a column and a vector for it
head(df) # it gives back the first rows of the data frame
head(df, 3) # gives back the first 3 rows of the data
str(df) # we see that the DF has 237 rows

## TODO analyze bmi

mean(df[, 8]) # bmi is the 8th column, so let's take the mean of the 8th column
median(df[, 8])
min(df[, 8])
summary(df$bmi) # a summary paranccsal lehet gyorsan analizalni, ez ad min-t es max-ot, mediant, ill 1st es 3rd percentile-t

min(df$bmi) # nem csak a dataframe valahanyadik oszlopara tudok utalmi, hanem nev szerint is hivhatom a vektort
max(df$bmi)
range(df$bmi) # gives the min and max values
diff(range(df$bmi)) # calculates the differenc of max value from min value
sum(df$weightKg) # a vektor osszes elemet osszeadja, veszi a szummat lenyegeben
length(df$weightKg) # megadja az elemek szamat
?length
length(df$bmi)
dim(df) # it gives the size of the data frame, number of rows x number of columns
nrow(df) # gives the number of rows
ncol(df) # gives the number of columns

## erdekesseg, torlok ket oszlopot, azt a kettot, amelyikbil a bmi-t szamoltam, de a bmi-t igy is megkapom, annak ertekei nem torlodnek
str(df)
df$weightKg <- NULL # igy torlok egy oszlopot a data-bol
df$heightCm <- NULL
str(df)

## lets do some visualization
hist(df$bmi)
abline(v=c(18, 25), col = 'red') # v stands for vertical, how to draw a vertical line anywhere
abline(h=c(18, 25), col = 'red') # how to draw a horizontal line anywhere
plot(density(df$bmi)) # calling a density function on bmi
boxplot(df$bmi)
boxplot(bmi ~ sex, df) # gender-tol fuggoen lett szetbontva, kulon fiuk es kulon a lanyok szerint, tehat a bmi a gender fuggvenyeben vizsgalhato
## a ~ fuggvennyel azt csinalom, hogy a ~ jel elott levo ertekeket szetbontom a ~ utan levo oszlop ertekei alapjan, vagyis ha a ~ utan gender van,
## akkor ket lehetseges erteket vehet fel. de ha olyan adat lenne ott, ami 4 erteket vehet fel, akkor 4 kulonbozo reszre bontana

install.packages('beanplot') # installing the package
library(beanplot) # after installing i need to load the package, with this command i am doing that, i am loading it to the library
beanplot (bmi ~ sex, df) # beanplot is better than boxplot in a sense

## a lenti ket megjelenites kozti kulonbseget akartuk megmutatni, a lenyege az, hogy a beanplot egy eleg powerful vizualizacios mod
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
table(df$sex) # it is a frequency chart, it tells you how many female and male are in the set
table(df$ageYear) # frequency chart on ages
barplot(table(df$sex)) # same just visualized with a barchart
dotchart(table(df$sex))
dotchart(table(df$sex), xlim = c(0, 150)) # i can set the min and max values that are on the x angle

pairs(df) # quick way to check different variables, another tool to visualize the dispersion / distribution among the variables of the dataset

install.packages('GGally')
library(GGally)
ggpairs(df) # bitang jo cucc vizualizaciohoz, korrelaciot szamol az elemek kozott !!!

install.packages('pairsD3')
library(pairsD3) # R -> JSON -> JS, a JSON most csak annyit jelent, hogy egy jason jellegu fajlban tarolja az adatot
pairsD3(df)

## intro to stats

## traditional statitstical tests
t.test(heightIn ~ sex, data = df) # t.test futtatasa, p-value az mondja meg, hogy a kulonbseg szignifikans-e vagy sem, ebben az esetben nem jelentos
## vagyis a magassagban nincs jelentos kulonbseg a nemek fuggvenyeben
t.test(weightLb ~ sex, data = df)
t.test(bmi ~ sex, data = df) # p-value = 0.7, ami azt jelenti, hogy a gender-tol fuggoen komoly elteres van a bmi index-ben
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
library(beanplot)
beanplot(df$price_HUF)
boxplot(df$price_HUF)

table(hotels$city) # frequency table

table(hotels$price_HUF) # szolt hogy tulleptuk a megjelenitheto elemek szamat, az alabbit tehetem:
table(cut(hotels$price_HUF, breaks = 10)) # igy mar maskepp nez ki, a cut es breaks parancsok segitsegevel meg tudtam adni, hogy hany bucket-ra
## ossza szet az osszes adatot
pie(table(cut(hotels$price_HUF, breaks = 10))) # persze ezt siman tudom pl pie chart-ban abrazolni
pie(table(cut(hotels$price_HUF, breaks = 10, dig.lab = 8))) # with dig.lab = 8 i can maximize the number of digits used to describe the number

dotchart(table(cut(hotels$price_HUF,
                   breaks = 20,
                   dig.lab = 8)))

## creating a new variable
hotels$price_HUF_cat <- cut(hotels$price_HUF, breaks = 20,dig.lab = 8)
tab_price_HUF_cat <- table(hotels$price_HUF_cat)
dotchart(tab_price_HUF_cat)

str(hotels)
max(hotels$price_HUF) # it tells what is the max value of the vector
which.max(hotels$price_HUF) # it tells me which values holds the max value
hotels[1374,] # plotting everything about that specific value
hotels[which.max(hotels$price_HUF),] # the thing we did above we can do with one line, it gives everything about the observatin that has the
## highest price in HUF
 
x <- which.max(hotels$price_HUF)
hotels[x,]

hotels[which.min(hotels$price_HUF),] # the cheapest room

## TODO list all places where we pay more than 100,000 HUF

pricey <- hotels[hotels$price_HUF > 100000,] # we generated a vector of true and false values
str(pricey) # we have 323 observations left
table(pricey$city) # frequency table about the cities
## or i can ...
which(hotels$price_HUF > 100000) # azokat az emeleket adja meg, amelyben az ar nagyobb mint 100,000

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
data.table(hotels) # gives you the 1st and last 5 rows
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

## daroczi megoldasai:
good <- hotels[price_HUF < 10000 & rating > 4, city]
str(good)
hotels[price_HUF < 10000 & rating > 4, length(city)] ## number of cases, where it is true (a city can occur more times)
hotels[price_HUF < 10000 & rating > 4, unique(city)] ## cities only, one city can occur more times in the above mentioned row, but only once in this
hotels[price_HUF < 10000 & rating > 4, length(unique(city))] # number of unique cities
## df[row, columns] 4, "city"
## dt[i, j] j => R expression (R expression is an R command)

hotels[price_HUF < 10000 & rating > 4 & city == 'Budapest, Hungary'] # konkret varosra tudok keresni

hotels[price_HUF < 10000, .N] # number of rows where it is true
hotels[price_HUF < 10000, mean(stars)] # mean of stars where price is less than 10000
## there are a few hotels that NAs
hotels[price_HUF > 250000, mean(stars, na.rm = TRUE)] # na.rm = TRUE - if the missing values (the NAs) should be removed before evaluating the values
## whit this I am telling R to remove the items where there is a missing value
hotels[price_HUF > 250000, mean(rating, na.rm = TRUE)]
hotels[price_HUF > 250000, table(stars)]

## dt[i, j, by = ...]
hotels[price_HUF > 250000, mean(stars, na.rm = TRUE), by = city] # varosok szerint adjuk meg a csillagok atlagos szamat
str(hotels)
hotels[price_HUF > 250000 & city == 'Naples, Italy']
?by

## TODO compute avg price per number of stars
hotels[, mean(price_HUF, na.rm = TRUE), by = stars]
## adjunk valami nevet az oszlop-nak:
hotels[, list(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
## hogy ne kelljen a list szot kiirni, eleg egy pontot tenni a helyere
hotels[, .(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
# rendezzuk szepen sorba az elemeket
price_per_stars <- hotels[, list(avg_price = mean(price_HUF, na.rm = TRUE)), by = stars]
price_per_stars
setorder(price_per_stars, stars)
price_per_stars # now it is ordered by the number of stars
## we can switch the order by using: -
setorder(price_per_stars, -stars)
price_per_stars

## gyanus, hogy 1.5 csillag eseten valamiert nagyon magas az atlag
hotels[stars == 1.5 & price_HUF > 100000] # szilveszterkor, pontosabban az ev utolso napjan ervenyes arak vannak itt
## lehet, hogy csak ezek repter melletti helyek es ezert ilyen dragak ott a szallasok

## 
hotels[, list(avg_prive = mean(price_HUF),
              avg_stars = mean(stars, na.rm = TRUE),
              .N, ## number of rows where it is true
              with5stars = sum(stars == 5) ## stars == 5 it will return true or false values
              ), by = city]

sum(c(1, 2, 5, 5, 3) == 5)
