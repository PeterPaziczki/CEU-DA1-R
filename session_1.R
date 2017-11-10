1 + 3
2*3
str('pi')
str(letters)
LETTERS
letters
str(letters)
letters[10] # this gives you the 10th letter
letters[99] # this would give you teh 99th letter, but that is not applicable
letters[c(1,10)] # this gives you more letter, the 1st and the 10th
c(1,10) # now we created a vector with two number in it, 1 and 10
str(c(1,10))
3^4
## csak egy egyeni style, hogy ha sor elején van a komment, akkor 2 #-t használunk
3 + 6 # ha sor vegen kommentelunk, akkor meg egy #-t hasznalunk
3 + 6 ## de hasznalhatnank 3 #t is, nem szamit
?pi
?str

## square root of x
str(x) # defining x as a string
x = 2 # giving value to x
x ^ 0.5
sqrt(x)

## random numbers between 0 and 1
runif(1)

## random numbers but with giving seed
set.seed(41) # setting the random number generator, meaning it would give the same random number on every computer

runif(5) # it generates 5 random numbers between 0 and 1

round(runif(5)) # generating 5 numbers between 0 and 1 then rounding them to 0 or 1
?round

round(runif(5),2) # be tudom allitani, hogy hany tizedes jegyig akarom a random szamokat kerekiteni
round(runif(5), digits = 3) # maskeppen is be tudom allitani, hogyan akarok kerekiteni, vagyis hany tizedes legyen

## TODO gen 1000 random numbers between 0 and 10
## digits = 2 => visualize

round(runif(1000, min = 0, max = 10), digits = 2) # itt most megadom, hogy 0 es 1 kozotti, hanem 0 es 10 kozotti szamokat generaljon 2 tizedesjeggyel

## another solutions for above mentioned task
x <- runif(1000, min = 0, max = 10) # 

x <- runif(1000)*10

x <- round(runif(1000, min = 0, max = 10))

plot(x)
hist(x)
boxplot(x)

f <- function(x) 2 * x + 1 # creating a function
# f <- function(x) {
# 2 * x + 1
# }

f(5) # using the function with different values
f(pi)
f(1:5) # i can give intervals to the function, so the function is calculated for each value in the interval given
1:5 # it is just a simple interval, its values are 1, 2, 3, 4 and 5 respectively

x <- 1:5 # loading the interval into a variable
f(x)
plot(x, f(x)) # az x tengelynek megadtam az ertekeket, ugy mint 1, 2, 3, 4 es 5, es ebben a tartomanyban kirajzolom a fuggvenyt
f(1.5)
plot(x, f(x), type = 'l') # l = lowecase - plotting but with drawing a continuous line

1:25 # 25 elemes intervallum
x <- seq(1, 5*pi, by = 0.1) # meeg tudom mondani, hogy mekkora ugrasokkal haladjunk
## fent letrehoztam egy szamsorozatot, 1-tol 5pi-ig, es 0.1-es ugrasokkal
plot(x, f(x)) # igy a fuggvenyt a teljes szamsorozatra kiszamolom es persze abrazolom
?seq

x <- seq(5, 98, by = 0.25) # ismet egy szamsorozat letrehozasa 5tol 98ig, 0.25-os ugrasokkal
plot(x, f(x), type = 'l') # az egesz abrazolasa, de nem pontokkal, hanem egy egyenes vonallal
?seq

## TODO 1 period of sine
x <- seq(1, 5, by = 0.1) ## we generated a vector here
plot(x, sin(x), type = 'l')

## random walk example
runif(25) # generating random numbers 0 and 1
round(runif(25)) * 2 - 1 # a sequence to generate 1s and -1s

x <- round(runif(25)) * 2 - 1
x
cumsum (x) # ez osszeadja az osszes elemet egyessevel haladva
?cumsum
plot(cumsum(x), type = 's') # plotting random walk example

## data.frame
## we are gonna create two vectors and do some data analysis

h <- c(174, 170, 160) # we created the h vector
w <- c(90, 80, 70) # creating vector w
plot(h,w, xlab = 'Height', ylab = 'Weight') # I can name the x and y angles
cor (h,w) # correlation coefficient, i can calculate a the correlation coefficient
## result was 0.9707253, it means that they are highly correlated
lm(w ~ h) # let's predict w, it is formula notation, I want to predict w with the help of h
?lm

fit <- lm (w~h) # egy valtozoba toltom, fitting an object
summary(fit) # abrazolom
abline(fit, col = 'blue') # fits a blue linear on the points

fit
-146.154 + 180 * 1.3462 # ezek szerint az en sulyom kb 96,162 kg kellene hogy legyen
predict (fit) # a megadott 174, 170 es 160 cm-es magassagok eseten megmondja, ha az egyenes milyen sulyokat ad ki, pontosan milyen pontok
## vannak az egyenesen, tehat ha van egyenesem, akkor adott x ertekek eseten megmondja, hogy az egyenesnek milyen ertekei vannak
?predict
predict(fit, list(h= 180)) # ha 180 cm lennek, akkor milyen suly esne az egyenesre
predict (fit, list(h = c(165, 174)))# i can create predictions for more numbers as well

## interesting example
predict(fit, list(h=c(56))) # it says the weight of a newborn will be -70kg, that is not realistic, what should we do?
## instead of linear we should use quadratic, exclude outliers, extreme values, need more data, range is important
## it is not always about finetuning or getting more data, sometimes you just need to consider basic things, like we mentioned above

## 20170924 a lentieket a quiz-hez csinÃ¡ltam

?max
set.seed(42)
runif(10, min = -1, max = 1)

set.seed(42)
x <- runif(10, min = -1, max = 1)
?mean
mean(x)

str('rounded')
rounded <- round(runif(10, min = -1, max = 1), digits = 2)
plot(x, f(x))
plot(rounded, f(rounded))
rounded          
