1 + 3
2*3
str('pi')
str(letters)
LETTERS
letters
str(letters)
letters[10]
letters[99]
letters[c(1,10)]
c(1,10)
str(c(1,10))
3^4
## élkfélkélfd
3 + 6 # érdemes
3 + 6 ## érdemes 
?pi
?str

## square root of x
str(x) # defining x as a string
x = 2 # giving value to x
x ^ 0.5
sqrt(x)

## random numbers
runif(1)

## random numbers but with giving seed
set.seed(41) # setting the random number generator, meaning it would give the same random number on every computerif(1)

runif(5) # it generated 5 random numbers

round(runif(5))
?round

round(runif(5),2) # be tudom állítani, hogy hány tizedes jegyig akarom a random számokat kerekíteni
round(runif(5), digits = 3) # másképp is be tudom állítani, hogyan akarok kerekíteni

## TODO gen 1000 random numbers between 0 and 10
## digits = 2 => visualize

round(runif(1000, min = 0, max = 10), digits = 2)

## another solution for above mentioned task
x <- runif(1000, min = 0, max = 10)
x <- runif(1000)*10

x <- round(runif(1000, min = 0, max = 10))

plot(x)
hist(x)
boxplot(x)

f <- function(x) 2 * x + 1
# f <- function(x) {
# 2 * x + 1
# }

f(5)
f(pi)
f(1:5)
1:5

x < - 1:5
f(x)
plot(x, f(x))
f(1.5)
plot(x, f(x), type = 'l') # l = lowecase

1:25
x <- seq(1, 5*pi, by = 0.1) # meeg tudom mondani, hogy mekkora ugrásokkal haladjunk
plot(x, f(x))

x <- seq(5, 98, by = 0.25)
plot(x, f(x), type = 'l')
?seq

## TODO 1 period of sine
x <- seq(1, 5, by = 0.1) ## we generated a vector here
plot(x, sin(x), type = 'l')

## random walk example
runif(25) # generating random numbers 0 and 1
round(runif(25)) * 2 - 1 # a sequence to generate 1s and -1s

x <- round(runif(25)) * 2 - 1
cumsum (x)
plot(cumsum(x), type = 's') # random walk example ábrázolása

## data.frame
## we are gonna create two vectors and do some data analysis

h <- c(174, 170, 160) # we created the h vector
w <- c(90, 80, 70)
plot(h,w, xlab = 'Height', ylab = 'Weight') # I can name the x and y angles
cor (h,w) # correlation coeffitient
## result was 0.9707253, it means that they are highly correaled
lm(w ~ h) # let's predict w, it is formula notation, I want to predict w with the help of h

fit <- lm (w~h) # egy változóba töltöm, fit object
summary(fit) # ábrázolom
abline(fit, col = 'blue') # fits a linear

fit
-146.154 + 180 * 1.3462 # ezek szerint az én súlyom kb 96,162 kg kellene hogy legyen
predict (fit)
predict(fit, list(h= 180))
predict (fit, list(h = c(165, 174)))# i can create predictions for more numbers

## interesting example
predict(fit, list(h=c(56))) # it says the weight of a newborn will be -70kg, that is not realistic, what should we do?
## instead of linear we should use quadratic, exclude outliers, extreme values, need more data, range is important
## it is not always about finetuning or getting more data, sometimes you just need to consider basic things, like we mentioned above

## 20170924 a lentieket a quiz-hez csináltam

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
                 