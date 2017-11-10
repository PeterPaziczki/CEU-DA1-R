set.seed(42)
x <- runif(15, min = 0, max = 5)
str(x)
plot(x)
?hist
hist(x, main = 'Histogram of 15 random numbers')
x <- seq(0, 4*pi, by = 0.1)
x
y <- cos(x)
plot(x, y, type = 'l', col = "red")
?curve
curve(cos(x), from = 0, to = 4*pi, col = 'red')

class(x)
class(y)
class(col)
class(type)