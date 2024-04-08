x <- rnorm(50)
x
y <- x+rnorm(50, 50, .1)
y
cor(x,y)

set.seed(1303)
rnorm(50)

set.seed(3)
y <- rnorm(100)
mean(y)
var(y)
sum((y - mean(y))^2) / (length(y) - 1)
sqrt(var(y)) == sd(y)

x <- rnorm(100)
y <- rnorm(100)
plot(x, y)
plot(x, y,
     xlab = "x-axis",
     ylab = "y-axis",
     main = "Plot of Y against X"
)
dev.off()

# 3D graphing
x <- seq(-pi , pi , length = 50)
y <- x
f <- outer(x, y, function(x, y) cos(y) / (1 + x^2))
?outer
contour(x, y, f)
contour(x, y, f, nlevels = 45, add = T)
fa <- (f - t(f)) / 2
contour(x, y, fa, nlevels = 15)

image(x, y, fa)
persp(x, y, fa)
persp(x, y, fa , theta = 30)
persp(x, y, fa , theta = 30, phi = 20)
persp(x, y, fa , theta = 30, phi = 70)
persp(x, y, fa , theta = 30, phi = 40)

# indexing
A <- matrix(1:16, 4, 4)
A
A[2,3]
# select multiple rows and multiple columns using vectors
A[c(1,3), c(2,4)]
A[1:3, 2:4]
A[1:2, ]
# negative excludes
A[-c(1,2),]
dim(A)

# reading data
Advertising <- read.csv("data/Advertising.csv")
Advertising |> head()
Advertising <- na.omit(Advertising)
attach(Advertising)
plot(TV, sales, col = "red", varwidth = T, horizontal = T)
pairs(Advertising)
pairs(~ TV + radio + newspaper + sales, Advertising)
summary(Advertising)
detach(Advertising)