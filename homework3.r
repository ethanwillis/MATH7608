# Question 5: Plot the hyperbola x^2 - y^2/3=1

#setup and plot the right hand side of the parabola.
x <- seq(1, 5, by = .01)
#compute the y coordinates for this part of the parabola.
y.upper <- 1*sqrt(3*x^2 - 3)
y.lower <- -1*sqrt(3*x^2 - 3)
y.max <- max(y.upper)
y.min <- min(y.lower)
plot(c(-5, 5), c(y.min, y.max), type="n", xlab="x", ylab="y")
# setup and plot the left hand side of the parabola.
xLeft <- seq(-5, -1, by = .01)
#compute the y coordinates for this part of the parabola.
yLeft.upper <- 1*sqrt(3*xLeft^2 -3)
yLeft.lower <- -1*sqrt(3*xLeft^2-3)
yLeft.max <- max(yLeft.upper)
yLeft.min <- min(yLeft.lower)
plot(c(-5, 5), c(yLeft.min, yLeft.max), type="n", xlab="x", ylab="y")
# Draw the diagonal asymptotes for this parabola.
asymptoteX <- seq(-5, 5, by=.01)
asymptoteY.upper <- sqrt(3)*asymptoteX
asymptoteY.lower <- -sqrt(3)*asymptoteX
plot(c(-5, 5), c(-5,5), type="n", xlab="x", ylab="y")
# draw the asymptotic lines
lines(asymptoteX, asymptoteY.upper)
lines(asymptoteX, asymptoteY.lower)
# draw the lines for each part of the parabola
lines(xLeft, yLeft.upper)
lines(xLeft, yLeft.lower)
lines(x, y.upper)
lines(x, y.lower)
