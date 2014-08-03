# Question 1
# input
x.values <- seq(-2, 2, by=0.1)

# for each x calculate y
n <- length(x.values)
y.values <- rep(0, n)
for(i in 1:n) {
	x <- x.values[i]
	# your expression y goes here
	if(x <= 0) {
		y <- -x^3
	} else if ( x <= 1 ) {
		y <- x^2
	} else if( x > 1 ) {
		y <- x^(1/2)
	}		
	y.values[i] <- y
}
plot(x.values, y.values, type="l")

# Do you think that f has a derivative at 1? Yes
# Do you think that f has a derivative at 0? Yes


# Question 2
# Calculate h(x, n) = 1 + x + x^2 +... + x^n

# Setup our x and n values.
x <- 2
n <- 0
# Initialize our result, h
h <- 0
# Calculate the Riemann Sum of x^i from i = 0 to n.
for(i in 0:n) {
	h <- h + x^i
}
# Output our calculation
print(h)


# Question 4
# Rewrite of program in question 2 using a while loop

# setup our x and n values
x <- 2
n <- 0
# initialize our result, h, and the successive power, i.
h <- 0
i <- 0
# sum the x raised to successive i powers.
while( i <= n ) {
	h <- h + x^i
	i <- i+1
}
# output our calculated value
print(h)

# Rewrite of our program in question 2 using vector programming

# setup our x and n values
x <- 2
n <- 0
# create a vector of all of our i = 0 to n values
i <- seq(0, n, by=1)
# sum x raised to each of these powers and print the result.
h <- sum(x^i)
# output our calculated value
print(h)


# Question 5
# create a vector of points and assign them to coordinates in a matrix
xy <- c(1, 0, 1, 0)
xy <- matrix(xy, nrow=2, ncol=2)
# create our premultiplication matrix and populte our radians value.
theta <- 1.57079633 
preMult <- c(cos(theta), -sin(theta), sin(theta), cos(theta))
preMult <- matrix(preMult, nrow=2, ncol=2, byrow=TRUE)
# do our matrix multiplication in order to get our rotated vectors
result <- xy %*% preMult
print(matrix(result, nrow=3, ncol=3))

# Queston 6: Given vector x, calculate it's geometric mean using a for loop
# and vector operations.

# Using a for loop
# Initialize vector x
x <- seq(1, 5, by=1)
mean <- 1
# for every element in our vector multiply it by the previous element,
# or multiply by 1 if it's our first element.
for(xi in x) {
	mean <- mean*xi
}
# raise our sum to the correct power to get the mean.
mean <- mean^(1/length(x))
print(mean)

# Calculate geometric mean using vectors.
# initialize vector x
x <- c(1, 2, 3, 4, 5)
# multiply each element in succession.
mean <- prod(x)
# raise to correct power to calculate geometric mean.
mean <- mean^(1/length(x))
print(mean)


# Question 10: Use a loop to find the minimum value in a vector x
# Initialize vector
x <- c(7, 3, 5, 4, 8, 10, 1, 2)
# Set default answer to be the index of the first element.
answer <-1 
# Loop through each index of elements in vector x
for(i in 2:length(x)) {
	# check if our new index contains a smaller value.
	if( x[i] < x[answer] ) {
		# if so, we have a new index where the smallest value is
		answer <- i
	}
}
# Print out the smalles value from our vector.
print(x[answer])
