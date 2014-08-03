# Chapter 4 Exercise 1 
EuclideanLength <- function(v) {
	# Find the Euclidean length of v.
	# square all elements of our vecto
	v <- v ^ 2
	# sum all squared elements
	sum <- sum(v)
	# calculate the square root of these elements
	sqrt(sum)
}
# test our function with the vector (1, 2, 3)
print(EuclideanLength( c(1, 2, 3) ))

# Chapter 4 Excercise 3 - Part A
# simulate 4 rolls of a six sided die.
roll1 <- sample(1:6, size=1)
roll2 <- sample(1:6, size=1)
roll3 <- sample(1:6, size=1)
roll4 <- sample(1:6, size=1)
if(roll1 == 6 || roll2 == 6 || roll3 == 6 || roll4 == 6) {
	print("You win!")
} else {
	print("You lose.")
}

# Part B - Rewrite part A as a function sixes that simulates n rolls.
sixes <- function(n = 4) {
	# create an empty vector to store simulated rolls
	rolls <- c()
	# pick n random rolls from a die and store them in a vector.
	for(i in 1:n) {
		rolls[i] = sample(1:6, size=1)
	}
	# Default value of losing.
	win <- FALSE
	# Check each of our rolls to see if we won.
	for(i in 1:n) {
		# if the ith roll is a 6 then we have won.
		if(rolls[i] == 6) {
			win <- TRUE
		}
	}

	return(win)
}
print(sixes())
print(sixes(6))
# We can make n have a default value of 4 by modifying the parameters of the 
# function definition  like this: sixes <- function(n=4)
# I've made this modification above.

# Chapter 4 Excercise 3 - Part C Simulate sixes(4) N times.
simulateSixes <- function(N) {
	# Create an empty vector to store the results of our simulations
	sims <- c()
	for(i in 1:N) {
		sims[i] = sixes()
	}
	numWins <- 0
	# sum how many wins we had.
	for(i in 1:length(sims)) {
		if(sims[i] == TRUE) {
			numWins <- numWins + 1
		}
	}
	# calculate probability of winning
	prob <- numWins/N
	# Calculate and print the difference between the theoretical
	# and simulated probability.
	theoreticalProb <- 1 - (5/6)^4
	print(abs(theoreticalProb - prob))
	return(prob)
}
# Simulate with different values of N
print(simulateSixes(100))
print(simulateSixes(1000))
print(simulateSixes(10000))
# How does the variability of your results depend on N?
# From several runs I noticed that as N increases the variability in the
# outcomes was smaller. For N=100 I noticed the result was .5 +- .1
# for N=1000 it was .5 +-.04 and for N=10000 I saw .51 +-.01

# I've modified my original program for part c to include the changes for calculating the theoretical probability as well as the simulation estimate and the difference of the two.
# The accuracy of my results varies by about 1 order of magnitude as N increases by 1 order of magnitude. What i noticed is that for n=10^2 the difference
# between the theoretical and simulated value approximately fluctuated between
# .1 and .001
# With n=10^3 it approximately fluctuated between .01 and .002
# With n=10^4 it approximately fluctuated between .001 and .002
