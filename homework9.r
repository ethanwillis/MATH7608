# Homework 9 - Exercise #3


# This is the simpson from the text that is supposed to be used in this
# excercise in conjunction with the Phi and phi functions and root finding functions. 
simpson = function(ftn, a, b, tol = 1e-9, verbose = FALSE) {
  # numerical integral of ftn from a to b
  # using Simpson's rule with tolerance tol
  #
  # ftn is a function of a single variable and a < b
  # if verbose is TRUE then n is printed to the screen
  # initialise
  n <- 4
  h <- (b - a)/4
  fx <- sapply(seq(a, b, by = h), ftn)
  S <- sum(fx*c(1, 4, 2, 4, 1))*h/3
  S.diff <- tol + 1  # ensures we loop at least once
  
  # increase n until S changes by less than tol
  while (S.diff > tol) {
    S.old <- S
    n <- 2*n
    h <- h/2
    fx[seq(1, n+1, by = 2)] <- fx  # reuse old ftn values
    fx[seq(2, n, by = 2)] <- sapply(seq(a+h, b-h, by = 2*h), ftn)
    S <- h/3*(fx[1] + fx[n+1] + 4*sum(fx[seq(2, n, by = 2)]) +
         2*sum(fx[seq(3, n-1, by = 2)]))
    
    S.diff <- abs(S - S.old)
  }
  if (verbose) cat('partition size', n, '\n')
  return(S)
}

# Here I modified several versions of the Phi functions
# in order to make them work with the root finding algorithms
# for the specified values of .5, .95, .975, and .99
# This works by setting the integral minus the value of p equal
# to 0 and solving for 0 using the newton-rhapsom method
phi = function(x) return(-.5+(exp(-x^2/2)/sqrt(2*pi)))
Phi = function(z) {
	if(z < 0) {
		return(0.5 - simpson(phi, z, 0))
	} else {
		return(0.5 + simpson(phi, 0, z))
	}
}
ftn1 = function(x) {
	return (c(Phi(x), phi(x)))
}

phi2 = function(x) return(-.95+(exp(-x^2/2)/sqrt(2*pi)))
Phi2 = function(z) {
	if(z<0) {
		return(0.5 - simpson(phi2, z, 0))
	} else {
		return(0.5 + simpson(phi2, 0, z))
	}
}
ftn2 = function(x) {
	return (c(Phi2(x), phi2(x)))
}

phi3 = function(x) return(-.975+(exp(-x^2/2)/sqrt(2*pi)))
Phi3 = function(z) {
	if(z<0) {
		return(0.5 - simpson(phi3, z, 0))
	} else {
		return(0.5 + simpson(phi3, 0, z))
	}
}
ftn3 = function(x) {
	return (c(Phi3(x), phi3(x)))
}

phi4 = function(x) return(-.99+(exp(-x^2/2)/sqrt(2*pi)))
Phi4 = function(z) {
	if(z<0) {
		return(0.5 -simpson(phi4, z, 0))
	} else {
		return(0.5 + simpson(phi4, 0, z))
	}
}
ftn4 = function(x) {
	return (c(Phi4(x), phi4(x)))
}

# Basic root finding algorithm from text.
newtonraphson <- function(ftn, x0, tol = 1e-9, max.iter = 100) {
	# initialise
	x <- x0
	fx <- ftn(x)
	iter <-  0
	# continue iterating until stopping conditions are met
	while ((abs(fx[1]) > tol) && (iter < max.iter)) {
		# Calculate successive approximate values of x
		x <- x - fx[1]/fx[2]
		fx <- ftn(x)
		iter <- iter + 1
		cat("At iteration", iter, "value of x is:", x, "\n")
  	}
  	# output depends on success of algorithm
  	if (abs(fx[1]) > tol) {
    		cat("Algorithm failed to converge\n")
    		return(NULL)
  	} else {
    		cat("Algorithm converged\n")
    		return(x)
  	}
}

# Expressed Phi and phi functions in terms of the root newton-rahpson 
# root finding algorithm and print out convergences for 
# each p value

#Finds Zp for p = 0.5
print(newtonraphson(ftn1, 1 ))
#Finds Zp for p = .95
print(newtonraphson(ftn2, 1 ))
#Finds Zp for p = .975
print(newtonraphson(ftn3, 1))
#Finds Zp for p = .99
print(newtonraphson(ftn4, 1))
