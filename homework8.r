# Homework 8 - Excercise 9

#  This is the formula derived for Xn+1 = Xn - (2*ftn[1](Xn)*ftn[2](Xn))/((2*f[2](Xn))^2 - f[1](Xn)*f[3](Xn))
# This function approximates the second Taylor expansion
approx = function(ftn, x0, tol = 1e-9, max.iter=100) {
	x <- x0
	fx <- ftn(x)
	iter <- 0
	
	# iterate until tolerance is met or max iterations
	# are exceeded.
	while( (abs(fx[1]) > tol) && (iter < max.iter) ) {
		# Modified this part to use the new root finding algorithm
		x <- x - (2*fx[1]*fx[2])/((2*fx[2]^2)-fx[1]*fx[3])
		fx <- ftn(x)
		iter <- iter + 1
		cat("At iteration", iter, "value of x is: ", x, "\n")
	}
	if( abs(fx[1]) > tol) {
		cat("Algorithm failed to converge \n")
		return(NULL)
	} else {
		cat("Algorithm converged\n")
		return(x)
	}
}

# Functions initial function, first, second derivatives for part A
fnA = function(x) {
	return (cos(x) - x)
}
fnA1 = function (x) {
	return (-sin(x) - 1)
}
fnA2 = function(x) {
	return(-cos(x))
}
# Vectorization function that returns f(x), f'(x), and f''(x)
ftnA = function(x) {
	return (c(fnA(x), fnA1(x), fnA2(x))) 	
}
# Approximate values for part A
print(approx(ftnA, 1))
print(approx(ftnA, 3))
print(approx(ftnA, 6))


# Functions for initial function, first, and second derivates for part B
fnB = function(x) {
	return (log(x) - exp(-x))
}
fnB1 = function(x) {
	return ( exp(-x) + 1/x )
}
fnB2 = function(x) {
	return ( ( (-1/(x^2)) - exp(-x)))
}
ftnB = function(x) {
	return (c(fnB(x), fnB1(x), fnB2(x)))
}
#Approximate values for part B
print(approx(ftnB, 2))

# Functions for initial function, first, and second derivatives for part C
fnC = function(x) {
	return (x^3 - x - 3)
}
fnC1 = function(x) {
	return ((3*(x^2)) - 1)
}
fnC2 = function(x) {
	return ( 6*x )
}
ftnC = function(x) {
	return (c(fnC(x), fnC1(x), fnC2(x)))
}
#Approximate values for part C
print(approx(ftnC, 0))

#Functions for initial function, first, and second derivatives for part D
fnD =  function(x) {
	return ( (x^3) -(7*(x^2)) + (14*x) - 8 )
}
fnD1 = function(x) {
	return ( (3*(x^2)) - (14*x) + 14 )
}
fnD2 = function(x) {
	return ( (6*x) - 14  )
}
ftnD = function(x) {
	return (c(fnD(x), fnD1(x), fnD2(x)))
}
#Approximate values for part D
print(approx(ftnD, 1.1))
print(approx(ftnD, 1.2))
print(approx(ftnD, 1.3))
print(approx(ftnD, 1.4))
print(approx(ftnD, 1.5))
print(approx(ftnD, 1.6))
print(approx(ftnD, 1.7))
print(approx(ftnD, 1.8))
print(approx(ftnD, 1.9))

#Functions for initial function, first, and second derivatives for part E
fnE = function(x) {
	return ( log(x)*exp(-x) )	
}
fnE1 = function(x) {
	return ( (exp(-x)*(1-(x*log(x))))/x  ) 
}
fnE2 = function(x) {
	return ( ( exp(-x)*((x^2)*(log(x))-(2*x)-1))/(x^2))
}
ftnE = function(x) {
	return (c(fnE(x), fnE1(x), fnE2(x)))
}
# Approximate values for part E
print(approx(ftnE, 2 ))

# Overall this algorithm converges faster than the Newton-Raphson algorithm.


# Excercise 11
# root finding algorithm implementing halley's method. 
# X(n+1)= [ ((m-1)Xn^m + (m+1)*k)/((m+1)Xn^m + (m-1)*k) ] * Xn
# x = 59, m=1/7, k = 0, find 1.790518691, tolerance = .000000001

# X(n+1) = Xn - ( f(Xn) / (f'(Xn) - ( (f(xn)*f''(xn) )/( 2*f'(xn) ) ) ))

halleysMethodApprox = function(ftn, x0, m0, k0, tol = 1e-9, max.iter=100) {
	x <- x0
	m <- m0
	k <- k0
	fx <- ftnH(x, m, k)
	iter <- 0
	print(fx[1])
	print(fx[2])
	print(fx[3])
	while( (abs(fx[1]) > tol) && (iter < max.iter) ) {
		# calculate X(n+1)
		c <- (2*fx[1]*fx[2])/( (2*(fx[2]^2)) - fx[1]*fx[3])
		b <- ( fx[1]/(fx[2] - ( (fx[1]*fx[3])/(2*fx[2]) ) ))
		x <- x - b
		fx <- ftnH(x, m, k)
		print(fx[1])
		print(fx[2])
		print(fx[3])
		iter <- iter + 1
		cat("At iteration", iter, "value of x is: ", x, "\n")
	}
	if( abs(fx[1]) > tol ) {
		cat("Algorithm failed to converge\n")
		return(NULL)
	} else {
		cat("Algorithm converged\n")
		return(x)
	}
}


# Original function, first derivative, and second derivative functions of f(x) = x^m - k
fnH = function(x, m, k) {
	return ( (x^m) - k ) 
}
fnH1 = function(x, m, k) {
	return ( m*(x^(m-1)) )
}
fnH2 = function(x, m, k) {
	return ( (m-1)*m*(x^(m-2)))
}
# function to bundle the function and all derivatives into a vector for the root finding algorithm
ftnH = function (x, m, k) {
	x <- abs(x)
	return ( c(fnH(x, m, k), fnH1(x, m, k), fnH2(x, m, k) ) )
}

# Approximate using halley's method. 
print(halleysMethodApprox(ftnH, 59, (1/7), 0))


# This gives the same result as the more direct method given in the text.
directMethod = function(x, m, k) {
	i <- ((m-1)*x^m + (m+1)*k)
	j <- ((m+1)*x^m + (m-1)*k)
	return ( ((i/j) * x))
}
print(directMethod(59, (1/7), 0))


