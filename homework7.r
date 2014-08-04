# Ethan Willis Homework 7 - Chapter 9: Exercise #7

# Selection Sort Method
# This function implements selection sort to sort an unsorted vector 
# @param x - an unsorted vector
# @return returns a sorted vector with the elements of x
selectionSort = function(x) {
	# initilalize our unsorted and sorted vectors u
	# and s respectively.
	u <- x
	s <- c()
	
	# while there are still unsorted elements in u.
	while(length(u) > 0) {
		# Select the index of the minimum element in vector u
		minIndex <- which.min(u)
		
		# Append the minimum element of u to the end of our
		# sorted vector s.
		s <- c(s, u[minIndex])
		
		# remove the minimum element at minIndex from vector u.
		u <- u[-minIndex] 
	}
	# return our sorted vector.
	return(s)
}
# You have to perform n^2 comparisons in order to implement selection sort.
# n elements are selected. And at each selection i, n to 1 elements are compared against.
print( selectionSort(c(2, 5, 3, 9, 8, 6, 4, 10, 7)) )

# Insertion sort method - This method performs insertion sort on an unsorted
# vector
# @param x - an unsorted vector
# @return returns a sorted vector
insertionSort = function(x) {
	# initialize our sorted and unsorted vectors
	u <- x
	s <- c()
	
	while(length(u) > 0) {
		# store and remove the last element,a, from u
		a <- u[length(u)]
		u <- u[-length(u)]
		
		# store this first element in empty vector s
		# find an index i of vector s where all
		# elements at i and below are less than a.
		i <- length(s)
		searchingForI = TRUE
		while( searchingForI && (i > 0) ) {
			# Stop searching if a is bigger than all
			# the sorted elements at and below index i
			if( a >= s[i] ) {
				searchingForI = FALSE
			}
			# otherwise search at index i-1
			else {
				i <- i - 1
			}
		}
		# Insert the element a in between elements at
		# indices i and i+1
		s <- c(head(s, i), a, tail(s, length(s)-i))
	}
	# return our sorted vector, s
	return(s)
}
print( insertionSort( c(1, 3, 8, 6, 7, 5, 4, 10, 9, 15) ) )
# Best and worst case analysis
# The best case for insertion sort is the case where all elements from the initial
# unsorted vector simply need to be appended to the end of the 
# sorted vector and only require 1 comparison each with the last
# element in the sorted vector.
# So in the best case n comparisons are required.
# The worst case scenario for insertion sort is where each element
# will need to be moved to the front of the sorted list. This requires
# a comparison against all inserted elements at each step which
# leaves us with n^2 comparisons in the worst case.


# Bubble Sort function - This function performs bubble sort on an unsorted vector x
# @param x - an unsorted vector
# @return returns a sorted vector
bubbleSort = function(x) {
	# if the vector length is 1, the vector is already sorted
	if(length(x) == 1) {
		return(x)
	}
	# otherwise we need to bubble sort.
	else{
		# do an initial bubbling and determine if
		# successive bubblings are required
		for(i in 1:(length(x)-1)) {
			# swap two values if necessary
			# and record that we swapped them
			if(x[i] > x[i+1]) {
				swap <- x[i]
				x[i] <- x[i+1]
				x[i+1] <- swap 
				swapped <- TRUE
			}
		}
		# Continue to bubble elements to the end of the vector
		# until no elements were bubbled/swapped in the
		# previous iteration
		while(swapped) {
			swapped <- FALSE
			for(j in 1:(length(x)-1)) {
				# swap two values if necessary
				# and record that we swapped them
				if(x[j] > x[j+1]) {
					swap <- x[j]
					x[j] <- x[j+1]
					x[j+1] <- swap
					swapped <-TRUE
				}
			}
		}
	}
	return(x)
}
print( bubbleSort( c(1, 3, 8, 7, 4, 2) ) )
# Given a vector of n elements that was already sorted the minimum
# number of comparisons required is n-1.This is because it compares
# each element at i with i + 1 and the element at position n doesn't
# need to be compared with the next element because there isn't one. 
# So n-1 is the minimum.
# In the worst case each element at position i will need to be bubbled
# to the last unsorted position. This means that the first element
# will need to be compared to n-1 elements. 
# The second element will need to be compared to n-2 elements.. and so
# on. This comes to n^2 comparisons in the worst case.



quickSort = function(x) {
	if( length(x) == 0 || length(x) == 1 ) {
		return(x)
	} else {
		a1 <- x[1]
		l <- x[x<a1]
		g <- x[x>a1]
		b <- quickSort(l)
		c <- quickSort(g)
		return( c(b, a1, c) )
	}

}
print( quickSort( c(4, 8, 2, 3, 9, 10, 11, 7) ) )
