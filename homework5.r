# Chapter 5 Excercise 3
# Read in treegrowth CSV
treeg <- read.csv("treegrowth.csv")
# function to get a tree by name
getit <- function(name, x) {
	if(all(x[[name]] == x[[name]][1])) {
		x[[name]][1]
	} else {
		x[[name]]
	}
}
getit2 <- function(name, x) {
	if(all(x[[name]] == x[[name]][3])) {
		x[[name]][3]
	} else {
		x[[name]]
	}
}
# function to get all values for a given tree in treeg
repts <- function(x) {
	res <- lapply(names(x), getit, x)
	names(res) <- names(x)
	res
}
repts2 <- function(x) {
	res <- lapply(names(x), getit2, x)
	names(res) <- names(x)
	res
}
# apply a split across the treeg data by the tree.id/tree name
trees <- lapply(split(treeg, treeg$tree.ID), repts)

# split our trees by habitat
trees1 <- lapply(split(treeg, treeg$habitat), repts2)

# function to find the maximum of a given set of data for a given
# column i.
my.max <- function(x, i) {
	max(x[[i]])
}
# Find the max age of the trees data using column 6
max.age <- max(sapply(trees1, my.max, 6))
# Find the max height of the trees data using column 5
max.height <- max(sapply(trees1, my.max, 5))

# Create our plot for all of our trees
plot(c(0, max.age), c(0, max.height), type="n", xlab="age (years)", ylab="height (feet)")
# Add lines to our plot.
for(i in 1:length(trees1)) {
	lines(trees1[[i]]$age, trees1[[i]]$height.ft)
}

# Chapter 5 Excercise 4
# A function that takes a list of vector rows for pascal's triangle.
# This function will return a new list with n+1 vector rows. 
pascal <- function(triangle) {
	# get the last vector row of the triangle
	lastRow <- triangle[[length(triangle)]]
	# initialize our new vector row with the first appended "1"
	newLastRow <- c(1)
	# for each element i in our old last row add it to i+1 and append
	# it to our new last row
	for(i in 1:length(lastRow)-1) {
		
		if( i == length(lastRow) ) {
			j <-lastRow[i] 
		} else {
			j <- lastRow[i] + lastRow[i+1]
		}
		newLastRow <- c(newLastRow, j )
	}
	# append our last "1" to the end of the new last row
	newLastRow <- c(newLastRow, 1)
	# insert this new last row into our triangle
	newTriangle = list(triangle, newLastRow)
}
print("pascal n = 1")
print(pascal( list(c(1)) ) )
print("pascal n = 10")
print(pascal(pascal(pascal(pascal(pascal(pascal(pascal(pascal(pascal(pascal(list(c(1)))))))))))))
