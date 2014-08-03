# Ethan Willis Homework 1

#Exercise 2 parts a - d
# Part a: Combine two vectors counting up to 8 and down from 7.
x <- c( (1:8), (7:1) )
# Print this vector.
x
# Part b: Create a vector consisting of the elements in part b
y <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)
# Print this vector.
y

# Part c: Create a 3 by 3 matrix consisting of the data 
# elements 0, 1, 1, 1, 0, 1, 1, 1, 0 and print it.
data1 <- c(0, 1, 1, 1, 0, 1, 1, 1, 0);
matrix1 <- matrix(data1, nrow=3, ncol=3, byrow=FALSE)
matrix1

# Part d: Create a 3 by 3 matrix consisting of the data
# 0, 2, 3, 0, 5, 0, 7, 0, 0 and print it
data2 <- c(0, 2, 3, 0, 5, 0, 7, 0, 0)
matrix2 <- matrix(data2, nrow=3, ncol=3, byrow=TRUE)
matrix2

# Exercise 4: Produce a vector containing integers 1 .. 100
# that are not divisible by 2, 3, or 7
# Create vector of all integers 1 to 100
vector1 <- (1:100)
# remove all elements that are divisible by 2 or 3 or 7.
vector1 <- vector1[vector1%%2 != 0 & vector1%%3 != 0 & vector1%%7 != 0]
# Print 
vector1

# Exercise 5
# Create and print initial queue
queue <- c("Steve", "Russell", "Alison", "Liam")
queue
# Part a: Barry arrives to the queue.
queue <- c(queue, "Barry")
queue
# Part b: Steve is served
queue <- queue[queue != "Steve"]
queue
# Part c: Pam talks her way to the front of the queue.
queue <- c("Pam", queue)
queue
# Part d: Barry gets impatient and leaves
queue <- queue[queue != "Barry"]
queue
# Part e: Alison also gets impatient and leaves
queue <- queue[queue != "Alison"]
queue
# Find where Russell is standing in line using which(x)
which(queue == "Russell")

