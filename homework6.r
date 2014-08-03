library("lattice")
ufc <- read.csv("ufc.csv")
str(ufc)
dp <- densityplot(~ dbh.cm | species, data = ufc)
bp <- bwplot(~ dbh.cm | species, data = ufc)
hist <- histogram(~ dbh.cm | species, data = ufc)
xyp <- xyplot(height.m ~ dbh.cm | species, data = ufc)
print(dp, split=c(1, 1, 2, 2), more = TRUE)
print(bp, split=c(2, 1, 2, 2), more = TRUE)
print(hist, split=c(1, 2, 2, 2), more = TRUE)
print(xyp, split=c(2, 2, 2, 2), more = FALSE)
# My interpretation of the plots and histograms is that
# the DBH of the WL species versus the DBH of the WC, DF, and
# GF species is much more variable. The remaining 3 species
# have dbh that center around a specific value with closer to
# normal distributions.

# excercise 3
sexesInfo <- rnorm(100, 160, 20)
pop <- data.frame(m=rnorm(100,160,20), f=rnorm(100, 160,20))
randomSample <- sample(x, size=length(x))
next.gen <- function(pop) {
	pop$m <- sample(pop$m)
	pop$m <- apply(pop, 1, mean)
	pop$f <- pop$m
	return(pop)
}
# generate nine generations inclusive of our first generation being the 
# intiial population
gen1 <- next.gen(pop)
gen2 <- next.gen(gen1)
gen3 <- next.gen(gen2)
gen4 <- next.gen(gen3)
gen5 <- next.gen(gen4)
gen6 <- next.gen(gen5)
gen7 <- next.gen(gen6)
gen8 <- next.gen(gen7)
# Create histograms for all 9 of our generations.
hist1 <- histogram(pop$m)
hist2 <- histogram(gen1$m)
hist3 <- histogram(gen2$m)
hist4 <- histogram(gen3$m)
hist5 <- histogram(gen4$m)
hist6 <- histogram(gen5$m)
hist7 <- histogram(gen6$m)
hist8 <- histogram(gen7$m)
hist9 <- histogram(gen8$m | main=list(label="Distribution of male height by generation"))
# Print out as a lattice all of our histograms on a 3 by 3 grid
print(hist1, split=c(1, 1, 3, 3), more = TRUE)
print(hist2, split=c(2, 1, 3, 3), more = TRUE)
print(hist3, split=c(3, 1, 3, 3), more = TRUE)
print(hist4, split=c(1, 2, 3, 3), more = TRUE)
print(hist5, split=c(2, 2, 3, 3), more = TRUE)
print(hist6, split=c(3, 2, 3, 3), more = TRUE)
print(hist7, split=c(1, 3, 3, 3), more = TRUE)
print(hist8, split=c(2, 3, 3, 3), more = TRUE)
print(hist9, split=c(3, 3, 3, 3), more = FALSE) 
