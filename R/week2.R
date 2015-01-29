# open data
dat <- read.csv("./data/femaleMiceWeights.csv")
# look at data
dat
# assign groups
control <- dat[1:12, 2]
treatment <- dat[13:24, 2]
# examine group means
print(mean(treatment))
print(mean(control))
# determine the difference between the means
diff <- mean(treatment) - mean(control)
diff
###############################################################################
# assign the columns to s and split data into two vectors defined by the weights
s = split(dat[, 2], dat[, 1])
s
# plot data with a stripchart
stripchart(s, vertical = TRUE, col = 1:2)
# add mean y-value for horizontal lines ('h')
abline(h = sapply(s, mean), col = 1:2)

# create 'highfat'
highfat <- s$hf
highfat
# sample 6-weights from the highfat mice
sample(highfat, 6)
sample(highfat, 6, replace = TRUE)
sample(highfat, 6, replace = FALSE)

# calculate proportions with logical vectors
highfat > 30
# convert logical into numeric
as.numeric(highfat > 30)
# calculate sum
sum(highfat > 30)
# and proportion of highfat mice over 30
mean(as.numeric(highfat > 30)) / length(highfat)
mean(highfat < 30)) / length(highfat)
mean(highfat >30)

###############################################################################
# load in another mouse population
population <- read.csv("./data/femaleControlsPopulation.csv")

# 12 control mice
control <- sample(population[,1], 12)
control
# another 12 control mice that we act as if they were not
treatment <- sample(population[, 1], 12)
mean(treatment) - mean(control)
# now do this 10,000 times, with for-loop
n <- 10000
# Null distribution
null <- vector("numeric", n)
# for-loop
for(i in 1:n){
  control <- sample(population[, 1], 12)
  treatment <- sample(population[, 1], 12)
  null[i] <- mean(treatment) - mean(control)
}
# determine the difference between the means
diff <- mean(treatment) - mean(control)
# or
diff <- mean(dat[13:24, 2]) - mean(dat[1:12, 2])

# percent difference
mean(null > diff)
###############################################################################
population <- read.csv("./data/femaleControlsPopulation.csv")
mean(population$Bodyweight)
# or
population <- population[, 1]
mean(population)
sample(population, 12)
mean(sample(population, 12))
# use replicate function instead of for-loop
sampleMean = replicate(10000, mean(sample(population, 12)))
head(sampleMean)
plot(sampleMean)
# use 'replicate' to calculate difference like above
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population), 12))
head(null)
plot(null)
###############################################################################
# illustrate the null distribution
n <- 1000
plot(0, 0, xlim = c(-5, 5), ylim = c(1, 30), type = "n")
totals <- vector("numeric", 11)
for(i in 1:n){
  control <- sample(population[, 1], 12)
  treatment <- sample(population[, 1, 12])
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff) + 6, 11), 1)
  totals[j] <- totals[j] + 1
  text(j - 6, totals[j], pch = 15, round(nulldiff, 1))
  #if(i < 15) scan() # You can add this line to interactively see values appear
###############################################################################
population <- read.csv("./data/femaleControlsPopulation.csv")
population <- population[, 1]
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
mean(null)
# histogram to display the calculated probibility of observing
# as extreme a difference from the null distribution
hist(null)
# orginal difference
diff = mean(dat[13:24,2]) - mean(dat[1:12,2])
# positive tail
abline(v=diff,col="red")
# negative tail
abline(v=-diff,col="red")

values <- seq(min(null),max(null),len=300)
myecdf <- ecdf(null)
plot(values,myecdf(values),type="l")

# determine the one-tailed probability of seeing as big a difference as we observed,
# calculated form your null distribution
null = replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
# percent difference or pvalue
mean(null > diff) # 1-tailed
# two-tailed
mean(null > diff) * 2
# my answer
(1-pnorm(diff, mean(null), sd(null)))*2
# quiz answer
mean(abs(null) > abs(diff))
