# course packages via github
install_github("genomicsclass/dagdata")
install_github("ririzarr/rafalib")
# load data for week1
dat <- read.csv("./data/femaleMiceWeights.csv")
tab <- read.csv("./data/msleep_ggplot2.csv")

# look at data
class(tab)
head(tab)
dim(tab)
colnames(tab)
# create/manipulate vectors with c()
c(tab$sleep_total, 1000)
# plot data
plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log="x")
# summary()
summary(tab)

# subset/index
tab[c(1, 2), ]
tab[ tab$sleep_total > 18, ]
tab$sleep_total[ c(1,2) ]

mean(tab$sleep_total[ tab$sleep_total > 18 ])
summary(tab[ tab$sleep_total > 18, ])

# which(), tells you index position > 18
which(tab$sleep_total > 18)
# within col sleep_total, what is the sleep_total at the first index position
tab$sleep_total[which(tab$sleep_total > 18)[1]]
# combine two logical arguements with '&'
which(tab$sleep_total > 18 & tab$sleep_rem < 3)[1]

# sort()
sort(tab$sleep_total)

# order()
order(tab$sleep_total)
tab$sleep_total[ order(tab$sleep_total) ]

# rank()
rank(c(1,2,2,3))
rank(tab$sleep_total)

# match()
# find the index of the first match of a vector in a second vector
match(c("Cow", "Owl monkey", "Cheetah"), tab$name)
idx <- match(c("Cow", "Owl monkey", "Cheetah"), tab$name)
idx
tab[idx,]
idx.rat <- match("Cotton rat", tab$name)
idx.rat
tab[idx.rat,]

# factors
vec = c("red", "blue", "red", "green", "green", "yellow", "orange")
fac = factor(vec)
fac
levels(fac)
vec == "blue"
fac2 = factor(vec, level = c("blue", "green", "yellow", "orange", "red"))
fac2
levels(fac2)
table(tab$order == "Rodentia")

# split()
s = split(tab$sleep_total, tab$order)
s
s[[17]]
s[["Rodentia"]]
mean(s[["Rodentia"]])

# lapply & sapply
lapply(s, mean)
sapply(s, mean)
# sapply and split shortcut
# tapply: vector + factor to group by + function to apply
tapply(tab$sleep_total, tab$order, mean)

lapply(s, sd)
sapply(s, sd)
tapply(tab$sleep_total, tab$order == "Primates", sd)

