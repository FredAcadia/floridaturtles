# get work directory path with getwd() then set
setwd("C:/Users/Fredo/Desktop/Unity/MATH 401/floridaturtles")

#Green turtle
greenturtle.nesting.data <- read.csv("data/greenturtletotals.csv", header = T, sep = ",")
greenturtle.nesting.year <- greenturtle.nesting.data$Year
greenturtle.nesting.year.n <- greenturtle.nesting.year - 2018
greenturtle.nesting.count <- greenturtle.nesting.data$Count

#Loggerhead
loggerhead.nesting.data <- read.csv("data/loggerheadturtletotals.csv", header = T, sep = ",")
loggerhead.nesting.year <- loggerhead.nesting.data$Year
loggerhead.nesting.year.n <- loggerhead.nesting.year - 2018
loggerhead.nesting.count <- loggerhead.nesting.data$Count

#Leatherback
leatherback.nesting.data <- read.csv("data/leatherbackturtletotals.csv", header = T, sep = ",")
leatherback.nesting.year <- leatherback.nesting.data$Year
leatherback.nesting.year.n <- leatherback.nesting.year - 2018
leatherback.nesting.count <- leatherback.nesting.data$Count

#All turtles
allturtles.nesting.data <- read.csv("data/allturtlestotals.csv", header = T, sep = ",")
allturtles.nesting.year <- allturtles.nesting.data$Year
allturtles.nesting.year.n <- allturtles.nesting.year - 2018
allturtles.nesting.count <- allturtles.nesting.data$Count

#Summary statistics
summary(greenturtle.nesting.data)
summary(loggerhead.nesting.data)
summary(leatherback.nesting.data)
summary(allturtles.nesting.data)

#bar plot all turtles
barplot(allturtles.nesting.count, names.arg = allturtles.nesting.year,
        xlab = "Year", ylab = "Count", main = "All turtles Data")

#Models
greenturtle_model <- glm(greenturtle.nesting.count~greenturtle.nesting.year.n, family = poisson)
loggerhead_model <- glm(loggerhead.nesting.count~loggerhead.nesting.year.n, family = poisson)
leatherback_model <- glm(leatherback.nesting.count~leatherback.nesting.year.n, family = poisson)
allturtles_model <- glm(allturtles.nesting.count~allturtles.nesting.year.n, family = poisson)

#model-based inferences
allturtles_model

#Plots
par(mfrow=c(2,2))
plot(greenturtle.nesting.year,greenturtle.nesting.count, xlab="Year",ylab="Count",pch=16,cex.lab=1.5,cex.axis=1, main="Green Turtle nesting count per year")
lines(greenturtle.nesting.year, fitted(greenturtle_model))
plot(loggerhead.nesting.year,loggerhead.nesting.count, xlab="Year",ylab="Count",pch=16,cex.lab=1.5,cex.axis=1, main="Loggerhead nesting count per year")
lines(loggerhead.nesting.year, fitted(loggerhead_model))
plot(leatherback.nesting.year,leatherback.nesting.count, xlab="Year",ylab="Count",pch=16,cex.lab=1.5,cex.axis=1, main="Leatherback nesting count per year")
lines(leatherback.nesting.year, fitted(leatherback_model))
plot(allturtles.nesting.year, allturtles.nesting.count, xlab="Year",ylab="Count",pch=16,cex.lab=1.5,cex.axis=1, main="All turtles nesting count per year" )
lines(allturtles.nesting.year, fitted(allturtles_model))

#Display the results of the parameter estimates and associated statistics
summary(greenturtle_model)
summary(loggerhead_model)
summary(leatherback_model)
summary((allturtles_model))
