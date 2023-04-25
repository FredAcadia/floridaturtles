# get work directory path with getwd() then set
setwd("/Users/fredericviennot/Documents/Perso/Unity/MATH 401/floridaturtles")

greenturtle.nesting.data <- read.csv("data/greenturtlenestingdata5years.csv", header = T, sep = ",")

greenturtle.nesting.year <- c(2018,2019,2020,2021,2022)
greenturtle.nesting.year.n <- greenturtle.nesting.year - 2018
greenturtle.nesting.count <- c(4546,53016,26656,32680,37028)

#Create an scatter plot of year vs. count
plot(greenturtle.nesting.year,greenturtle.nesting.count, xlab="Year",ylab="Count",pch=16,cex.lab=1.5,cex.axis=1)

#Fit the model, log(count) = b0 + b1*year using possion distribution 
#The output is saved in an object scrub.jay
scrub.jay<-glm(greenturtle.nesting.count~greenturtle.nesting.year.n, family=poisson)

#Display the results of the parameter estimates and associated statistics
summary(scrub.jay)
