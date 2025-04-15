# Linear Regression R Script

# Set your working directory. Go to tab "Session"- "Set Working Directory"-"Choose Directory" 
#   and browse for the folder where you saved your data file

# First, clear all previous stuff out of the workspace...
rm(list = ls());


# No need to install any package

# Import the dataset file
mydata <- read.csv("startup_funding_linear.csv",header=TRUE)

# Read the comma-delimited data file and make sure:
#     1) The first row contain the data labels
#     2) Numeric data fields only contain numbers

# Turn on output to a file (in addition to the screen). This way we've got a record of
# what we did.
#   append   = FALSE means overwrite the file if it already exists
#   split    = TRUE  means send the output to the console too!
sink("RegressionOutput.txt", append=FALSE, split=TRUE)


## Now let's try a simple linear regression with only one single explainatary variable (only one x in the model)
# In this model, we are interested in the relationship between charges (individual medical costs billed by health insurance)
#    and bmi (body mass index). So charges is the outcome variable (y) and bmi is the explaintary variable (x).
# Before running the regression model, we check the correlation between x and y by looking at the graph
pdf("rplot.pdf")
plot(AmountUSD ~ BangaloreBased, data = mydata)


# Next we fit the model to the data by creating a formula and passing it to the lm function. 
# Note: Formulas in R take the form (y ~ x).  
# so we make the model (charges ~ bmi)
# We fit the model with the dataset we defined in line 13 (mydata)
# If you use a different data set, or want to try different inputs, you'll need to change the command

fit <- lm(AmountUSD ~ BangaloreBased, data = mydata)


# Invoke the summary function on any model you've fit with lm and get some metrics indicating the quality of the fit
summary(fit)

# Other Singles
plot(AmountUSD ~ Year, data = mydata)
fit2 <- lm(AmountUSD ~ Year, data = mydata)
summary(fit2)

plot(AmountUSD ~ InMegaCity, data = mydata)
fit3 <- lm(AmountUSD ~ InMegaCity, data = mydata)
summary(fit3)

# Use the fitted model to predict charges, given the value of bmi
mydata$AmountUSD.fit <- predict(fit)


# Check the correlation between predicted charges and the real charges by looking at the graph
# If the model fits the data well, we should be able to see a strong correlation between charges and charges.fit

plot(AmountUSD ~ AmountUSD.fit, data = mydata)



# Then we try to fit a linear regression with more than one explanatory variable 
# For instance, let's say you want to measure the effect of not just bmi, 
# but also age, gender, number of kids, smoke or not?
# To add more explanatory variables, just use the + sign. i.e. (y ~ x + z). 
# To make sure the model does not have multi-collinearity issue, we check the correlation between each two explanatory variable
#pairs(price ~ bmi + age + male + children + smoker, data = mydata)

# Caculate correlation matrix
#res <- cor(mydata)

#round(res, 2)

#  We fit the model with the dataset "mydata"
mfit <- lm(AmountUSD ~ IndustryIsConsumerInternet + BangaloreBased + InMegaCity + TechRelated + PrivateOrSeed + TopInvestor + Year, data = mydata)


# Again, invoke the summary function
summary(mfit)

# Use the fitted model to predict charges, given the value of bmi
mydata$AmountUSD.mfit <- predict(mfit)


# Check the correlation between predicted charges and the real charges by looking at the graph
# If the model fits the data well, we should be able to see a strong correlation between charges and charges.mfit


plot(AmountUSD ~ AmountUSD.mfit, data = mydata)
#dev.off() 


# Turn off output
sink();



