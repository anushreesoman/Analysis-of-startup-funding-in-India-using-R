# Logistic Regression R Script

# First, clear all previous stuff out of the workspace...
rm(list = ls());

# Input your dataset
mydata <- read.csv("startup_funding-project.csv",header=TRUE)
summary(mydata)

# Read the comma-delimited data file and put them into a variable called "dat." Make sure:
#     1) The first row contain the data labels
#     2) Numeric data fields only contain numbers


# Turn on output to a file (in addition to the screen). This way we've got a record of
# what we did.
#   append   = FALSE means overwrite the file if it already exists
#   split    = TRUE  means send the output to the console too!
sink("RegressionOutput.txt", append=FALSE, split=TRUE);

# Fit the model to the data by creating a formula and passing it to the glm function. 
# In our case we want to predict the survival of a passanger using his/her age
# so we make the model (Survived ~ age)
# In other words, we're representing the relationship between Age (x) and Survived (y).
# If you use a different data set, or want to try different inputs, you'll need to change the model!!
# Note: Formulas in R take the form (y ~ x). 
fit = glm(FiveMillionOrMore ~ InMegaCity, data = mydata, family="binomial")


# Invoke the summary function on any model you've fit with glm and get some metrics indicating the quality of the fit
summary(fit)


fit2 = glm(FiveMillionOrMore ~ Industry, data = mydata, family="binomial")
summary(fit2)
# But what if you have more than one predictor variable? 
# For instance, let's say you want to measure the effect of not just age, 
# but also the Pclass, Sex, Age, etc.Pclass indicates each passenger's class (1st, 2nd, or 3rd)
#   SibSp indicates the number of siblings/spouses aboard the Titanic, Parch indicates the number of parents/children aboard the Titanic
#   Fare is the fare paid for ticket

# To add more predictor variables, just use the + sign. i.e. (y ~ x + z). 
pairs(FiveMillionOrMore ~ BangaloreBased + Industry+ TechRelated + InMegaCity+ PrivateOrSeed, data = mydata)

mfit = glm(FiveMillionOrMore ~ BangaloreBased + Industry + TechRelated + InMegaCity+ PrivateOrSeed, data = mydata, family="binomial")



# Again, invoke the summary function
summary(mfit)

# To add more interaction terms as predictor variables, just use the + sign. i.e. (y ~ x + z + x*z). 
# Open your csv data file and add the interaction terms: Pclass_Male (Pclass*Male),
#  Pclass_Age, Sex_Age,	and Age_SibSp
# Modify your model by incuding those interaction terms
mfit_interaction = glm(FiveMillionOrMore ~ BangaloreBased + Industry + TechRelated + InMegaCity+ PrivateOrSeed+ Year + TopInvestor, data = mydata, family="binomial")

# Again, invoke the summary function
summary(mfit_interaction)


# Turn off output
sink();

