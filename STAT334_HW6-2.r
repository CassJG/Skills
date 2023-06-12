library(car)
library(olsrr)
library(tidyverse)
library(effects)

# Import the USTemp6.csv data file
USTemp6 <- read.csv("/Users/giovannetticharles/Desktop/STAT334_HW6-2/USTemp6.csv")
# Make sure that Heading = Yes and Rownames = first column
temp <- USTemp6
temp <- mutate(temp,cLat = scale(Lat,scale=FALSE),
               cLong = scale(Long,scale=FALSE),
               cAltitude = scale(Altitude,scale=FALSE),
               cCoastMiles = scale(CoastMiles,scale=FALSE))
temp$Coast <- factor(temp$Coast)
temp$Region <- factor(temp$Region)

# Problem 1
# Run Model temp1
temp1 <- lm(JanTemp ~ cLat*cLong + cAltitude + cCoastMiles, data=temp)
summary(temp1)
ols_vif_tol(temp1)

# Check assumptions for Model temp1
residualPlots(temp1,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(temp1,envelope=FALSE,pch=16,id=FALSE)

# Compute and plot the interaction effects from Model temp1
temp1.eff <- effect("cLat*cLong",temp1)
temp1.eff
plot(temp1.eff,"cLat",lines=list(multiline=TRUE,rug=FALSE))
plot(temp1.eff,"cLong",lines=list(multiline=TRUE,rug=FALSE))

# Run Model temp2
temp2 <- lm(JanTemp ~ cLat + cAltitude + Region*Coast, data=temp)
summary(temp2)
Anova(temp2)
ols_vif_tol(temp2)

# Check assumptions for Model temp2
residualPlots(temp2,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(temp2,envelope=FALSE,id=FALSE,pch=16)

# Compute and plot the interaction effects from Model temp2
temp2.eff <- effect("Region*Coast",temp2)
temp2.eff
plot(temp2.eff,"Region",lines=list(multiline=TRUE,rug=FALSE))

# Run Model temp3
temp3 <- lm(JanTemp ~ cLat + cAltitude + Region*cCoastMiles, data=temp)
summary(temp3)
Anova(temp3)
ols_vif_tol(temp3)

# Check assumptions for Model temp3
residualPlots(temp3,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(temp3,envelope=FALSE,id=FALSE,pch=16)

# Compute and plot the interaction effects from Model temp3
temp3.eff <- effect("Region*cCoastMiles",temp3)
temp3.eff
plot(temp3.eff,"cCoastMiles",lines=list(multiline=TRUE,rug=FALSE))


# Import the CommProp.csv data file
CommProp <- read.csv("/Users/giovannetticharles/Desktop/STAT334_HW6-2/CommProp.csv")
# Make sure that Heading = Yes
cp <- CommProp
cp.centered <- scale(cp[,-1],scale=FALSE)
colnames(cp.centered) <- c("cAge","cExpenses","cVacancy","cSqft100K")
cp <- data.frame(cp,cp.centered)

# Problem 2
# Model 1
cp1 <- lm(Rental~cAge+cExpenses+cVacancy+cSqft100K,data=cp)

# Since the number of predictors is small, look at all possible regressions
all.cp1 <- ols_step_all_possible(cp1)
View(all.cp1)
# The plot will appear in a separate window.
plot(all.cp1)

# Model 2: Add all two-way interactions to Model 1
cp2 <- update(cp1,.~.^2)

# Since the number of predictors and interactions isn't large (it's only 10),
# use "best subsets regression". This might take a few minutes.
best.cp2 <- ols_step_best_subset(cp2)
View(best.cp2)
# The plot will appear in a separate window.
plot(best.cp2)

#Residual testing
summary(cp2)
residualPlots(cp2,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)
qqPlot(cp2,envelope=FALSE,id=FALSE,pch=16)
influenceIndexPlot(cp2, vars = c("studentized"))
#leverage cut off 0.40
influenceIndexPlot(cp2, vars = c("hat"))
qf(0.5, 11, 70)
influenceIndexPlot(cp2, vars = c("Cook"))


# Forward selection using p-value < 0.01 to enter the model
step.p <- ols_step_forward_p(cp2,penter=0.01)
step.p
step.p.model <- step.p$model
summary(step.p.model)

# Forward selection using AIC to choose the model
step.aic <- ols_step_forward_aic(cp2)
step.aic
step.aic.model <- step.aic$model
summary(step.aic.model)


# Problem 3
# Model 3
# Use the poly() command to make a quadratic polynomial for Age
Age.poly <- poly(cp$Age,2,raw=TRUE)
# Use this polynomial in the regression
cp3 <- lm(Rental~Expenses+Sqft100K+Age.poly,data=cp)
summary(cp3)
ols_vif_tol(cp3)

#Residual Plots
residualPlots(cp3,type="rstudent",pch=16,quadratic=FALSE,id=FALSE,tests=FALSE)

# Model 4
# Create a new column to hold the (cAge)^2 values
# Doing this will make using the Predict function easier.
cp$cAge2 <- cp$cAge^2
cp4 <- lm(Rental~cAge+cExpenses+cSqft100K+cAge2,data=cp)
summary(cp4)
ols_vif_tol(cp4)

cpage <- lm(Rental~cAge+cExpenses+cSqft100K,data=cp)

#Comparing models
anova(cpage, cp4)

# Here are the means of the original x-variables
(mean.cp <- colMeans(cp[,2:5]))

# The code below centers the input values and computes the interval. Replace each ? with a value.
new.x <- data.frame(cAge=8-mean.cp[1],cExpenses=16-mean.cp[2],cSqft100K=2.5-mean.cp[4],cAge2=(64-(mean.cp[1])^2))
Predict(cp4,newdata=new.x,interval="confidence",level=0.95)
