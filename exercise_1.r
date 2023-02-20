library(ggplot2)
library(reshape)
# If you get an error on this command, you probably need to install ggplot2
# and/or reshape, with:
# > install.packages('ggplot2')
# > install.packages('reshape')
# Then, run the library command again
#“Wage2” dataset can be found more information https://cran.r-project.org/web/packages/wooldridge/wooldridge.pdf
#“mroz” datase can be found here https://cran.r-project.org/web/packages/MASS/MASS.pdf


wages<-read.csv('http://inta.gatech.s3.amazonaws.com/wage2.csv')
# Read data from gatech website. This data accompanies the econometrics textbook by Wooldridge:
# http://www.amazon.com/Introductory-Econometrics-A-Modern-Approach/dp/1111531048


summary(wages)
# Print summary statistics for the data frame

table(wages$educ)
# Tabulate education levels in the sample

ggplot(data=wages, aes(x=educ, y=wage)) + geom_point() + stat_smooth(formula=y~x)
# Look at the distribution of education and wages in a scatter plot

model.results <- lm(wage ~ educ, data=wages)
# Fit a linear model where y is wage and x is education

print(model.results)
# print a simple version of the model results

summary(model.results)
# Print a more detailed version of model results

data.to.predict <-data.frame(educ=c(1,12,14,50))
data.to.predict$predicted.wage <- predict(model.results, data.to.predict)
# Predict outcomes for different people with different levels of education, 1,
# 12, 14, and 50 years
data.to.predict
  
# To control for more variables, add them to the Right hand side of the
# 'formula', which is the 'wage ~ educ' piece of the code
model.results.detail <- lm(wage ~ educ + IQ, data=wages)
summary(model.results.detail)
# Note that adding IQ here reduces the coefficient on education, which makes
# sense per the discussion of omitted variables that we have done
# Try using different combinations of variables to see what works and makes sense.

ggplot(data=wages, aes(x=educ, y=wage)) + geom_violin(aes(group=educ)) + stat_smooth(data=wages, aes(x=educ, y=wage), method = 'lm')
# A picture of how the linear model does, with marginal distributions
# Save the picture by uncommenting the line below:
# ggsave('violin.png', width=7, height=5, units = "in")

lfp <- read.csv('http://inta.gatech.s3.amazonaws.com/mroz.csv')
# Load labor force participation data
summary(lfp)

linear.model <- lm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data = lfp)
summary(linear.model)
# Fit a linear model

logit.model <- glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data = lfp, family='binomial')
# Fit a logistic model using the 'glm' command
summary(logit.model)

probit.model <-glm(inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6, data = lfp, family=binomial(link = "probit"))
# Fit a probit model, also using the 'glm' command.
# Note how the family command changes us from logit to probit

#############
#Prepare to plot predicitons for education from 0 to 20. Don't worry about
# understanding this code
predicted <- data.frame(educ=seq(0,20))
predicted$nwifeinc <- 17.7
predicted$exper<-9
predicted$expersq<-81
predicted$age<-42
predicted$kidslt6<-.238
predicted$kidsge6<-1.35
predicted$Logit<-predict(logit.model, newdata=predicted, type="response")
predicted$Probit<-predict(probit.model, newdata=predicted, type="response")
predicted$Linear<-predict(linear.model, newdata=predicted)

subdata <- predicted[,c("educ", "Linear", "Probit","Logit")]
msd<-melt(subdata, id="educ")
ggplot(msd) + geom_line(aes(x=educ, y=value, colour=variable)) +
  scale_colour_manual(values=c("red","green","blue"), name="") +
  ggtitle("Binary Response") +
  theme(plot.title = element_text(lineheight=8, face="bold", size=26)) + 
  theme(legend.text = element_text(size=18)) +
  theme(axis.title = element_text(size=18)) + 
  theme(legend.title = element_text()) + 
  labs(x="Education", y="Probability Woman Is In Labor Force")
#ggsave('binary_response.png', width=7, height=5, units = "in")
# Complete plotting logit and probit comparison
################

# Stepwise regression
library(MASS)
start.model<-lm(wage ~ hours + IQ + KWW + educ + exper + tenure + age + married + black + south + urban + sibs, data=wages)
# Give an initial model, which will be the most coefficients we'd want to ever use
summary(start.model)
stepwise.model<- step(start.model)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit
summary(stepwise.model)

stepwise.model.interactions <- step(start.model, scope=wage~.^2)
# The command "step" adds and subtracts coefficients to maximize a measure of
# goodness of fit. 
summary(stepwise.model.interactions)
