#################################################################
## Marketing analytics                                         ##
## Lecture 3 Exercise in Individual Market Response Models	   ##
## Logistic Regression                                         ##
## admittance Data Exercise                                    ##
##                                                             ## 
#################################################################

# free memory
#rm(list = ls())
#gc()


# Load files from a directory and merge them to one file
#getwd() 
#setwd("C:/test/")


# Read the Data
admittanceData<-read.csv("admittance.csv",header=T)

#or use web link to get the data
admittanceData <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
 

# Show the first few rows of the data
head(admittanceData)

with(admittanceData, table(admit,rank))

#Show attributes  
attributes(admittanceData)

# draw the data
plot(admittanceData) 
with(admittanceData, plot( gpa,gre, ylab="gpa", xlab="gre"))
 

# Fit the Data to the model
# mydata$rank <- factor(mydata$rank)
# mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
model <- glm(admit ~ gpa + gre, family = "binomial", data=admittanceData)
summary(model)


#Model attributes and coefficients  
attributes(model)
model$coefficients

# Now we get the individual risk factors (or odds ratios).
# The 2 at the end is the number of decimals we want.
round(exp(coef(model)), 2) 



#Find the admitance for gpa 3, gre 660  
admitance660and3 <- coef(model)[[1]]+coef(model)[[2]]*3+coef(model)[[3]]*660

#Find the probability of admitting chance
probofadmit<-round((exp(admitance660and3)/(exp(admitance660and3)+1)), 2) 
probofadmit


#Alternatively, finding the admitting chance  
datatopredict<-data.frame(gre=660:663, gpa= 1:4)
predict(model, datatopredict, type = "response")

 
 
# Show the plot of the model 
plot(model)
  

# Compare Different Models
model1 <- glm(admit ~ gpa + gre , data=admittanceData)
summary(model1)

#convert rank to a factor to indicate that rank should be treated as a categorical variable
admittanceData$rank <- factor(admittanceData$rank)
model2 <- glm(admit ~ gpa + gre + rank , data=admittanceData)
summary(model2)


#Plot Observed Values vs Predicted Values
pred <- predict(model, type="response")
plot(admittanceData$admit, pred, xlab="Observed Values", ylab="Predicted Values")
abline(a=0, b=1)
 
