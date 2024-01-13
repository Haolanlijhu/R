library(foreign)  # Read data from different statistical packages
library(lmtest)   # Post-estimation commands
library(car)      # Companion to Applied Regression
library(dynlm)      # Dynamic Linear Regression
library(sandwich)   # Robust Covariance Matrix Estimators
library(stargazer)       # For the stargazer table output
library(AER)             # For the ivregress command

# Import Data
D <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/hprice1.dta")

#Descriptive analysis
summary(D$price)
summary(D[c("price","sqrft")])

#histogram
hist(D$price, main = "Histogram of Housing Price", xlab = "Housing Price, $1000s") 
#scatterplot
plot(D$price, D$sqrft, main="Housing Price and Size", xlab="Housing Price, $1000s", ylab="Square Footage")
grid()
#diagrammatic group
par(mfrow = c(1,3))  # Tell R you want a 1-by-3 figure 
hist(D$price, breaks = 10, main = "Histogram of Housing Price", xlab = "Housing Price, $1000s") 
hist(D$sqrft, breaks = 10, main = "Histogram of Housing Size", xlab = "Square Footage") 
plot(D$price, D$sqrft, main="Housing Price and Size", xlab="Housing Price, $1000s", ylab="Square Footage")
grid()

#Confidence Intervals
ybar  <- mean(D$price)    
s     <- sd(D$price)      
n     <- length(D$price)  
df    <- n-1              
se    <- s/sqrt(n)        
t     <- qt(.975, df)
(CI   <- c(ybar-t*se, ybar+t*se))

#hypothesis testing
mu    <- 300
tstat <- (ybar-mu)/se
alpha <- 0.05 
crt   <- qt(1-(alpha/2), df)
abs(tstat)>crt

# p-value
p    <- pt(abs(tstat),df)
(pval  <- 2*(1-p))

#singal commend to do all the above
(test  <- t.test(D$price, mu=300, conf.level=0.95))

#Linear Regression
firstols <- lm(price ~ sqrft, data=D)
summary(firstols)
muliols <- lm(price ~ sqrft + bdrms, data=D)
summary(muliols)
#log-log
summary(lm(log(price) ~ log(nox) + radial + crime + rooms, data = D))
#log-level
summary(lm(log(price) ~ nox + radial + crime + rooms, data = D))
#level-log
summary(lm(price ~ log(nox) + radial + crime + rooms, data = D))
# two-stage
firststage  <- lm(price ~ sqrft + bdrms, data=D)
D$v <- resid(firststage)
summary(lm(price ~ v, data=D))
#confidence interval
confint(myols) #95%
#Wald test
unrestricted_ols <- lm(lprice ~ lnox + radial + crime + rooms, data = D) 
restricted_ols1 <- lm(lprice ~ 1, data = D)
waldtest(restricted_ols1,unrestricted_ols,test = "Chisq")

#Nonlinear Models
rm(list=ls())
D <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/cps91.dta")
summary(lm(log(hrwage) ~ log(educ) + age + I(age^2), data = D, subset = educ>0)) #quadratic term
summary(lm(log(hrwage) ~ log(educ) + age + I(age^2) + black + hispanic, data = D, subset = educ>0)) # Dummy varibales
summary(lm(log(hrwage) ~ (black + hispanic)*log(educ) + age + I(age^2), data = D, subset = educ>0)) #interations

#Heteroskedasticity
rm(list=ls())
load("ARE106-movingoffthefarm.RData")
attach(D)
#White test
m2 <- lm(agl ~ I(1/pci))
coeftest(m2)
e2 <- (resid(m2))^2                       
m2_het <- lm(e2 ~ I(1/pci) + I(1/pci^2))  
R2 <- summary(m2_het)$r.squared           
N <- dim(D)                               
W <- N[1]*R2                              
c <- qchisq(1-.05,2)                      
sprintf("White Stat = %f. Crit Val = %f. Reject null of homoskedasticy: %s", W,c,as.character(W>c))
#Breusch-Pagan”(BP) test
bptest(m2, ~ I(1/pci) + I(1/pci^2))
#White’s robust standard errors
X <- 1/pci                           
x2 <- (X-mean(X))^2                   
den <- (sum(x2))^2                  
estse <- (sum(e2*x2)/den)^0.5        
sprintf("White robust standard error for b1 = %f", estse)
#easy way to generate White’s robust standard errors
var2 <- hccm(m2, type="hc0")
coeftest(m2, vcov = var2)

#Correlated Errors
rm(list=ls())
load("ARE106-oil_rea.RData")
attach(D)
#convert to time-series object
rea <- ts(D[,"rea"], start = 1974, frequency = 12)
rpo <- ts(D[,"rpo"], start = 1974, frequency = 12)\
#time series
mod1 <- dynlm(rpo ~ rea)
coeftest(mod1)
sd(rea)
b1 <- coefficients(mod1)["rea"]
sprintf("The response of oil prices to a one standard deviation change in `rea` is %f", sd(rea)*b1)
#Breusch-Godfrey test
e <- resid(mod1)                    
mod1_aux <- dynlm(e ~ L(e) + rea)   
summary(mod1_aux)
T <- dim(rpo)[1]                    
BG.test <- (T-1)*summary(mod1_aux)$r.squared  
c <- qchisq(1-.05,1)               
sprintf("Breusch-Godrey Stat = %f. Crit Val = %f. Reject null of homoskedasticy: %s", BG.test,c,as.character(BG.test>c))
# easy way to do Breusch-Godfrey test
bgtest(mod1)
#Newey-West corrected standard errors
coeftest(mod1, vcov = NeweyWest(mod1, lag = 15))
coeftest(mod1, vcov = vcovHAC)

#Sample Selection
rm(list=ls())
D <- read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/mroz.dta")
attach(D)
sel1 <- lm(inlf ~ educ  + exper + I(exper^2))
coeftest(sel1)
I1 <- predict(sel1)
heck1 <- lm(log(wage) ~ educ + exper + I(exper^2) + I1)
coeftest(heck1)
sel2 <- lm(inlf ~ educ + exper + I(exper^2) + nwifeinc + age + kidslt6 + kidsge6)
coeftest(sel2)
I2 <- predict(sel2)
heck2 <- lm(log(wage) ~ educ + exper + I(exper^2) + I2)
coeftest(heck2) 
stargazer(list(heck2,m1), type="text")

#IV
#2SLS
first.stage <- lm(educ ~ fatheduc + exper + I(exper^2), data = D)
summary(first.stage)
educ_pred <- fitted(first.stage)
iv <- lm(log(wage) ~ educ_pred + exper + I(exper^2), data = D)
stargazer(ols2, iv, type="text")
# 2SLS in one step
iv2 <- ivreg(log(wage) ~ educ + exper + I(exper^2) | exper + I(exper^2) + fatheduc, data = D)
stargazer(ols2, iv, iv2, type="text")
