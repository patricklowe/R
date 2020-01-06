# Patrick Lowe - 16725829

# open the housing CSV
Housing = read.csv(file.choose())
par(mar=c(3,3,3,3)) # this works best for my machine

######
# Q1 #
######

# Create a boxplot for housing prices
boxplot(Housing$Price)
# Create a Histogram of prices
hist(Housing$Price, freq=FALSE)
lines(density(Housing$Price),lwd=2, col="blue")
# create a summary
summary(Housing)

######
# Q2 #
######

# Convert Categorical to factors
Housing$Lot.Type.f <- factor(Housing$Lot)
Housing$Bath.Type.f <- factor(Housing$Bath)
Housing$Bed.Type.f <- factor(Housing$Bed)
#Housing$Garage.Type.f <- factor(Housing$Garage) # chosen not to treat as categorical, very well could be
Housing$School.Type.f <- factor(Housing$School)


#SUMMARY AND A BOXPLOT 
#PRICES: BEDROOMS,BATHROOMS,GARAGE,SCHOOL
boxplot(Housing$Price~Housing$Bed,data=Housing,main="Price by #Bedrooms",col="blue",border="red")
tapply(Housing$Price, Housing$Bed, summary)
boxplot(Housing$Price~Housing$Bath,data=Housing,main="Price by #Bathrooms",col="blue",border="red")
tapply(Housing$Price, Housing$Bath, summary)
boxplot(Housing$Price~Housing$Garage,data=Housing,main="Price by Garage Space",col="blue",border="red")
tapply(Housing$Price, Housing$Garage, summary)
boxplot(Housing$Price~Housing$School,data=Housing,main="Price by Schools",col="blue",border="red")
tapply(Housing$Price, Housing$School, summary)

######
# Q3 #
######
# Summary, Correlation, Pairs Plots:
#price and each of the numeric predictorvariables.

# Summary
tapply(Housing$Price, Housing$Bed, summary)

# Correlation
# step by step: cor(Housing$Price,Housing$Bed)
# Or graphically:
library(corrplot)
M <- cor(Housing[,1:7])
corrplot.mixed(M)
# Pairs Plots
pairs(Housing[,1:7])

######
# Q4 #
######
# MLR Model
mod = lm(Housing$Price ~ Housing$Size
         + Housing$Lot.Type.f
         + Housing$Bath.Type.f
         + Housing$Bed.Type.f
         + Housing$Year
         + Housing$Garage 
         + Housing$School.Type.f, 
         data=Housing)
summary(mod)

# Plot Residuals
resid(mod) #List of residuals
plot(density(resid(mod)))
qqnorm(resid(mod))
qqline(resid(mod))
library(ggplot2)
residualPlot <- ggplot(aes(x=.fitted, y=.resid),data=mod)+geom_point()+geom_hline(yintercept=0)
residualPlot

#########
# Anova #
#########

# COMPUTE THE TYPE 1 ANOVA TABLE. INTERPRET THE OUTPUT
anova(mod)
SSR <- sum(anova(mod)[1:7,2])
MSR <- SSR/7
SSE <- anova(mod)[8,2]
MSE <- anova(mod)[8,3]
test_stat <- MSR/MSE
qf = qf(0.05,22,53)

#Q3. Compute a type 2 anova table comparing the full model with all predictor
#variables to the the reduced model with the suggested predictor variable
#identified in the previous question removed.
# Reduced Model
red_mod = lm(Housing$Price ~ Housing$Size
         + Housing$Lot.Type.f
         + Housing$Bath.Type.f
         + Housing$Bed
         + Housing$Garage 
         + Housing$School.Type.f, 
         data=Housing)
Anova(red_mod)
SSR2 <- sum(anova(red_mod)[1:6,2])
MSR2 <- SSR/6
SSE2 <- anova(mod)[7,2]
MSE2 <- anova(mod)[7,3]
test_stat2 <- MSR2/MSE2
qf2 = qf(0.05,21,54)

anova(red_mod, mod)

###############
# Diagnostics #
###############
library("GGally")
library(car)
ggpairs(Housing[,1:7])
ggpairs(Housing[,c(1:1, (ncol(Housing) - 4):ncol(Housing))])
avPlots(red_mod)
crPlots(red_mod)

# random/i.i.d sample
dwt(red_mod)

# multicollinearity
gvif(red_mod)
M <- cor(Housing)
corrplot.mixed(M)

# Zero Conditional Mean & Homoskedasticity
plot(fitted(red_mod),rstudent(red_mod))
abline(h=0)
plot(Housing$Size,rstudent(red_mod),main="Studentized Res: Size",col="blue")
plot(Housing$Lot,rstudent(red_mod),main="Studentized Res: Lot",col="blue")
plot(Housing$Bath,rstudent(red_mod),main="Studentized Res: Bath",col="blue")
plot(Housing$Bed,rstudent(red_mod),main="Studentized Res: Bed",col="blue")
plot(Housing$Garage,rstudent(red_mod),main="Studentized Res: Garage",col="blue")
plot(Housing$School,rstudent(red_mod),main="Studentized Res: School",col="blue",border="red")

# normality
hist(rstudent(red_mod),freq=FALSE)
lines(density(rstudent(red_mod)), lwd=2, col="blue")
qqnorm(rstudent(red_mod))
qqline(rstudent(red_mod))

# Leverage points
library(olsrr)
lev = hat(model.matrix(red_mod))
plot(lev,main="Leverage Points",col="blue")
# highlighting high lieverage points
plot(Housing$Size,Housing$Price,main="Leverage Points (Price/Size)",col="blue")
points(Housing[4,]$Size,Housing[4,]$Price,col="red")
points(Housing[5,]$Size,Housing[5,]$Price,col="red")
points(Housing[6,]$Size,Housing[6,]$Price,col="red")
points(Housing[21,]$Size,Housing[21,]$Price,col="red")
points(Housing[37,]$Size,Housing[37,]$Price,col="red")
points(Housing[74,]$Size,Housing[74,]$Price,col="red")

# Influential Points
cook = cooks.distance(red_mod)
plot(cook,ylab="Cooks distances")
which(cook>0.08)
plot(cook,ylab="Cooks distances",main="Influential",col="blue")
points(41,cook[41],col="red")
points(73,cook[73],col="red")

# plot both points
plot(Housing$Size,Housing$Price,main="Leverage:Red & Influential: Green (Price/Size)",col="blue")
points(Housing[4,]$Size,Housing[4,]$Price,col="red")
points(Housing[5,]$Size,Housing[5,]$Price,col="red")
points(Housing[6,]$Size,Housing[6,]$Price,col="red")
points(Housing[21,]$Size,Housing[21,]$Price,col="red")
points(Housing[37,]$Size,Housing[37,]$Price,col="red")
points(Housing[74,]$Size,Housing[74,]$Price,col="red")
points(Housing[41,]$Size,Housing[41,]$Price,col="green")
points(Housing[73,]$Size,Housing[73,]$Price,col="green")

# Outliers
outlierTest(red_mod)
CD=cooks.distance(red_mod)
CD[as.numeric(which(CD> 0.05))]

#PLOT PRICES, FITTED VALUE
new.prices <- data.frame(Housing$Price)
CI <- predict(red_mod, newdata = new.prices, interval = "confidence")
PI <- predict(red_mod, newdata = new.prices, interval = "predict")

plotdata <- data.frame(Housing$Price,CI[,1:3],PI[,2:3])
names(plotdata)[names(plotdata) == "lwr"] <- "CI lwr"
names(plotdata)[names(plotdata) == "upr"] <- "CI upr"
names(plotdata)[names(plotdata) == "lwr.1"] <- "PI lwr"
names(plotdata)[names(plotdata) == "upr.1"] <- "PI upr"

plotdata[,1]=round(plotdata[,1],2)
plotdata[,2]=round(plotdata[,2],2)
plotdata[,3]=round(plotdata[,3],2)
plotdata[,4]=round(plotdata[,4],2)
plotdata[,5]=round(plotdata[,5],2)
plotdata[,6]=round(plotdata[,6],2)

library(ggplot2)
U <- plotdata$`CI upr`
L <- plotdata$`CI lwr`
U2 <- plotdata$`PI upr`
L2 <- plotdata$`PI lwr`

ggplot(plotdata, aes(x = fit, y = Housing.Price)) +
  geom_point() +
  geom_errorbar(aes(ymax = U, ymin = L),colour = "red") +
  geom_ribbon(aes(ymin = L2, ymax = U2), fill = "blue", alpha = 0.1) +
  geom_smooth(method = "lm", se = FALSE, colour = "green")

