#####################################################################
#                  Data Used : Red White Wine Quality                     #
#####################################################################
#options(rgl.useNULL = TRUE)
library(rgl)
library (ggplot2)
library(qpcR) #has PRESS residuals
library(rgl)
library(robustbase)
library(MPV)
wine <- read.csv("wine-quality-white-and-red 2.csv")
wine
print(is.data.frame(wine))
cat('The number of columns in the data is:',ncol(wine),'\n')
cat('The number of rows in the data is:',nrow(wine))

y = wine$quality
x1 = wine$fixed.acidity
x2 = wine$volatile.acidity
x3 = wine$citric.acid
x4 = wine$residual.sugar
x5 = wine$chlorides
x6 = wine$free.sulfur.dioxide
x7 = wine$total.sulfur.dioxide
x8 = wine$density
x9 = wine$pH
x10 = wine$sulphates
x11 = wine$alcohol
x12 = wine$type
wine

res = lm(y~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11)
summary(res)
anova(res)

press = PRESS(res)
SST = sum(anova(res)['Sum Sq'])
R.pred1 = 1 - (press/SST)
R.pred1
# The value of R^2 prediction is 0.2890, which means that we can expect
# our model to explain about 34.83% of the variability in predicting new
# observations.


########################## Adequacy Test ##################################

# Further we can plot the model diagnostic checking for other problems such 
# as normality of error term etc
Stud = rstudent(res)
qqnorm(Stud)
qqline(Stud, col = "steelblue", lwd = 2)
plot(res$fitted.values, Stud)
plot(res)           #will give the clear illustration
# From the plots, data seems to have constant variance
# The qq plot shows a slight deviation from normality.
# We will try boxcox transformation to fix that.

# Small departures from the normality assumption do not affect the model greatly.

library(MASS)
bs <- boxcox(res)
# We see from the graph that the value of lambda is 1, which corresponds to
# a transformation of y-1, which is nothing but a translation, and won't
# make any difference. So we don't go for transformation.

# Checking for outliers
# we don't see any outliers in the residual plots. So we also check for
# outliers using Cook's distance.
D1=cooks.distance(res)
which(D1 >1) 

#no outliers


## checking for X1 variable
dfq1<- mutate(wine ,bin=ntile(x1,30))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X2 variable
dfq1<- mutate(wine ,bin=ntile(x2,40))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X3 variable
dfq1<- mutate(wine ,bin=ntile(x3,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

## parabola transformation

## checking for X4 variable
dfq1<- mutate(wine ,bin=ntile(x4,10))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop +stat_summary(fun = mean, geom = "line", col ="red" ,aes(group=1))

## checking for X5 variable
dfq1<- mutate(wine ,bin=ntile(x5,40))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X6 variable
dfq1<- mutate(wine ,bin=ntile(x6,50))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X7 variable
dfq1<- mutate(wine ,bin=ntile(x7,50))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X8 variable
dfq1<- mutate(wine ,bin=ntile(x8,50))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X9 variable
dfq1<- mutate(wine ,bin=ntile(x9,50))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X10 variable
dfq1<- mutate(wine ,bin=ntile(x10,50))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop

## checking for X11 variable
dfq1<- mutate(wine ,bin=ntile(x11 , 100))
crop=ggplot(data=dfq1, mapping=aes(x=bin, y=wine$quality,group = bin)) + geom_boxplot()
crop





############################# Multicollinearity ##########################
data = wine[,2:12]
library(corrplot)
correlations = cor(data)
corrplot(correlations,method = "number")


res = cor(data)
res
solve(cor(data))

VIF=diag(solve(cor(data)))
VIF



############################ polynomial ############################

#check for the quadratic
x=wine[,2:12]
colnames(x)=c('x1','x2','x3','x4','x5','x6','x7','x8','x9','x10','x11')
quad_x = cbind(x^2)
colnames(quad_x)=c('x1_sq','x2_sq','x3_sq','x4_sq','x5_sq','x6_sq','x7_sq',
                   'x8_sq','x9_sq','x10_sq','x11_sq')
quad_x =cbind(x,quad_x)
# Let's check the correlation 

cor(quad_x)
# another way to check the multicollinearity is VIF
VIF=diag(solve(cor(quad_x)))
VIF

VIF=diag(solve(cor(x)))
VIF

library(corrplot)
correlations = cor(quad_x)
corrplot(correlations)
corrplot

# high correlation
# The correlation matrix shows that the correlation among all the 
# explanatory variables is not very high though there are some values which
# are near to 1. 
# center the data and check again.


cx=sweep(wine[,2:12],2, FUN='-',apply(wine[,2:12],2,mean))
colnames(cx)=c('cx1','cx2','cx3','cx4','cx5','cx6','cx7','cx8',
               'cx9','cx10','cx11')
quad_cx=cbind(cx^2)
colnames(quad_cx)=c('cx1_sq','cx2_sq','cx3_sq','cx4_sq','cx5_sq','cx6_sq','cx7_sq',
                    'cx8_sq','cx9_sq','cx10_sq','cx11_sq')
quad_cx =cbind(cx,quad_cx)   #matrix of cx and cx^2

cor(quad_cx)   
# another way to check the multicollinearity is VIF
VIF=diag(solve(cor(quad_cx)))
VIF

correlations = cor(quad_cx)
corrplot(correlations)
corrplot

# after centering the data correlation  decreased, so let's fit the new model 
cx1 = x1 - mean(x1)
cx2 = x2 - mean(x2)
cx3 = x3 - mean(x3)
cx4 = x4 - mean(x4)
cx5 = x5 - mean(x5)
cx6 = x6 - mean(x6)
cx7 = x7 - mean(x7)
cx8 = x8 - mean(x8)
cx9 = x9 - mean(x9)
cx10 = x10 - mean(x10)
cx11 = x11 - mean(x11)
cx1_sq = cx1^2
cx2_sq = cx2^2
cx3_sq = cx3^2
cx4_sq = cx4^2
cx5_sq = cx5^2
cx6_sq = cx6^2
cx7_sq = cx7^2
cx8_sq = cx8^2
cx9_sq = cx9^2
cx10_sq = cx10^2
cx11_sq = cx11^2


regLC = lm(y~cx1 + cx2 + cx3 + cx4 + cx5 + cx6 + cx7 + cx8 + cx9 + cx10 + cx11)
summary(regLC)
anova(regLC)


center_data = cbind(cx1 , cx2 , cx3 , cx4 , cx5 , cx6 , cx7 , cx8 , cx9 , cx10 , cx11)

VIF=diag(solve(cor(center_data)))
VIF


# But R^2 = 0.2921 which is same as above, so we'll go for polynomial model
regLC2 = lm(y~cx1 + cx2 + cx3 + cx4 + cx5 + cx6 + cx7 + cx8 + cx9 + cx10 + cx11+
              cx1_sq+cx2_sq+cx3_sq + cx4_sq + cx5_sq + cx6_sq + cx7_sq + 
              cx8_sq + cx9_sq + cx10_sq + cx11_sq+cx1:cx2 +cx1:cx3+cx1:cx4+
              cx1:cx5 +cx1:cx6 +cx1:cx7 +cx1:cx8 +cx1:cx9 +cx1:cx10 +cx1:cx11+
              cx2:cx3 +cx2:cx4+cx2:cx5+cx2:cx6+cx2:cx7+cx2:cx8+cx2:cx9+
              cx2:cx10+cx2:cx11+cx3:cx4+ cx3:cx5+ cx3:cx6+ cx3:cx7+ cx3:cx8+
              cx3:cx9+ cx3:cx10+ cx3:cx11+ cx4:cx5 + cx4:cx6+ cx4:cx7+ cx4:cx8+
              cx4:cx9+ cx4:cx10+ cx4:cx11+ cx5:cx6+ cx5:cx7+ cx5:cx8+ cx5:cx9+ 
              cx5:cx10+ cx5:cx11+ cx6:cx7+ cx6:cx8+ cx6:cx9+ cx6:cx10+ cx6:cx11+
              cx7:cx8 +cx7:cx9+cx7:cx10+cx7:cx11+cx8:cx9+cx8:cx10+cx8:cx11+
              cx9:cx10 +cx9:cx11+cx10:cx11)
summary(regLC2) 
anova(regLC2)

#new R^2 value is 0.3612



############## Variable selection Forward ############

# Forward Selection
res0 = lm(y~ 1, data= wine)
res0
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data =wine, test = "F") 
# add x11 because lowest AIC and smallest p val and Highest F val

res1 = lm(y~ x11, data= wine)
add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x2 so we'll 
# add x2
summary(res1)

res2 = lm(y~ x11+x2, data= wine)
add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x10 so we'll 
# add x10
summary(res2)

res3 = lm(y~ x2+x10+x11, data= wine)
add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x4 so we'll 
# add x4
summary(res3)

res4 = lm(y~ x2+x4+x10+x11, data= wine)
add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x7 so we'll 
# add x7
summary(res4)

res5 = lm(y~ x2+x4+x7+x10+x11, data= wine)
add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x6 so we'll 
# add x6
summary(res5)

res6 = lm(y~x2+x4+x6+x7+x10+x11, data= wine)
add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x5 so we'll 
# add x5
summary(res6)

res7 = lm(y~ x2+x4+x5+x6+x7+x10+x11, data= wine)
add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x5 so we'll 
# add x5
summary(res7)

res8 = lm(y~ x2+x4+x5+x6+x7+x10+x11+x9, data= wine)
# now Smallest AIC, smallest p and highest F val  is for x9 so we'll 
# add x9
add1(res8, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
summary(res8)

##final model after forward selection: 
res8 = lm(y~ x2+x4+x5+x6+x7+x10+x11+x9, data= wine)



##############Back Propagation ############

# Backward selection
# Backward selection
res0 = lm(y~ x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x1 though AIC is small but p value is largest
# and F value is smallest

# Backward selection
res0 = lm(y~ x2+x3+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x3 though AIC is small but p value is largest
# and F value is smallest

# Backward selection
res0 = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res0,y~x2+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x9 though AIC is small but p value is largest
# and F value is smallest

#final model after backward slection at alpha 0.1
res0 = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)



###################### Step-wise regression #######################
# Step-wise selection
res0 = lm(y~ 1, data=wine) 
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# x11 has smallest p largest F and smallest AIC add x11
res1 = lm(y~ x11, data= wine) 
# let's see do we need to drop x11 or not 
drop1(res1, y~x11, data = wine, test = "F")# no need to drop


add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res2 = lm(y~ x2+x11, data= wine) 
drop1(res2, y~x2+x11, data = wine, test = "F")# no need to drop

add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res3 = lm(y~ x2+x11+x10, data= wine) 
drop1(res3, y~x2+x11+x10, data = wine, test = "F")# no need to drop

add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res4 = lm(y~ x2+x11+x10+x4, data= wine) 
drop1(res4, y~x2+x11+x10+x4, data = wine, test = "F")# no need to drop

add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res5 = lm(y~ x2+x11+x10+x4+x7, data= wine) 
drop1(res5, y~x2+x11+x10+x4+x7, data = wine, test = "F")# no need to drop

add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res6 = lm(y~ x2+x11+x10+x4+x7+x6, data= wine) 
drop1(res6, y~x2+x11+x10+x4+x7+x6, data = wine, test = "F")# no need to drop

add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res7 = lm(y~ x2+x11+x10+x4+x7+x6+x5, data= wine) 
drop1(res7, y~x2+x11+x10+x4+x7+x6+x5, data = wine, test = "F")# no need to drop

add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
res9 = lm(y~ x2+x11+x10+x4+x7+x6+x5+x9, data= wine) 
drop1(res9, y~x2+x11+x10+x4+x7+x6+x5+x9, data = wine, test = "F")# no need to drop

add1(res9, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
#we wont anything now

# the final model is  y~ x2+x11+x10+x4+x7+x6+x5+x9

## exhaustive search:
library(leaps)
all <- regsubsets(x=cbind(x2,x4,x5,x6,x7,x9,x10,x11), y=y,  method = "exhaustive", all.best = FALSE, nbest = 6)
summary(all)
Cp <- summary(all)$cp
AdjR2 <- summary(all)$adjr2
SSRes <- summary(all)$rss
R2 <- summary(all)$rsq
Matrix <- summary(all)$which

p <- apply(Matrix,1, sum)
MSRes <- SSRes/(13-p)

# Make a nice table
output <- cbind(p, Matrix, SSRes, R2, AdjR2, MSRes, Cp)
colnames(output)[3:6] <- c("x2", "x4", "x5", "x6" , "x7" , "x9", "x10","x11") 
output



# Sanity check: fit the last model (with all 8 parameters) by hand and compare the output you get:
fit = lm(y~ x2+x4+x5+x6+x7+x9+x10+x11, data= wine)
summary(fit)

# Obtain least squares parameter estimates for all models
coef(all, 1:43)







# all VIF values are less than 10
#correlation between x and x^2 after centering
# after centering the data correlation  decreased, so let's fit the new model 
cx1 = x1 - mean(x1)
cx2 = x2 - mean(x2)
cx3 = x3 - mean(x3)
cx4 = x4 - mean(x4)
cx5 = x5 - mean(x5)
cx6 = x6 - mean(x6)
cx7 = x7 - mean(x7)
cx8 = x8 - mean(x8)
cx9 = x9 - mean(x9)
cx10 = x10 - mean(x10)
cx11 = x11 - mean(x11)



regLC = lm(y1~cx1 + cx2 + cx3 + cx4 + cx5 + cx6 + cx7 + cx8 + cx9 + cx10 + cx11)
summary(regLC)
anova(regLC)

cbind()


# But R^2 = 0.3606 which is same as above, so we'll go for polynomial model
regLC2 = lm(y1~cx1 + cx2 + cx3 + cx4 + cx5 + cx6 + cx7 + cx8 + cx9 + cx10 + cx11+
              cx1_sq+cx2_sq+cx3_sq + cx4_sq + cx5_sq + cx6_sq + cx7_sq + 
              cx8_sq + cx9_sq + cx10_sq + cx11_sq+cx1:cx2 +cx1:cx3+cx1:cx4+
              cx1:cx5 +cx1:cx6 +cx1:cx7 +cx1:cx8 +cx1:cx9 +cx1:cx10 +cx1:cx11+
              cx2:cx3 +cx2:cx4+cx2:cx5+cx2:cx6+cx2:cx7+cx2:cx8+cx2:cx9+
              cx2:cx10+cx2:cx11+cx3:cx4+ cx3:cx5+ cx3:cx6+ cx3:cx7+ cx3:cx8+
              cx3:cx9+ cx3:cx10+ cx3:cx11+ cx4:cx5 + cx4:cx6+ cx4:cx7+ cx4:cx8+
              cx4:cx9+ cx4:cx10+ cx4:cx11+ cx5:cx6+ cx5:cx7+ cx5:cx8+ cx5:cx9+ 
              cx5:cx10+ cx5:cx11+ cx6:cx7+ cx6:cx8+ cx6:cx9+ cx6:cx10+ cx6:cx11+
              cx7:cx8 +cx7:cx9+cx7:cx10+cx7:cx11+cx8:cx9+cx8:cx10+cx8:cx11+
              cx9:cx10 +cx9:cx11+cx10:cx11)
summary(regLC2) 
anova(regLC2)


# We can see that most of the cxi_sq  and interaction terms are not 
# significant and R^2 is 0.4346 which is little improved.
# 
# But, We cannot say that the model is bad with small R^2 , there might be some
# other factors which might increase the R^2.
# let's check whether there are outliers or not in cxi, cxi_sq 
# where i = 1,2,....,11 using cook's distance

D2=cooks.distance(regLC2)
which(D2 >1) #no outliers

# let's  plot them and check the adequacy of the model
Stud = rstudent(regLC2)
qqnorm(Stud)
qqline(Stud) # looks like same as above that is no improvement
plot(regLC2$fitted.values, Stud) 

#Since most of the quadratic terms are not significant, we will reduce
#the model.

reduced.regLC2 = lm(y1~cx2+cx3+cx5+cx6+cx7+cx9+cx10+cx11+cx1_sq+cx4_sq+
                      cx9_sq+cx10_sq+cx11_sq+cx1:cx5+cx1:cx8+cx2:cx7+
                      cx2:cx11+cx3:cx9+cx3:cx11+cx5:cx8+cx6:cx8+
                      cx6:cx10+cx6:cx11+cx7:cx8+cx7:cx10+cx7:cx11+
                      cx8:cx9+cx9:cx11)

summary(reduced.regLC2)

#We see an R^2 value of 0.4112, and Residual standard error of 0.6252,
# which is a slight improvement from the full model.

#Reducing the model further
reduced.regLC2.1 = lm(y~cx2+cx3+cx5+cx6+cx7+cx9+cx10+cx11+cx1_sq+
                        cx9_sq+cx10_sq+cx1:cx5+cx1:cx8+cx2:cx7+
                        cx3:cx9+cx3:cx11+cx6:cx8+
                        cx6:cx10+cx6:cx11+cx7:cx8+cx7:cx11)

summary(reduced.regLC2.1)

#We get an R^2 value of 0.4099 and Residual Standard error as 0.6245
#We see that there is not much improvement in the R^2 or standard error value.

# Since even the reduced model contains a lot of variables, the model tends
# to be complex. So we will try stepwise regression for variabe selection on
# original data, as always a simpler regression model is preferred.
# We can then compare the two models.



############## Variable selection check(step-wise regression) ############

# Forward Selection
res0 = lm(y~ 1, data= wine)
res0
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data =wine, test = "F") 
# add x11 because lowest AIC and smallest p val and Highest F val

res1 = lm(y~ x11, data= wine)
add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x2 so we'll 
# add x2
summary(res1)

res2 = lm(y~ x11+x2, data= wine)
add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x10 so we'll 
# add x10
summary(res2)

res3 = lm(y~ x2+x10+x11, data= wine)
add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x4 so we'll 
# add x4
summary(res3)

res4 = lm(y~ x2+x4+x10+x11, data= wine)
add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x7 so we'll 
# add x7
summary(res4)

res5 = lm(y~ x2+x4+x7+x10+x11, data= wine)
add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x6 so we'll 
# add x6
summary(res5)

res6 = lm(y~x2+x4+x6+x7+x10+x11, data= wine)
add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# now Smallest AIC, smallest p and highest F val  is for x5 so we'll 
# add x5
summary(res6)

res7 = lm(y~ x2+x4+x5+x6+x7+x10+x11, data= wine)
add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
summary(res7)

res8 = lm(y~ x2+x4+x5+x6+x7+x10+x11+x9, data= wine)
add1(res8, y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
summary(res8)

# now p value for x1,x3, x4 and x8 are larger than 0.1 which is not
# good so will stop here
# So x2,x5,x6,x7,x9,x10 and x11 are good candidate

# Using forward selection reduced model will be
# y = 4.4300987 -1.0127527*x2 -2.0178138*x5 +0.0050774*x6 -0.0034822*x7 
#   -0.4826614 *x9+ 0.8826651 *x10+ 0.2893028*x11
# with Residual standard error: 0.6477 on 1591 degrees of freedom
# Multiple R-squared:  0.3595,	Adjusted R-squared:  0.3567 
# F-statistic: 127.6 on 7 and 1591 DF,  p-value: < 2.2e-16


####################### Backward Selection ##################
# Backward selection
res0 = lm(y~ x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data= wine)
drop1(res0,y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
# first beta we don't want is x8 though AIC is small but p value is largest
# and F value is smallest

res1 = lm(y~ x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res1,y~x1+x2+x3+x4+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# 2nd term we don't want is x4

res3 = lm(y~ x1+x2+x3+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res3,y~x1+x2+x3+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# 3rd term we don't want is x1

res4 = lm(y~ x2+x3+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res4,y~x2+x3+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
# 3rd term we don't want is x3

res5 = lm(y~ x2+x5+x6+x7+x9+x10+x11, data= wine)
drop1(res5,y~x2+x5+x6+x7+x9+x10+x11, data = wine, test = "F")
summary(res5)
# we'll stop here because all p-values are small
# Using backward selection reduced model will be
# y = 4.4300987 -1.0127527*x2 -2.0178138*x5 +0.0050774*x6 -0.0034822*x7 
#   -0.4826614 *x9+ 0.8826651 *x10+ 0.2893028*x11
# with Residual standard error: 0.6477 on 1591 degrees of freedom
# Multiple R-squared:  0.3595,	Adjusted R-squared:  0.3567 
# F-statistic: 127.6 on 7 and 1591 DF,  p-value: < 2.2e-16
# Which is exactly same as For forward selection

###################### Step-wise regression #######################
# Step-wise selection
res0 = lm(y~ 1, data=wine) 
add1(res0, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
# x11 has smallest p largest F and smallest AIC add x11
res1 = lm(y~ x11, data= wine) 
# let's see do we need to drop x11 or not 
drop1(res1, y~x11, data = wine, test = "F")# no need to drop


add1(res1, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res2 = lm(y~ x2+x11, data= wine) 
drop1(res2, y~x2+x11, data = wine, test = "F")# no need to drop

add1(res2, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res3 = lm(y~ x2+x10+x11, data= wine) 
drop1(res3, y~x2+x10+x11, data = wine, test = "F")# no need to drop

add1(res3, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res4 = lm(y~ x2+x7+x10+x11, data= wine) 
drop1(res4, y~x2+x7+x10+x11, data = wine, test = "F")# no need to drop

add1(res4, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res5 = lm(y~ x2+x5+x7+x10+x11, data= wine) 
drop1(res5, y~x2+x5+x7+x10+x11, data = data, test = "F") # no need to drop

add1(res5, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res6 = lm(y~ x2+x5+x7+x9+x10+x11, data= wine) 
drop1(res6, y~x2+x5+x7+x9+x10+x11, data = wine, test = "F") # no need to drop

add1(res6, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res7 = lm(y~ x2+x5+x6+x7+x9+x10+x11, data= wine) 
drop1(res7, y~x2+x5+x6+x7+x9+x10+x11, data = wine, test = "F") # no need to drop

add1(res7, y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11, data = wine, test = "F")
res8 = lm(y~ x2+x3+x5+x6+x7+x9+x10+x11, data= wine) 
drop1(res8, y~x2+x3+x5+x6+x7+x9+x10+x11, data = wine, test = "F") 
# need to drop x3 
# so the reduced model will be 
red_linear_model = lm(y~ x2+x5+x6+x7+x9+x10+x11) 
summary(red_linear_model)
anova(red_linear_model)
# Using step-wise selection reduced model will be
# y = 4.4300987 -1.0127527*x2 -2.0178138*x5 +0.0050774*x6 -0.0034822*x7 
#   -0.4826614 *x9+ 0.8826651 *x10+ 0.2893028*x11
# with Residual standard error: 0.6477 on 1591 degrees of freedom
# Multiple R-squared:  0.3595,	Adjusted R-squared:  0.3567 
# F-statistic: 127.6 on 7 and 1591 DF,  p-value: < 2.2e-16
# Which is exactly same as  forward  and backward selection

#Calculating R^2 prediction
press = PRESS(red_linear_model)$stat
SST = sum(anova(red_linear_model)['Sum Sq'])
R.pred2 = 1 - (press/SST)

# The value of R^2 prediction is 0.3515267, which means that we can expect
# our model to explain about 35.15% of the variability in predicting new
# observations.

# let's do the adequacy checks
######################### Adequacy Check ##########################
Stud = rstudent(red_linear_model)
qqnorm(Stud)
qqline(Stud, col = "steelblue", lwd = 2)
plot(red_linear_model$fitted.values, Stud)
plot(red_linear_model)
#The residuals & fitted plot seems to be quite different from the one 
# from normal regression, Because the dependent variable is 
# discrete.
################# Multicollinearity#########################

x.red=cbind(x2,x5,x6,x7,x9,x10,x11)
cor(x.red)
# a clear case of  no multicollinearity.
# However if there is no such value,  we can't say 
# that there is no multicollinearity because, they can be depend on each other

# another way to check the multicollinearity is VIF
VIF=diag(solve(cor(x.red)))
VIF
#  if VIF < 10 then there is no  multicollinearity
######################## Outliers ########################
D3=cooks.distance(red_linear_model)
which(D3 >1)
# As we can see, there seems not to be an influential outlier in 
# the plot of residuals & leverage plot above.

####################### Confidence Interval ####################
confint(red_linear_model)
#For all variables, the confidence interval does not include zero. 
# This implies that all variables are reliable.

