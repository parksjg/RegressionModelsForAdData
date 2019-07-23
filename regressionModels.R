# Prepare the data
library(faraway)

set.seed(689934)
df = read.csv("~/Downloads/ad_report.csv", header = TRUE, sep = ",")
summary(df)
#head(df)
#df
df = df[-c(1,2),]
df = df[,-c(16,17)]
df = df[,-c(1,2,6)]
df = df[,-c(7,11,12)]

#head(df)


names(df) = c("reach", "impressions", "freq", "results", "CPR", "spent", "linkClicks", "CPC", "CTR")

df = df[,c(4,1,2,3,5,6,7,8,9)]
head(df)

plot(df, main="Plot of the Response and Predictors")
summary(df)

test_df = read.csv("~/Downloads/ad2_report.csv", header = TRUE, sep = ",")
test_df = test_df[-c(1),]
test_df = test_df[,-c(16,17)]
test_df = test_df[,-c(1,2,6)]
test_df = test_df[,-c(7,11,12)]
names(test_df) = c("reach", "impressions", "freq", "results", "CPR", "spent", "linkClicks", "CPC", "CTR")
test_df = test_df[,c(4,1,2,3,5,6,7,8,9)]
#test_df = test_df[,-c(7)]
head(test_df)

lmod = lm(results ~ ., data = df)
summary(lmod)
par(mfrow = c(2,2))
plot(lmod)

vif(lmod)

##===============================================================================================
# Multiple Linear Regression Model
mseMLR = mean((df$results - predict(lmod, df))^2)
mseMLR

# Prediction with test data
mseTest1 = mean((test_df$results - predict(lmod, test_df))^2)
mseTest1

#lmodTEST = lm(results ~ ., data=test_df)
#summary(lmodTEST)
#plot(lmodTEST)

#mseTEST = mean((test_df$results - predict(lmodTEST, test_df))^2)
#mseTEST

## Taking out point 48
df2 = df[-46,]
lmod2 = lm(results ~ ., data = df2)
summary(lmod2)

par(mfrow = c(2,2)) 
plot(lmod2)

par(mfrow = c(1,1)) 
halfnorm(residuals(lmod2))

mseMLR2 = mean((df2$results - predict(lmod2, df2))^2)
mseMLR2

# Prediction with test data
mseMLR2 = mean((test_df$results - predict(lmod2, test_df))^2)
mseMLR2

## Taking out point 48 and 53
df3 = df[-c(46,51),]
lmod3 = lm(results ~ ., data = df3)
summary(lmod3)

par(mfrow = c(2,2)) 
plot(lmod3)

par(mfrow = c(1,1)) 
halfnorm(residuals(lmod3))

mseMLR3 = mean((df3$results - predict(lmod3, df3))^2)
mseMLR3

# Prediction on test data
mseTest3 = mean((test_df$results - predict(lmod3, test_df))^2)
mseTest3

##===============================================================================================
## General linear model
glmod = glm(results ~ ., data = df)
summary(glmod)
par(mfrow = c(2,2))
plot(glmod)

##===============================================================================================
## General linear model with points 48 and 53 taken out
glmod2 = glm(results ~ ., data = df3)
summary(glmod2)
par(mfrow = c(2,2))
plot(glmod2)

##===============================================================================================
## General linear model with points 48 and 53 taken out, and only CPR, spent, linkClicks, and CPC
glmod3 = glm(results ~ CPR + spent + linkClicks + CPC, data = df3)
summary(glmod3)
par(mfrow = c(2,2))
plot(glmod3)

pred_errorGLM = sum((exp(predict(glmod3)) - df3$results)^2); pred_errorGLM

##===============================================================================================
## GAM model
#install.packages("mgcv", dependencies = TRUE)
library(mgcv)
library(ggplot2)
modGAM <- gam(results ~ s(reach) + s(impressions) + s(freq) + s(CPR) + s(spent) + s(linkClicks) + s(CPC) + s(CTR), data=df)

res <- residuals(modGAM, type="deviance") #compute the deviance residuals
#res <- residuals(modGAM) #compute the deviance residuals

#residual and QQ plot
par(mfrow = c(1,1))
plot(log(predict(modGAM, type = "link")), res)
abline(h=0, lty=2)
qqnorm(res)
qqline(res)
par(mfrow = c(2,4))
plot(modGAM) 

#fitted vs actual
ggplot(df,aes(exp(predict(modGAM)),results)) + geom_point(shape=1) + geom_abline(slope=1)

par(mfrow = c(2,2))
gam.check(modGAM)
summary(modGAM)
pred_errorGAM = sum(predict(modGAM)-df$results^2); pred_errorGAM
#pred_errorGLM = sum((exp(predict(glmod)) - df$results)^2); pred_errorGLM

##===============================================================================================
modGAM2 <- gam(results ~ s(CPR) + s(spent) + CPC + s(linkClicks), data=df3)
res2 <- residuals(modGAM2, type="deviance") #compute the deviance residuals
#residual and QQ plot
par(mfrow = c(1,1))
plot(log(predict(modGAM2, type = "link")), res2, main="Fitted Values vs Residuals", ylab="Deviance Residuals")
abline(h=0, lty=2)
qqnorm(res2)
qqline(res2)
par(mfrow = c(3,1))
plot(modGAM2) 

#fitted vs actual
ggplot(df3,aes(exp(predict(modGAM2)),results)) + geom_point(shape=1) + geom_abline(slope=1)
par(mfrow = c(2,2))
gam.check(modGAM2)
summary(modGAM2)
errorGAM2 = mean((predict(modGAM2)-df3$results)^2); errorGAM
pred_errorGLM = sum((exp(predict(glmod)) - df3$results)^2); pred_errorGLM

mseGAM2 = mean((df3$results - predict(modGAM2, df3))^2)
mseGAM2

##===============================================================================================
## AIC
#install.packages("leaps", dependencies = TRUE)
library(leaps)
n = length(df$results)
#n
b = regsubsets(results ~ ., data=df, nvmax = 8)
rs = summary(b)
aic = n*log(rs$rss/n) + 2*(2:9)
i = which.min(aic)

dataAIC = df[, rs$which[i,]]
lmodAIC = lm(results ~ ., data = dataAIC)
summary(lmodAIC)

par(mfrow = c(2,2))
plot(lmodAIC) 


vif(lmodAIC)

mseAIC = mean(((df$results) - (predict(lmodAIC, dataAIC)))^2)
mseAIC

##===============================================================================================
## AIC with points 48 and 53 taken out
n2 = length(df3$results)
#n
b2 = regsubsets(results ~ ., data=df3, nvmax = 8)
rs2 = summary(b2)
aic2 = n*log(rs2$rss/n) + 2*(2:9)
i2 = which.min(aic2)

dataAIC2 = df3[, rs2$which[i2,]]
lmodAIC2 = lm(results ~ ., data = dataAIC2)
summary(lmodAIC2)

par(mfrow = c(2,2))
plot(lmodAIC2) 

vif(lmodAIC2)

mseAIC2 = mean(((df3$results) - (predict(lmodAIC2, dataAIC2)))^2)
mseAIC2

##===============================================================================================
## PCA and PCR
s = prcomp(df[,-1], scale=TRUE)
summary(s)
par(mfrow = c(1,1))
plot(s$sdev, type = "o", xlab="Principal Components", ylab="Standard Deviation", main="Amount of Variation Explained by each PC")

pc = data.frame(s$x)
pc = pc[,-c(7,8)]
pc$results = df$results
#pc

lmodPCR = lm(results ~ ., data=pc)
summary(lmodPCR)

par(mfrow = c(2,2))
plot(lmodPCR)

vif(lmodPCR)

msePCR = mean((pc$results - fitted(lmodPCR))^2)
msePCR


##===============================================================================================
## PCA and PCR with points 48 and 53 taken out
s2 = prcomp(df3[,-1], scale=TRUE)
summary(s2)
plot(s2$sdev, type = "o")

pc2 = data.frame(s2$x)
#pc = pc[,-c(7,8)]
pc2$results = df3$results
#pc

lmodPCR2 = lm(results ~ ., data=pc2)
summary(lmodPCR2)

vif(lmodPCR2)

msePCR2 = mean((pc2$results - fitted(lmodPCR2))^2)
msePCR2


##===============================================================================================
##===============================================================================================
##===============================================================================================

## Baysian Linear Regression


library(rstan)
library(shinystan)
options(mc.cores = parallel::detectCores())
### The STAN model ###
rstan_options(auto_write = TRUE)
#install.packages("mvtnorm",dependencies = TRUE)
library(mvtnorm)

set.seed(689934)
summary(df3)

#lmod = lm(df$Y ~ df$var1 + df$var3, data = df)
#summary(lmod)
#plot(lmod)

#r = residuals(lmod)

#plot(density((r-mean(r))/sqrt(var(r))), xlim=range(c(-3,3)), ylim=range(c(0,0.4)))
#plot(density(r))

N <- length(df3$results)
results <- df3$results
reach <- df3$reach
impressions <- df3$impressions
freq <- df3$freq
CPR <- df3$CPR
spent <- df3$spent
linkClicks <- df3$linkClicks
CPC <- df3$CPC
CTR <- df3$CTR

# Model matrix (with column of 1s for intercept and three covariates)
X <- cbind(Const = 1, X1 = reach, X2 = impressions, X3 = freq, X4 = CPR, X5 = spent, X6 = linkClicks, X7 = CPC, X8 = CTR)
K <- ncol(X)

stan_code <- '
data {
int           N ; // integer, number of observations
int           K ; // integer, number of columns in model matrix
matrix[N,K]   X ; // N by K model matrix
vector[N]     results ; // vector of N observations
}

parameters {
real<lower=0> sigma ; // real number > 0, standard deviation
vector[K]     beta ;  // K-vector of regression coefficients
}

model {
beta ~ normal(0, 5) ;       // prior for betas
sigma ~ cauchy(1, 2) ;    // prior for sigma
results ~ normal(X*beta, sigma) ; // vectorized likelihood
}

generated quantities {
// Here we do the simulations from the posterior predictive distribution

vector[N] y_rep_reg ; // vector of same length as the data y
vector[N] y_rep_normal;
vector[N] y_rep_err;
for (n in 1:N) {
  y_rep_normal[n] = normal_rng(X[n]*beta, sigma);
  y_rep_reg[n] = X[n]*beta;
  y_rep_err[n] = X[n]*beta + normal_rng(0, sigma);
}

}
'

# Prepare the data we'll need as a list
stan_data <- list(results = results, X = X, N = N, K = K)

# Fit the model
stanfit <- stan(model_code = stan_code, data = stan_data, iter=10000, chains=4, seed=483892929, refresh=2000)

summary(stanfit)
get_posterior_mean(stanfit)
plot(stanfit, show_density = TRUE, ci_level = 0.5, fill_color = "purple")

posterior <- extract(stanfit, include = T)

#yrep <- posterior_predict(posterior)
mean(apply(posterior$y_rep_normal, 2, median) == results)
mean(apply(posterior$y_rep_reg, 2, mean) == results)
mean(apply(posterior$y_rep_err, 2, median) == results)
mean(posterior$y_rep_reg[1,] == results)

mseB = mean((df3$results - posterior$y_rep_normal)^2)
mseB

library(MASS)
truehist(results, col="#B2001D")
lines(density(results), col="skyblue", lwd=2)
summary(results)
#plot(results)

## y_rep_normal[n] = normal_rng(X[n]*beta, sigma);
truehist(posterior$y_rep_normal, 50, col="#B2001D")
lines(density(posterior$y_rep_normal), col="skyblue", lwd=2)
lines(density(results), col="green", lwd=2)
summary(posterior$y_rep_normal[,1])
#plot(posterior$y_rep_normal[3,])

## y_rep_reg[n] = X[n]*beta;
truehist(posterior$y_rep_reg, 50, col="#B2001D")
lines(density(posterior$y_rep_reg), col="skyblue", lwd=2)
lines(density(results), col="green", lwd=2)
summary(posterior$y_rep_reg[,1])

## y_rep_err[n] = X[n]*beta + normal_rng(0, sigma);
truehist(posterior$y_rep_err[,8], col="#B2001D")
lines(density(posterior$y_rep_err[,8]), col="skyblue", lwd=2)
lines(density(results), col="green", lwd=2)
summary(posterior$y_rep_err[,8])


#posterior$y_rep

# Launch ShinyStan
launch_shinystan(stanfit)
