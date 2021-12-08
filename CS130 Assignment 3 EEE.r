#obtained from assignment
set.seed(130)  
n = 1000  #number of data points

#syntax for the normal distribution here is rnorm(sample size, mean, SD)
V1 = rnorm(n, 50, 10)
#getting a binary variable
V2 = sample(c(1,0), replace = TRUE, size = n, prob = c(.6,.4))
V3 = rnorm(n, 50, 10)
#1 is new fuel, 0 is old fuel type 
D  = as.numeric(rnorm(n, .01*V1 - .75*V2 + 0.02*V3, 1) > .5)
Y  = rnorm(n, .85*D - 0.15*V2 + 0.95*V3 + 275, .4)

#combining everything in a data frame
df = data.frame(V1, V2, V3, D, Y)



#Step 3

#install relevant packages
install.packages("gridExtra")
install.packages("Matching")
 
#use libraries gridextra and matching
library(gridExtra)
library(Matching)
MatchBalance(D~V2+V3, data=df)



#Step 4

p_f_1 <- mean(df$Y[D == 1]) - mean(df$Y[D == 0])
p_f_1
 


#Step 6

#Source: This code was extracted (and modified) from CS130 Session 18 forum workbook 

propscore <- glm(data = df, formula = D ~ V2 + V3, family = "binomial")
library(Matching)
ps = propscore$fitted

matchh = Match(Y = df$Y, Tr = df$D, X = ps, M = 1)
mb = MatchBalance(data = df, formul = D ~ V2 + V3, match.out = matchh, nboots = 1000)


#Step 7
matchh$est
#output [,1]
#[1,] 1.17967

#Step 8
#sensitivity analysis 
#first install packages
install.packages("rbounds")
#use rbounds
library(rbounds) 
psens(matchh, Gamma=19, GammaInc = .1)

#Step 9
#code modified from https://nbviewer.org/gist/viniciusmss/a156c3f22081fb5c690cdd58658f61fa (shared in CS130 session 9)

#genetic matching
#install and load rgenoud
install.packages(“rgenoud”)
library(rgenoud)

#covariates V2 and V3
X <- cbind(df$V2, df$V3)
#outcome variable
Y <- df$Y
#treatment variable D
Tr <- df$D

genmout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, pop.size=100,
max.generations = 20, wait.generations = 5)
moutput <- Match(Tr=Tr, X=X, Y=Y,M=1, estimand="ATT", Weight.matrix = genmout)
mb <- MatchBalance(data = df, formul = D ~ V2 + V3, match.out = genmout,
nboots = 1000)



#Step 10

#using caliper values with previous code

genecaliper <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, pop.size=100,
max.generations = 20, wait.generations = 5, caliper = c(1e-2, 1e5))
calioutput<- Match(Tr=Tr, X=X, Y= Y, M=1, estimand="ATT", Weight.matrix =
genecaliper, caliper = c(1e-2, 1e5))
summary(calioutput)

#Step 11


usingexact <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1, pop.size=100,
max.generations = 20, wait.generations = 5, exact = c(1e-2, 1e5))
exactout<- Match(Tr=Tr, X=X, Y= Y, M=1, estimand="ATT", Weight.matrix =
usingexact, exact = c(1e-2, 1e5))
summary(exactout)

#Step 12

moutput$est




#Step 13

psens(moutput, Gamma=19, GammaInc = .1)


