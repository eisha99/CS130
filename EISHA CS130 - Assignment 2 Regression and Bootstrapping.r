#Note: All codes were run on R-studio (and the same libraries are not available on my Jupyter notebook, hence running errors in this doc)


set.seed(150)

#setting up independent variables

# creating normal distribution for ages 18 to 25 
age = round(rnorm(750, 21, 3.5)) 

# creating a random sample for gender (M and F)
gender= c(sample(1:2, 750, replace = T)) 

#setting up dependent variable
grades = 80+ age*0.5 - gender*10+ rnorm(750)


#setting up stochastic noise
irred_err = rnorm(750)

dataset = data.frame(age, gender, grades)

head(dataset)

#STEP 2

#fitting a linear regresseion model
reg_model <- lm(grades~age+gender)
summary(reg_model)
#The coefficients found are similar to the ones in DGP. It is 0.49 for age. I used 0.5. For gender is is -9.99 where I used -10.

#STEP 3

confint(reg_model, 'age', level=0.95)
confint(reg_model, 'gender', level=0.95)

#STEP 4

simulated_g <- sim(reg_model, n.sims = 100000)
coefficient_g <- coef(simulated_g)
coefficient_age<- coefficient_g[,2]
coefficient_gender <- coefficient_g[,3]
quantile(coefficient_age,probs=c(0.025,0.975))
quantile(coefficient_gender,probs=c(0.025,0.975))



#step5


simulatedobject = sim(reg_model, n.sims = 10000)
#storage matrix is created for predicted outcomes
storagematrix <- c() 
#The given values of 1 and -2 for variables are used
W = c(1, -2)
for (i in 1:1000){
storagematrix[i] <- exp(sum(simulatedobject@coef[i,]*c(1,W)))/
(1 + exp(sum(simulatedobject@coef[i,]*c(1,W))));
}
quantile(storagematrix, probs = c(0.005, 0.995))

#step 6

function_coefficient <- function(data,index){
    model <- lm(data[,1])[index] ~ (data[,2])[index] + (data[,3])[index])
    model$coef
}
ind <- sample (1000,1000, replace = TRUE)
function_coefficient(data1, indx)

bootstrapped <- boot(dataset, function_coefficient, R=2000)
boot.ci(boot.out= bootstrapped, type = "norm")


#Question2

#STEP1
#source=https://www.statology.org/how-to-calculate-mse-in-r/
library(dplyr)

#reading given data
housing = read.csv("https://github.com/ageron/handson-ml/raw/master/datasets/housing/housing.csv")

#splitting the data into 2 
ind <- sample(nrow(housing), size = 0.8*nrow(housing))
trainingdataset <- housing [ind,]
testingdataset <- housing [-ind,]
#model 1 and 2 as provided
model_1<- lm(median_house_value ~ housing_median_age + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)
model_2 <- lm(median_house_value ~ housing_median_age^2 + median_income + total_bedrooms + as.factor(ocean_proximity), data = housing)
#using predict function for estimating outcomes
model_prediction_1 <- predict(model_1,testingdataset)
model_prediction_2<- predict(model_2,testingdataset )
#Calculating MSE 
MSE_mod_1 <-(mean(testingdataset$median_house_value - model_prediction_1^2))
MSE_mod_2 <-(mean(testingdataset$median_house_value - model_prediction_2^2))
#printing MSE
MSE_mod_1
MSE_mod_2

#STEP2

#source: https://www.geeksforgeeks.org/loocvleave-one-out-cross-validation-in-r-programming/

library(boot)
GLM_Model_1 <- glm (median_house_value ∼ housing_median_age + median_income + total_bedrooms + as.factor(ocean_proximity) , data = housing)
cv.err1 <- cv.glm (housing , GLM_Model_1, 10)$delta
GLM_Model_2 <- glm (median_house_value ~ housing_median_age +
I(housing_median_age^2) + median_income + total_bedrooms +
as.factor(ocean_proximity), data=housing)
cv.err2 <- cv.glm (housing , GLM_Model_2, 10)$delta
cv.err1
cv.err2

#STEP 3

cv.err1 <- cv.glm (housing , GLM_Model_1, 10)$delta
cv.err2 <- cv.glm (housing , GLM_Model_2, 10)$delta

#Question 3


#STEP 1

library(boot)

#loading data and dividing into subsets
data("ToothGrowth")
ToothGrowth_org <- ToothGrowth[ToothGrowth$supp=='OJ',]
vit_dose_1 <- ToothGrowth_org[ToothGrowth_org$dose==0.5,]
vit_dose_2<- ToothGrowth_org[ToothGrowth_org$dose==2,]

# function set up
Boot_function <- function(data, indices) {
  q <- data[indices,] # allows *boot* to select sample
  fin <- lm(length(indices)*(X-Y), data=q)
  return(coef(fin))
}
# bootstrapping with 1000 replications
replicated_re <- boot(data="ToothGrowth", statistic= Boot_function,
   R=1000, formula= 1/length(indices)*(X-Y))
#the average effect can be found by subtracting the mean of dose 1 of vit with dose 2
#source:https://www.statmethods.net/advstats/bootstrapping.html

#STEP 2

#I was unable to come to a single conclusion due to a library issue (my code does not run properly). However, in general a confidence interval shows the probability that a parameter will fall between a pair of values around the mean (here the mean is the dosage of both)


#Question 4

admission <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
head(admission)

admit_variable <- glm(admit ~ ., data = admission, family = "binomial")
admit_variable


