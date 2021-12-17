#This is the Code for our final assignment CS130

#The Code was run in Rstudio and Rconsole yet saved in Jupyter for easy sharing


#Extension Part-> Genetic Matching

#loading and viewing the data
load("/Users/eisha/Downloads/Bail et al PNAS 2018.rdata")
View(twitter_data)


#install packages 
install.packages("rgenoud") 
nstall.packages("Matching") 

#loading packages
library(rgenoud)
library(Matching)

attach(twitter_data)

#omitting any data that is NA
twitter_data2 <- na.omit(twitter_data)
#using new data
attach(twitter_data2)


#Defining treatment variable, Y and X
Tr <- treat
Y <- ideology_seven_point_wave_5
X <- cbind(caseid, gender, education, family_income, birth_year, ideo_homogeneity_offline, north_central, northeast, south, west, percent_co_party, friends_count_wave_1, political_wave_1, freq_twitter_wave_1, bin_maker)

#performing genetic matching
genout <- GenMatch(Tr=Tr, X=X, estimand="ATT", M=1,pop.size=100, max.generations=10, wait.generations=1, print.level=0)

mout <- Match(Y=Y, Tr=Tr, X=X, estimand="ATT", Weight.matrix=genout)

mb  <- MatchBalance(treat~gender+education+family_income+birth_year+ northeast+south+west, twitter_data2, match.out=mout, print.level=0)


#printing the result
print(mb)


