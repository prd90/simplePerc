# simplePerc
A single layer perceptron in R
CS 5331-02 Data Analysis and Machine Learning with R

Package tested with Sonar Data set in mlbench package
Please follow these steps to load and work with Sonar data:

library(mlbench)
data(Sonar)
s= Sonar
s$Class <- as.numeric(s$Class)
s$Class[s$Class==2] <- 0 #Rock
s$Class[s$Class==1] <- 1 #Mine

More details on CRAN SITE:
https://cran.r-project.org/web/packages/mlbench/mlbench.pdf
