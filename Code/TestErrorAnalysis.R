## Author: NiranjanKumar 

library(ggplot2)

#============================================================================================================
#OBJECTIVE: Analysis of Test Error (RMSE) for increase in Sample Size - Polynomial Regression of order 7 and 9
#============================================================================================================

## DATA
set.seed(0)

# Loading the data
region_data = read.csv("Data/region_view.csv")

head(region_data) #Top 5 rows of data

# Names of all columns in the data
names(region_data)

# Dimensions of the data
dim(region_data)

## Splitting the data - Test Observations 50 and Train Observations 1800
rand_data = sample(1:nrow(region_data),50)

test_data = region_data[rand_data,]  #Test data has 50 observations
train_data = region_data[-rand_data,] #Train data has 1800 observations

#=============================================================================================
# ANALYSING TEST ERROR FOR INCREASE IN NUMBER OF OBSERVATIONS FOR POLYNOMIAL REGRESSION OF ORDER 7
#=============================================================================================

## ANALYSING THE TEST ERROR FOR DIFFERENT SIZES OF TRAINING DATA. 
## WE WILL FIT POLYNOMIAL REGRESSION OF ORDER 7 FOR DIFFERENT SAMPLE SIZES 

sample_size = c()  # An empty vector to keep track of all different sample sizes
test_error_vec = c() # An empty vector to keep track of test error for different sample sizes

sizes = c(20, 40, 60, 80, 100, 200, 300, 400, 500, 800)


for (i in sizes){
  train_sample_data = train_data[sample(nrow(train_data),i),]
  
  # FITTING THE MODEL OF ORDER 7
  
  m7 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7), train_sample_data)
  
  
  #TRAIN AND TEST ACCURACY 
  sum(m7$residuals^2) #Training Error
  pred = predict(m7, newdata=test_data)
  test_error = sum((pred-test_data$total_purchase_value)^2) #Test Error
  
  #ADDING SAMPLE SIZE AND TEST ERROR TO VECTORS
  sample_size <- c(sample_size, i)
  test_error_vec <- c(test_error_vec, test_error)
}

#======================================================
##PLOTTING OF GRAPH

png("Plots/TestingError_SampleSize.png")


plot(sample_size, sqrt(test_error_vec), xlab = "Sample Size", ylab = "Test Error (RMSE)",
     pch = 19, cex = 0.5, main = "Test Error vs Sample Size for a Polynomial Regression of Order - 11")
lines(sample_size,sqrt(test_error_vec), col = 'blue', type = 'l', lwd = 1.5)

dev.off()

#======================================================================================================
#REPEAT ABOVE FOR ORDER 9 MODEL
