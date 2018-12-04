## Author: NiranjanKumar 

library(ggplot2)

#============================================================================================================
#OBJECTIVE: Analysis of Model Over Fitting due to increase Model Complexity
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
#=============================================================================================
# OVERFITTING DUE TO INCREASE IN ORDER OF POLYNOMIAL REGRESSION SAMPLE SIZE 20
#=============================================================================================

##Getting 4 Different Samples of Data from training Data - Samples of size 20.
set.seed(2)

##Taking Random Samples with out replacement 4 times for 4 different samples
sample_data_list_20 <- lapply(1: 4, function(i) train_data[sample(1:nrow(train_data),20),])

##============================================================================================
## TAKING FIRST SAMPLE 
sample_train_data <- sample_data_list_20[[1]]

# FITTING THE MODEL OF ORDER 1
m1 <- lm(total_purchase_value ~ clicks, sample_train_data)

# FITTING THE MODERL OF ORDER 2 
m2 <- lm(total_purchase_value ~ clicks + I(clicks^2), sample_train_data)

# FITTING THE MODERL OF ORDER 7
m7 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7), sample_train_data)

# FITTING THE MODERL OF ORDER 8
m8 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7) + I(clicks^8), sample_train_data)

# FITTING THE MODERL OF ORDER 9
m9 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7) + I(clicks^8) + I(clicks^9), sample_train_data)

# FITTING THE MODERL OF ORDER 10
m10 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7) + I(clicks^8) + I(clicks^9)+ I(clicks^10), sample_train_data)

lst_models = list(m1, m2, m7, m8, m9, m10)
model_complexity = c()

##For Loop For Saving Graphs into a Plots Folder
for (i in lst_models){
  #Getting the order number from model
  order_num = nrow(summary(i)$coefficients)-1
  
  model_complexity <- c(model_complexity, order_num)
  
  #Concatinating the Graph Name with Order Number
  graph_name = paste(c("Order",order_num,"Sample-20"),collapse = "_")

  #Concatinating the Graph Name with Folder Name - Plots
  graph_name_foldername = paste(c("Plots",graph_name),collapse = '/')
  #print(graph_name_foldername)
  
  #Name for Title of Plot
  main_title_1 = paste("Polynomial Regression of Order-", order_num, sep = "")
  main_title = paste(main_title_1,"(Sample Size = 20)")
  
  #Saving the Plot Name to JPEG Format
  jpeg(paste(c(graph_name_foldername,'jpg'),collapse = '.'))
  
  #PLOTTING THE MODELS OVER THE DATA 
  plot(sample_train_data$clicks, sample_train_data$total_purchase_value, pch = 19, cex = 0.5, xlab = "Number of Clicks", 
       ylab = "Total Purchase value", main = main_title)
  lines(sort(sample_train_data$clicks), fitted(i)[order(sample_train_data$clicks)], col = 'magenta', type = 'l', pch = 20)
  
  dev.off()
  
}

#SAVING THE GRAPH TO A FOLDER
png("Plots/Different_Orders_Sample-20.png")

#PLOTTING THE MODELS OVER THE DATA
plot(sample_train_data$clicks, sample_train_data$total_purchase_value, pch = 19, cex = 0.5, xlab = "Number of Clicks", 
     ylab = "Total Purchase value", main = "Polynomial Regression of Different Orders (Sample Size = 20)")
lines(sort(sample_train_data$clicks), fitted(m1)[order(sample_train_data$clicks)], col = 'magenta', type = 'l',lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m2)[order(sample_train_data$clicks)], col = 'darkgreen', type = 'l',lwd =  2, pch = 20)
lines(sort(sample_train_data$clicks), fitted(m7)[order(sample_train_data$clicks)], col = 'red', type = 'l', lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m8)[order(sample_train_data$clicks)], col = 'blue', type = 'l', lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m9)[order(sample_train_data$clicks)], col = 'brown', type = 'l',lwd =  2, pch = 20)
lines(sort(sample_train_data$clicks), fitted(m10)[order(sample_train_data$clicks)], col = 'orange', type = 'l',lwd =  2, pch = 20)

# Add a legend
legend("topright", legend=c("Order-1", "Order-2","Order-7", "Order-8","Order-9","Order-10"), 
       col=c("magenta", "darkgreen","red","blue","brown","orange"),lty=1, cex=0.8, text.font = 4, bg = "lightblue",
       title = "Different orders")

dev.off()

##============================================================================================

#For Loop to Calculate Train RSS and Test RSS
df_rss_s20 = data.frame(complexity = model_complexity)


#Looping through all the samples of same size, N = 20
for (i in sample_data_list_20){
  sample_train_data = i
  
  train_rss_vec = c() # To Store Training RSS
  test_rss_vec = c() # To Store Testing RSS
  
  # FITTING THE MODEL OF ORDER 1
  m1 <- lm(total_purchase_value ~ clicks, sample_train_data)
  
  # FITTING THE MODERL OF ORDER 2 
  m2 <- lm(total_purchase_value ~ clicks + I(clicks^2), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 7
  m7 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 8
  m8 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7) + I(clicks^8), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 9
  m9 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7) + I(clicks^8) + I(clicks^9), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 10
  m10 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
            + I(clicks^7) + I(clicks^8) + I(clicks^9)+ I(clicks^10), sample_train_data)
  
  lst_models = list(m1, m2, m7, m8, m9, m10)
  
  for (j in lst_models){
    
    #TRAIN AND TEST ACCURACY 
    train_rss <- sum(j$residuals^2) #Training Error
    pred = predict(j, newdata=test_data)
    test_error = sum((pred-test_data$total_purchase_value)^2) #Test Error
    
    train_rss_vec <- c(train_rss_vec, train_rss)
    test_rss_vec <- c(test_rss_vec, test_error)
  }
  
  df_rss_s20 <- cbind(df_rss_s20, train_rss_vec, test_rss_vec)
}
##============================================================================================
## GRAPH BETWEEN TEST RSS, TRAINING RSS AND MODEL COMPLEXITY - FOR DIFFERENT SAMPLES OF SAME SIZE - 20

colnames(df_rss_s20) <- c("com", "train_s20_1", 'test_s20_1','train_s20_2','test_s20_2','train_s20_3', 'test_s20_3','train_s20_4','test_s20_4')

#FOR SAMPLE - 1, TEST RSS 
ggplot(df_rss_s20, aes(x = com, y = test_s20_1, color = "Sample-1")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 20)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))


#FOR SAMPLE - 2, TEST RSS 
ggplot(df_rss_s20, aes(x = com, y = test_s20_2, color = "Sample-2")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 20)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#2E8B57"))

#FOR SAMPLE - 3, TEST RSS 
ggplot(df_rss_s20, aes(x = com, y = test_s20_1, color = "Sample-3")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 20)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#00008B"))

#FOR SAMPLE - 4, TEST RSS 
ggplot(df_rss_s20, aes(x = com, y = test_s20_1, color = "Sample-4")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 20)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#C71585"))


##============================================================================================
## GRAPH BETWEEN TEST RSS, TRAINING RSS AND MODEL COMPLEXITY - SAMPLE SIZE N = 20

#ADDING MODEL COMPLEXITY, TRAIN RSS AND TEST RSS INTO DATAFRAME
df <- data.frame(complexity = model_complexity, train_rss_name = sqrt(train_rss_vec), test_rss_name = sqrt(test_rss_vec))

#ADDING COMPLEXITY
g <- ggplot(df, aes(x = complexity, y = test_rss_name, color ="TestRMSE"))

#ADDING TEST RSS
g <- g + geom_line(size  = 1.2)

#ADDING TRAIN RSS
g <- g + geom_line(aes(y = train_rss_name, color = "TrainRMSE"),size = 1.2)

g <- g + labs(color = "RMSE")

#ADDING X-TITLE AND Y_TITLE AND MAIN TITLE
g <- g + xlab("Model Complexity (Order of Polynomial)") + ylab("Error(RMSE)") + 
  ggtitle("Model Error vs Model Complexity (Sample Size = 20)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))

g

#=============================================================================================
#=============================================================================================
# OVERFITTING DUE TO INCREASE IN ORDER OF POLYNOMIAL REGRESSION SAMPLE SIZE 100
#=============================================================================================

##Getting 4 Different Samples of Data from training Data - Samples of size 100.
set.seed(3)

##Taking Random Samples with out replacement 4 times for 4 different samples
sample_data_list_100 <- lapply(1: 4, function(i) train_data[sample(1:nrow(train_data),100),])

##============================================================================================
## TAKING FIRST SAMPLE 
sample_train_data <- sample_data_list_100[[1]]

# FITTING THE MODEL OF ORDER 1
m1 <- lm(total_purchase_value ~ clicks, sample_train_data)

# FITTING THE MODERL OF ORDER 2 
m2 <- lm(total_purchase_value ~ clicks + I(clicks^2), sample_train_data)

# FITTING THE MODERL OF ORDER 7
m7 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7), sample_train_data)

# FITTING THE MODERL OF ORDER 8
m8 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7) + I(clicks^8), sample_train_data)

# FITTING THE MODERL OF ORDER 9
m9 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
         + I(clicks^7) + I(clicks^8) + I(clicks^9), sample_train_data)

# FITTING THE MODERL OF ORDER 10
m10 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
          + I(clicks^7) + I(clicks^8) + I(clicks^9)+ I(clicks^10), sample_train_data)

lst_models = list(m1, m2, m7, m8, m9, m10)
model_complexity = c()

##For Loop For Saving Graphs into a Plots Folder
for (i in lst_models){
  #Getting the order number from model
  order_num = nrow(summary(i)$coefficients)-1
  
  model_complexity <- c(model_complexity, order_num)
  
  #Concatinating the Graph Name with Order Number
  graph_name = paste(c("Order",order_num,"Sample-100"),collapse = "_")
  
  #Concatinating the Graph Name with Folder Name - Plots
  graph_name_foldername = paste(c("Plots",graph_name),collapse = '/')
  #print(graph_name_foldername)
  
  #Name for Title of Plot
  main_title_1 = paste("Polynomial Regression of Order-", order_num, sep = "")
  main_title = paste(main_title_1,"(Sample Size = 100)")
  
  #Saving the Plot Name to JPEG Format
  jpeg(paste(c(graph_name_foldername,'jpg'),collapse = '.'))
  
  #PLOTTING THE MODELS OVER THE DATA 
  plot(sample_train_data$clicks, sample_train_data$total_purchase_value, pch = 19, cex = 0.5, xlab = "Number of Clicks", 
       ylab = "Total Purchase value", main = main_title)
  lines(sort(sample_train_data$clicks), fitted(i)[order(sample_train_data$clicks)], col = 'magenta', type = 'l', pch = 20)
  
  dev.off()
  
}


#SAVING THE GRAPH TO A FOLDER
png("Plots/Different_Orders_Sample-100.png")

#PLOTTING THE MODELS OVER THE DATA
plot(sample_train_data$clicks, sample_train_data$total_purchase_value, pch = 19, cex = 0.5, xlab = "Number of Clicks", 
     ylab = "Total Purchase value", main = "Polynomial Regression of Different Orders (Sample Size = 100)")
lines(sort(sample_train_data$clicks), fitted(m1)[order(sample_train_data$clicks)], col = 'magenta', type = 'l',lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m2)[order(sample_train_data$clicks)], col = 'darkgreen', type = 'l',lwd =  2, pch = 20)
lines(sort(sample_train_data$clicks), fitted(m7)[order(sample_train_data$clicks)], col = 'red', type = 'l', lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m8)[order(sample_train_data$clicks)], col = 'blue', type = 'l', lwd =  2,pch = 20)
lines(sort(sample_train_data$clicks), fitted(m9)[order(sample_train_data$clicks)], col = 'brown', type = 'l',lwd =  2, pch = 20)
lines(sort(sample_train_data$clicks), fitted(m10)[order(sample_train_data$clicks)], col = 'orange', type = 'l',lwd =  2, pch = 20)

# Add a legend
legend("topleft", legend=c("Order-1", "Order-2","Order-7", "Order-8","Order-9","Order-10"), 
       col=c("magenta", "darkgreen","red","blue","brown","orange"),lty=1, cex=0.8, text.font = 4, bg = "lightblue",
       title = "Different orders")

dev.off()

##============================================================================================

#For Loop to Calculate Train RSS and Test RSS
df_rss_s100 = data.frame(complexity = model_complexity)


#Looping through all the samples of same size, N = 20
for (i in sample_data_list_100){
  sample_train_data = i
  
  train_rss_vec = c() # To Store Training RSS
  test_rss_vec = c() # To Store Testing RSS
  
  # FITTING THE MODEL OF ORDER 1
  m1 <- lm(total_purchase_value ~ clicks, sample_train_data)
  
  # FITTING THE MODERL OF ORDER 2 
  m2 <- lm(total_purchase_value ~ clicks + I(clicks^2), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 7
  m7 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 8
  m8 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7) + I(clicks^8), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 9
  m9 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
           + I(clicks^7) + I(clicks^8) + I(clicks^9), sample_train_data)
  
  # FITTING THE MODERL OF ORDER 10
  m10 <- lm(total_purchase_value ~ clicks + I(clicks^2) + I(clicks^3) + I(clicks^4) + I(clicks^5) + I(clicks^6) 
            + I(clicks^7) + I(clicks^8) + I(clicks^9)+ I(clicks^10), sample_train_data)
  
  lst_models = list(m1, m2, m7, m8, m9, m10)
  
  for (j in lst_models){
    
    #TRAIN AND TEST ACCURACY 
    train_rss <- sum(j$residuals^2) #Training Error
    pred = predict(j, newdata=test_data)
    test_error = sum((pred-test_data$total_purchase_value)^2) #Test Error
    
    train_rss_vec <- c(train_rss_vec, train_rss)
    test_rss_vec <- c(test_rss_vec, test_error)
  }
  
  df_rss_s100 <- cbind(df_rss_s100, train_rss_vec, test_rss_vec)
}
##============================================================================================
## GRAPH BETWEEN TEST RSS, TRAINING RSS AND MODEL COMPLEXITY - FOR DIFFERENT SAMPLES OF SAME SIZE - 20

colnames(df_rss_s100) <- c("com", "train_s100_1", 'test_s100_1','train_s100_2','test_s100_2','train_s100_3', 'test_s100_3','train_s100_4','test_s100_4')

#FOR SAMPLE - 1, TEST RSS 
ggplot(df_rss_s100, aes(x = com, y = test_s100_1, color = "Sample-1")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 100)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))


#FOR SAMPLE - 2, TEST RSS 
ggplot(df_rss_s100, aes(x = com, y = test_s100_2, color = "Sample-2")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 100)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#2E8B57"))

#FOR SAMPLE - 3, TEST RSS 
ggplot(df_rss_s100, aes(x = com, y = test_s100_3, color = "Sample-3")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 100)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#00008B"))

#FOR SAMPLE - 4, TEST RSS 
ggplot(df_rss_s100, aes(x = com, y = test_s100_4, color = "Sample-4")) + geom_point(size = 2) + 
  geom_line(size = 1.2) + labs(color = "Sample") + xlab("Model Complexity (Order of Polynomial)") +
  ylab("TestError(RMSE)") + ggtitle("Model Error vs Model Complexity (Sample Size = 100)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15)) + scale_color_manual(values=c("#C71585"))


##============================================================================================
## GRAPH BETWEEN TEST RSS, TRAINING RSS AND MODEL COMPLEXITY - SAMPLE SIZE N = 100

#ADDING MODEL COMPLEXITY, TRAIN RSS AND TEST RSS INTO DATAFRAME
df <- data.frame(complexity = model_complexity, train_rss_name = sqrt(train_rss_vec), test_rss_name = sqrt(test_rss_vec))

#ADDING COMPLEXITY
g <- ggplot(df, aes(x = complexity, y = test_rss_name, color ="TestRMSE"))

#ADDING TEST RSS
g <- g + geom_line(size  = 1.2)

#ADDING TRAIN RSS
g <- g + geom_line(aes(y = train_rss_name, color = "TrainRMSE"),size = 1.2)

g <- g + labs(color = "RMSE")

#ADDING X-TITLE AND Y_TITLE AND MAIN TITLE
g <- g + xlab("Model Complexity (Order of Polynomial)") + ylab("Error(RMSE)") + 
  ggtitle("Model Error vs Model Complexity (Sample Size = 100)") + 
  theme(axis.title.x = element_text(color = 'Blue', size = 13),
        axis.title.y = element_text(color = 'Blue', size = 13),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 15))

g




