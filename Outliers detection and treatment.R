#Removing the objects saved in the console
rm(list = ls())

#Setting the work directory
setwd("C:/Users/Sharath P Dandamudi/Desktop/Titanic")

# Inject outliers into data.
cars1 <- cars[1:30, ]  # original data
cars_outliers <- data.frame(speed=c(19,19,20,20,20), dist=c(190, 186, 210, 220, 218))  # introduce outliers.
cars2 <- rbind(cars1, cars_outliers)  # data with outliers

# Look at summary() of data
summary(cars2)

# View histogram of the dist variable
hist(cars2$dist)

# View boxplot of dist variable
boxplot(cars2$dist)

#Capping outlier values - OUTLIER TREATMENT 1
cars2$dist
qnt <- quantile(cars2$dist, probs=c(.25, .75), na.rm = T)
caps <- quantile(cars2$dist, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(cars2$dist, na.rm = T)
H

cars2$dist[cars2$dist < (qnt[1] - H)] <- caps[1]
cars2$dist[cars2$dist > (qnt[2] + H)] <- caps[2]

summary(cars2)

#Removing outlier values - OUTLIER TREATMENT 2

# Get indices of outliers
dist_outlier_index <- which(cars2$dist > (qnt[2] + H) | cars2$dist < (qnt[1] - H))
dist_outlier_index

str(dist_outlier_index)

cars2_wo_outliers <- cars2[-dist_outlier_index, ]

#Replacing outlier values with mean/median- OUTLIER TREATMENT 2

dist_outlier_index <- which(cars2$dist > (qnt[2] + H) | cars2$dist < (qnt[1] - H))
dist_outlier_index

str(dist_outlier_index)

# Make copy of data
cars2_replace <- cars2

# Compute the median of dist
median_dist <- median(cars2$dist, na.rm = TRUE)

# Replace outlier values with median
cars2_replace$dist[dist_outlier_index] <- median_dist

summary(cars2_replace)



