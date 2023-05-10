#install.packages('boot',dep=TRUE)
library(boot)
# Load dplyr 
library('dplyr')
library(ggplot2)

mydata.SB <- read.csv("upstream_problem10_05-08-2023_01_19_10.csv");
mydata.NB <- read.csv("downstream_problem10_05-08-2023_01_19_10.csv");

mydata_df.SB <- mydata.SB %>% select("X01.J1":"X53.J47");
mydata_df.NB <- mydata.NB %>% select("X01.J1":"X53.J47");
head(mydata_df.SB)

# Define the function that calculates the metric of interest - mean speed on the 
# motorway M1 from J1 - J48
average_speed <- function(data, i){
  d2 <- data[i,] 
  return(rowMeans(d2))
}

# Set the seed for repeatability
set.seed(412)
# Using Random Sampling with Replacement
upstream.bootstrap <- boot(mydata_df.SB,average_speed,R=10000);
downstream.bootstrap <- boot(mydata_df.NB,average_speed,R=10000);
upstream.bootstrap
downstream.bootstrap
# Display the summary of the sampling of the upstream average speed
summary(upstream.bootstrap)

# Display the summary of the sampling of the upstream average speed
summary(downstream.bootstrap)

# Compute the statistics of the sampling process
print(range(upstream.bootstrap$t))
print(mean(upstream.bootstrap$t))
print(sd(upstream.bootstrap$t))

# Confidence Interval of the SouthBound average Speed
boot.ci(boot.out=upstream.bootstrap,type=c('norm','basic','perc','bca'))


# Compute the statistics of the sampling process
print(range(downstream.bootstrap$t))
print(mean(downstream.bootstrap$t))
print(sd(downstream.bootstrap$t))

# Confidence Interval of the NorthBound average Speed
boot.ci(boot.out=downstream.bootstrap,type=c('norm','basic','perc','bca'))

# Plot the SouthBound Average speed Histogram
plot(upstream.bootstrap)
hist(rowMeans(upstream.bootstrap$t), xlab='Average Speed [km/hr]',
     main='Histogram of Average Speed on M1 Motorway SB')
abline(v=mean(upstream.bootstrap$t), lty=4, lwd=2, col='red' )

hist(rowMeans(upstream.bootstrap$data), xlab='Average Speed [km/hr]',
     main='Histogram of Raw Average Speed on M1 Motorway SB')
abline(v=mean(rowMeans(upstream.bootstrap$data)), lty=4, lwd=2, col='red' )

# Plot the NorthBound Average speed Histogram
plot(downstream.bootstrap)

hist(rowMeans(downstream.bootstrap$t), xlab='Average Speed [km/hr]',
     main='Histogram of Average Speed on M1 Motorway NB')
abline(v=mean(downstream.bootstrap$t), lty=4, lwd=2, col='red' )

hist(rowMeans(downstream.bootstrap$data), xlab='Average Speed [km/hr]',
     main='Histogram of Raw Average Speed on M1 Motorway NB')
abline(v=mean(rowMeans(downstream.bootstrap$data)), lty=4, lwd=2, col='red' )


