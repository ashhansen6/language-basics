# READING AND WRITING FILES IN R #

# Packages used:
library(tidyverse)

# Since R is the main language that I code in, I'm going to fabricate a data set here
# Let's make up data for a video game streamer

data <- data.frame(matrix(nrow = 100, ncol = 2))

# Making up likert data (not generally solicited, but let's do it anyways)
likert <- sample(1:5, 100, p = c(0.05, 0.01, 0.04, 0.3, 0.6), replace = T) %>% 
  round()
hist(likert)

data[,1] <- likert # Storing to first column

# Making up "donations" data
for (i in 1:NROW(data)){
  if (data[i,1] == 1){
    donator <- round(rbinom(1, 1, 0.005))
    if (donator == 1){
      data[i,2] <- ceiling(rexp(1, 1/2))
    } else {
      data[i,2] <- 0
    }
  } else if (data[i,1] == 2){
    donator <- round(rbinom(1, 1, 0.005))
    if (donator == 0){
      data[i,2] <- ceiling(rexp(1, 1/2))
    } else {
      data[i,2] <- 0
    }
  } else if (data[i,1] == 3){
    donator <- round(rbinom(1, 1, 0.025))
    if (donator == 1){
      data[i,2] <- ceiling(rexp(1, 1/2))
    } else {
      data[i,2] <- 0
    }
  } else if (data[i,1] == 4){
    donator <- round(rbinom(1, 1, 0.10))
    if (donator == 1){
      data[i,2] <- ceiling(rexp(1, 1/5))
    } else {
      data[i,2] <- 0
    }
  } else if (data[i,1] == 5){
    donator <- round(rbinom(1, 1, 0.25))
    if (donator == 1){
      data[i,2] <- ceiling(rexp(1, 1/10))
    } else {
      data[i,2] <- 0
    }
  } 
}

## Renaming columns
colnames(data ) <- c("rating", "donations")

ggplot(data) +
  geom_histogram(aes(x = donations, fill = as.factor(rating)), bins = 10)

# Creating watch time (in minutes) data
data$watchtime <- rep(NA, 100)

for (i in 1:NROW(data)){
  if (data[i,1] <= 3){
    data$watchtime[i] <- rnorm(1, mean = 20, sd = 5)
  } else {
    data$watchtime[i] <- rnorm(1, mean = 60, sd = 20)
  }
  if (data$watchtime[i] > 120){ # Capping the stream duration to 120 min (2 hrs)
    data$watchtime[i] <- 120
  }
}

hist(data$watchtime)

# Creating chat message count

# Creating chat messages removed count

# Creating net chat messages count

# Creating 'follower' boolean

# Creating previous viewer boolean

# Creating viewer country factor
