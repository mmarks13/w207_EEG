library(nnet)
library(mlogit)
library(ROCR)
library(psych)
library(GPArotation)
library(lavaan)
library(caret)
library(car)
library(zoo)

options(scipen = 100)

# Path to folder containing files
folder.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\MIDS\\Machine Learning\\Final\\KaggleTest\\train\\"

# Make data file
path.first.file <- "C:\\Users\\carson.GROUP5\\Dropbox\\MIDS\\Machine Learning\\Final\\KaggleTest\\train\\subj1_series1_data.csv"

# Load first file to get structure for appending
master.train.data <- read.csv(path.first.file, header = T, stringsAsFactors = F)

# Only keep first row
master.train.data <- master.train.data[1,]

# Loop through each series and append to master

for(series in 1:8) {
  folder.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\MIDS\\Machine Learning\\Final\\KaggleTest\\train\\"
  path.name <- paste0(folder.path,"subj",1,"_","series",series,"_","data",".csv")
  print(path.name)
  new.file <- read.csv(path.name, header = T, stringsAsFactors = F)
  master.train.data <- rbind(master.train.data, new.file)
}

# Remove duplicate first row
train.data <- master.train.data[-1,]

# Make events file
path.first.file.events <- "C:\\Users\\carson.GROUP5\\Dropbox\\MIDS\\Machine Learning\\Final\\KaggleTest\\train\\subj1_series1_events.csv"

master.train.events <- read.csv(path.first.file.events, header = T, stringsAsFactors = F)
master.train.events <- master.train.events[1,]

for(series in 1:8) {
  path.name <- paste0(folder.path,"subj","1","_","series",series,"_","events",".csv")
  print(path.name)
  new.file <- read.csv(path.name, header = T, stringsAsFactors = F)
  master.train.events <- rbind(master.train.events, new.file)
}

train.events <- master.train.events[-1,]

# Make random subsets
mini.train.data <- train.data[1:100000,]
mini.train.events <- train.events[1:100000,]

mini.dev.data <- train.data[100001:200000,]
mini.dev.events <- train.events[100001:200000,]

mini.train.events$NoEvent <- rowSums(mini.train.events[2:7])
mini.train.events$NoEvent <- ifelse(mini.train.events$NoEvent == 0,1,0)

mini.dev.events$NoEvent <- rowSums(mini.dev.events[2:7])
mini.dev.events$NoEvent <- ifelse(mini.dev.events$NoEvent == 0,1,0)

dev.outcome <- colnames(mini.dev.events)[2:8][max.col(mini.dev.events[,2:8])]

# Features
mini.train.data$last.handstart <- c(NA, mini.train.events$HandStart[1:(length(mini.train.events$HandStart)-1)])
mini.dev.data$last.handstart <- c(NA, mini.dev.events$HandStart[1:(length(mini.dev.events$HandStart)-1)])

first.touch.mean <- rollapply(mini.train.events$FirstDigitTouch, width = 30, by = 1, FUN = mean, na.rm = T)
first.touch.mean <- first.touch.mean[-length(first.touch.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
first.touch.mean <- c(na_vector, first.touch.mean)
mini.train.data$FirstDigitTouch.mean <- first.touch.mean

first.touch.mean <- rollapply(mini.dev.events$FirstDigitTouch, width = 30, by = 1, FUN = mean, na.rm = T)
first.touch.mean <- first.touch.mean[-length(first.touch.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
first.touch.mean <- c(na_vector, first.touch.mean)
mini.dev.data$FirstDigitTouch.mean <- first.touch.mean
plot(mini.dev.data$FirstDigitTouch.mean)

hand.start.mean <- rollapply(mini.train.events$HandStart, width = 30, by = 1, FUN = mean, na.rm = T)
hand.start.mean <- hand.start.mean[-length(hand.start.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
hand.start.mean <- c(na_vector, hand.start.mean)
mini.train.data$HandStart.mean <- hand.start.mean

hand.start.mean <- rollapply(mini.dev.events$HandStart, width = 30, by = 1, FUN = mean, na.rm = T)
hand.start.mean <- hand.start.mean[-length(hand.start.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
hand.start.mean <- c(na_vector, hand.start.mean)
mini.dev.data$HandStart.mean <- hand.start.mean
plot(mini.dev.data$HandStart.mean)

both.start.mean <- rollapply(mini.train.events$BothStartLoadPhase, width = 30, by = 1, FUN = mean, na.rm = T)
both.start.mean <- both.start.mean[-length(both.start.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
both.start.mean <- c(na_vector, both.start.mean)
mini.train.data$BothStartLoadPhase.mean <- both.start.mean

both.start.mean <- rollapply(mini.dev.events$BothStartLoadPhase, width = 30, by = 1, FUN = mean, na.rm = T)
both.start.mean <- both.start.mean[-length(both.start.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
both.start.mean <- c(na_vector, both.start.mean)
mini.dev.data$BothStartLoadPhase.mean <- both.start.mean
plot(mini.dev.data$BothStartLoadPhase.mean)

lift.off.mean <- rollapply(mini.train.events$LiftOff, width = 30, by = 1, FUN = mean, na.rm = T)
lift.off.mean <- lift.off.mean[-length(lift.off.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
lift.off.mean <- c(na_vector, lift.off.mean)
mini.train.data$LiftOff.mean <- lift.off.mean

lift.off.mean <- rollapply(mini.dev.events$LiftOff, width = 30, by = 1, FUN = mean, na.rm = T)
lift.off.mean <- lift.off.mean[-length(lift.off.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
lift.off.mean <- c(na_vector, lift.off.mean)
mini.dev.data$LiftOff.mean <- lift.off.mean
plot(mini.dev.data$LiftOff.mean)

replace.mean <- rollapply(mini.train.events$Replace, width = 30, by = 1, FUN = mean, na.rm = T)
replace.mean <- replace.mean[-length(replace.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
replace.mean <- c(na_vector, replace.mean)
mini.train.data$Replace.mean <- replace.mean

replace.mean <- rollapply(mini.dev.events$Replace, width = 30, by = 1, FUN = mean, na.rm = T)
replace.mean <- replace.mean[-length(replace.mean)] # Remove final mean because it includes last value
na_vector <- rep(NA,30)
replace.mean <- c(na_vector, replace.mean)
mini.dev.data$Replace.mean <- replace.mean
plot(mini.dev.data$Replace.mean)


# Create an empty data frame for appending output probabilities
probs <- data.frame(c(1:100000))

# Build a logistic regression for each of the 6 outcome variables
for(i in 2:8) {
  model <- glm(mini.train.events[,i] ~ ., data = mini.train.data[,-1], family = "binomial")
  print(summary(model))
  predictions <- predict(model, mini.dev.data[,-1], type = "response")
  probs[,i] <- predictions
}

# Convert to data frame and add column headers
probs.df <- as.data.frame(probs)
colnames(probs.df) <- colnames(mini.train.events)
probs.final <- probs.df[,-1]
probs.final$max.prob <- colnames(probs.final)[max.col(probs.final)]
View(probs.final[1000:2000,])

correct <- probs.final$max.prob[dev.outcome != 'NoEvent'] == dev.outcome[dev.outcome != 'NoEvent']
mean(correct, na.rm = T)
