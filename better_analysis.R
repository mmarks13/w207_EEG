library(nnet)
library(mlogit)
library(ROCR)

options(scipen = 100)

# Path to folder containing files
folder.path <- "/Users/carsonforter/Dropbox/MIDS/Machine Learning/Final/KaggleTest/train"

# Make data file
path.first.file <- "/Users/carsonforter/Dropbox/MIDS/Machine Learning/Final/KaggleTest/train/subj1_series1_data.csv"

# Load first file to get structure for appending
master.train.data <- read.csv(path.first.file, header = T, stringsAsFactors = F)

# Only keep first row
master.train.data <- master.train.data[1,]

# Loop through each series and append to master

for(series in 1:8) {
  folder.path <- "/Users/carsonforter/Dropbox/MIDS/Machine Learning/Final/KaggleTest/train/"
  path.name <- paste0(folder.path,"subj",1,"_","series",series,"_","data",".csv")
  print(path.name)
  new.file <- read.csv(path.name, header = T, stringsAsFactors = F)
  master.train.data <- rbind(master.train.data, new.file)
}

# Remove duplicate first row
train.data <- master.train.data[-1,]

# Make events file
path.first.file.events <- "/Users/carsonforter/Dropbox/MIDS/Machine Learning/Final/KaggleTest/test/subj1_series1_events.csv"

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
