library(nnet)
library(mlogit)
library(ROCR)
library(psych)
library(GPArotation)
library(lavaan)

# Path to folder containing files
folder.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\MIDS\\Machine Learning\\Final\\KaggleTest\\train\\"

#Make data file
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
#View(train.data)

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
View(train.events)

# Make random subsets
sample.vector <- sample(nrow(train.data), 100000)
mini.train.data <- train.data[sample.vector,]
mini.train.events <- train.events[sample.vector,]

sample.dev.vector <- sample(nrow(train.data), 100000)
mini.dev.data <- train.data[sample.dev.vector,]
mini.dev.events <- train.events[sample.dev.vector,]

# Make dummy for no event
mini.train.events$NoEvent <- 1 - rowSums(mini.train.events[2:7])
mini.dev.events$NoEvent <- 1 - rowSums(mini.train.events[2:7])

# Create single outcome factor of events
mini.train.data$outcome <- colnames(mini.train.events)[2:8][max.col(mini.train.events[,2:8])]
mini.dev.data$outcome <- colnames(mini.dev.events)[2:8][max.col(mini.dev.events[,2:8])]

# Logistic regression
model <- multinom(outcome ~ ., data = mini.train.data[,-1])
#summary(model) takes forever to run for some reason

#Prediction report etc.
predictions <- predict(model, newdata = mini.dev.data[,-1])
predictions.correct <- predictions == mini.dev.data$outcome
predict.df <- cbind(mini.dev.data$outcome,predictions, predictions.correct)
View(predict.df)

# Percent correct is high, but only because NoEvent is so common
mean(predictions.correct)

# Terrible at predicting actual events
mean(predictions.correct[mini.dev.data$outcome != "NoEvent"])

# Reduce noise with FA
scree(mini.train.data[,2:33])
mini.train.fa <- fa(mini.train.data[,2:33], nfactors=6, rotate="oblimin", fm="pa")