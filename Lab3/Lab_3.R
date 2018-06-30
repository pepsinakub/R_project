install.packages("editrules")
library(editrules)
### Raw data
sleep.df <- read.table("sleep.txt",header=T,fill=T)
head(sleep.df)
### Technically correct data
sleep.df <- read.csv("sleep.csv",header=T)
summary(sleep.df[,c(1,2,9)])
sleep.df <- read.csv("sleep.csv",header=T,colClasses=c('character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','factor','factor','factor'))
summary(sleep.df[,c(1,2,9)])

### Consistent data
#### NA's
summary(sleep.df[,c(4,5,6)])
sleep.df[sleep.df==-999] <- NA
summary(sleep.df[,c(4,5,6)])
#### Do the sleeps add up?
attach(sleep.df)
round(total.sleep-nondreaming.sleep-dreaming.sleep,1)
detach(sleep.df)
sleep.df[is.na(sleep.df)] <- 0
attach(sleep.df)
round(total.sleep-nondreaming.sleep-dreaming.sleep,1)
detach(sleep.df)

### Obvious inconsistencies
sleep.df <- within(sleep.df,{sum.sleep <- round(nondreaming.sleep+dreaming.sleep,1)})
rules <- editset("total.sleep==sum.sleep")
summary(violatedEdits(rules,sleep.df))

### Imputing missing values
sleep.df[sleep.df==0] <- NA
#### Mean imputation
summary(sleep.df$dreaming.sleep)
I <- is.na(sleep.df$dreaming.sleep)
sleep.df$dreaming.sleep[I] <- mean(sleep.df$dreaming.sleep,na.rm=T)
summary(sleep.df$dreaming.sleep)

### Model imputation
sleep.df <- read.csv("sleep.csv",header=T,colClasses=c('character','numeric','numeric','numeric','numeric','numeric','numeric','numeric','factor','factor','factor'))
sleep.df[sleep.df==-999] <- NA
summary(sleep.df$total.sleep)
model <- lm(total.sleep~log(body.weight)+overall.danger.index,data=sleep.df)
I <- is.na(sleep.df$total.sleep)
sleep.df$total.sleep[I] <- predict(model,newdata=sleep.df[I,])
summary(sleep.df$total.sleep)

# Exercise
# 1. Create a function to find a mean imputation between neighbors.
# E.g., x <- c(1,2,NA,4,5,6), NA is 3 which comes from (2+4)/2
#
r_Mean <- function(x_){
  x_[is.na(x_)] <- (x_[which(is.na(x_))-1] + x_[which(is.na(x_))+1]) /2
  x_
}

data1 <- c(1,2,NA,4,5,6)
data2 <- c(1,2,NA,7,8,NA,10)

r_Mean(data1)
r_Mean(data2)

# 2. Cleaning Lab3_data.csv
#
lab3_df <- read.csv("lab3_data.csv",header=T)
lab3_df

summary(sleep.df[,c(1,2,9)])
sleep.df <- read.csv("sleep.csv",header=T,
              colClasses=c('numeric','numeric','character','numeric','numeric'))


x1 <- "aaa"
x2 <- "bbb"

paste(x1,x2, sep="")
