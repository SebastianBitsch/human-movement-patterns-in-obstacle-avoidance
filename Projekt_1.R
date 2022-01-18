load("armdata.rdata")

options(rgl.useNULL = TRUE)
library(rgl)
clear3d()

#Experiment to plot
experiment.number <- 5 #1 #outlier 5

#Plotting variables
colors <- c("red", "orange", "yellow", "green", "blue", "purple", "pink", "black","brown","cyan")
heights <- c(rep(c(20,27.5, 35),times=5),0)
distances <- c(rep(c(15,22.5,30,37.5,45),each=3),0)

start_cyl <- cylinder3d(cbind(0, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
target_cyl <- cylinder3d(cbind(60, 0, seq(0, 10, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl1 <- cylinder3d(cbind(0, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)
cyl2 <- cylinder3d(cbind(60, 0, 10 + seq(0, 12.5, length = 10)), radius = c(3,3,3), sides = 20, closed = -2)

#Change the position and height of the obstacle depending on the experiment number
cyl3 <- cylinder3d(cbind(distances[experiment.number], 0, seq(0, heights[experiment.number], length = 10)), radius = c(3,3,3), sides = 10, closed = -2)


shade3d(addNormals(subdivision3d(start_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(target_cyl)), col = "darkgreen")
shade3d(addNormals(subdivision3d(cyl1)), col = "pink")
shade3d(addNormals(subdivision3d(cyl2)), col = "pink", alpha = 0.5)
shade3d(addNormals(subdivision3d(cyl3)), col = "lightblue")
surface3d(c(-7, 67), c(-20, 20), matrix(0, 2, 2), col = "brown", alpha = 0.9, specular = "black")

# Plot the lines
for (j in 1:10) {
  for (i in 1:10) {
    if (j == 2) {
      lines3d(armdata[[experiment.number]][[j]][[i]],color = colors[j])
    } else {
      lines3d(armdata[[experiment.number]][[j]][[i]],color = colors[j],alpha=0.5)
    }
  }
}

rglwidget()

#NOTE: armdata[[5]][[2]][[7]] is bad, 1251 in df


# --------------------
# DATA PROCESSING

experiments <- matrix(NA,16*10*10*3,100)

count <- 1
for (i in 1:16) {
  for (j in 1:10) {
    for (k in 1:10) {
      experiments[count,] <- armdata[[i]][[j]][[k]][,1]
      experiments[count+1,] <- armdata[[i]][[j]][[k]][,2]
      experiments[count+2,] <- armdata[[i]][[j]][[k]][,3]
      
      count <- count+3
    }
  }
}

# Convert matrix to dataframe
df <- data.frame(experiments)
test.df <- data.frame(experiments)

# -------------
# Add x,y,z col
dimension <- c(rep(c("x","y","z")))
df$dimension <- dimension

library(tidyverse)
library("ggplot2")
library(svglite)


par(mfrow = c(3,3),mai = c(0.55, 0.55, .15,0))
for (i in c(25,50,75)) {
  all.x <- as.vector(as.matrix(df[seq(1,4800,by=3),i]))
  all.y <- as.vector(as.matrix(df[seq(2,4800,by=3),i]))
  all.z <- as.vector(as.matrix(df[seq(3,4800,by=3),i]))
  ylabel <- NA
  if (i == 75) {
    ylabel <- "Sample Quantiles"
  }
  qqnorm(all.x, main = str_c('X',i,' Normal Q-Q Plot'),xlab=ylabel);qqline(all.x);
  qqnorm(all.y, main = str_c('Y',i,' Normal Q-Q Plot'),xlab=ylabel,ylab=NA);qqline(all.y)
  qqnorm(all.z, main = str_c('Z',i,' Normal Q-Q Plot'),xlab=ylabel,ylab=NA);qqline(all.z)
}
?par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 2) + 0.1)


# -------------
# Add height and distance columns
heights <- c(rep(rep(c("S","M","T"),each=300),times=5),rep('-',times=300))
distances <- c(rep(c(15,22.5,30,37.5,45),each=900),rep('-',times=300))

df$obstacle.height <- heights
df$obstacle.distance <- distances

# -------------
# Add person col
person.id <- rep((rep(seq(1,10),each=30)),times=16)
df$person.id <- person.id

# -------------
# Add experiment col
experiment.id <- rep(seq(1,16),each=300)
df$experiment.id <- experiment.id

# -------------
# Check for NA values: 
# which(is.na(test.df),arr.ind =TRUE)
# test.df$experiment.id <- df$experiment.id
# test.df$person.id <- df$person.id
# 
# test.df[1441,]$experiment.id
# test.df[2044,]$experiment.id
# test.df[2941,]$experiment.id
# test.df[3241,]$experiment.id
# test.df[3841,]$experiment.id
# test.df[4144,]$experiment.id
# 
# sum(is.na(test.df))


# Remove NA-values
for (i in 1:4800) {
  for (j in 1:100) {
    if (is.na(df[i,j])) {
      dim <- df[i,]$dimension
      df[i,j] <- mean(df[,j][df$dimension==dim],na.rm=TRUE)
    }
  }
}
# Check the worst row to see if the data looks ok
plot(seq(1,100),as.list(df[3843,1:100]),type='p')
#hist(as.list(as.data.frame(t(df[3,20:30]))))


# NOTE: armdata[[5]][[2]][[7]] is bad, 1251 in df
plot(seq(1,100),as.list(df[1251,1:100]),type='p')
for (i in 1251:1251) {
  for (j in 22:46) {
    dim <- df[i,]$dimension
    df[i,j] <- mean(df[,j][df$dimension==dim],na.rm=TRUE)
  }
}
par(mfrow = c(1,1),mai=c(1,1,1,1))
plot(seq(1,100),as.list(df[1251,1:100]),type='p',main="Corrected Trajectory",xlab="Time",ylab="Z-value")


# -------------
# Create max.z value column
max.z <- rep(NA, 4800)
for (i in seq(3,4800,by=3)) {
  m <- max(df[i,1:100])
  max.z[(i-2):i] <- m
}
df$max.z <- max.z

hist(max.z)
qqnorm(max.z)
qqline(max.z)

# -------------
# Create argmax.z value column
argmax.z <- rep(NA, 4800)
for (i in seq(3,4800,by=3)) {
  m <- which.max(df[i,1:100])
  argmax.z[(i-2):i] <- m
}
df$argmax.z <- argmax.z


# -------------
# Create max.val value column
max.val <- rep(NA, 4800)
for (i in 1:4800) {
  m <- max(df[i,1:100])
  max.val[i] <- m
}
df$max.val <- max.val


# -------------
# Create mean of row column
df$mean <- rowMeans(df[,1:100])

# -------------
# Create length of curve column X/Y/Z
lengths <- rep(NA, 1600)
k <- 1

for (i in seq(1,4800,by=3)) {
  dist.sum <- 0
  for (j in 2:100) {
    dist.sum <- dist.sum + dist(t(as.matrix(df[i:(i+2),(j-1):j])),method="euclidean")[1]
  }
  lengths[k] <- dist.sum
  k <- k + 1
}
df$curve.length <- rep(lengths, each=3)

hist(df$curve.length)

# -------------
# Create length of curve column per dimension
lengths <- rep(NA, 4800)

for (i in 1:4800) {
  dist.sum <- 0
  for (j in 2:100) {
    dist.sum <- dist.sum + abs(df[i,j] - df[i,j-1])
  }
  lengths[i] <- dist.sum
}
df$dim.length <- lengths


# -------------
# Create length of curve column X/Y
lengths <- rep(NA, 1600)
k <- 1

for (i in seq(1,4800,by=3)) {
  dist.sum <- 0
  for (j in 2:100) {
    dist.sum <- dist.sum + dist(t(as.matrix(df[i:(i+1),(j-1):j])),method="euclidean")[1]
  }
  lengths[k] <- dist.sum
  k <- k + 1
}
df$curve.length.xy <- rep(lengths, each=3)

# -------------
# Create length of curve column X/Z
lengths <- rep(NA, 1600)
k <- 1

for (i in seq(1,4800,by=3)) {
  dist.sum <- 0
  for (j in 2:100) {
    dist.sum <- dist.sum + dist(t(as.matrix(df[c(i,i+2),(j-1):j])),method="euclidean")[1]
  }
  lengths[k] <- dist.sum
  k <- k + 1
}
df$curve.length.xz <- rep(lengths, each=3)

# -------------
# Create length of curve column Y/Z
lengths <- rep(NA, 1600)
k <- 1

for (i in seq(1,4800,by=3)) {
  dist.sum <- 0
  for (j in 2:100) {
    dist.sum <- dist.sum + dist(t(as.matrix(df[(i+1):(i+2),(j-1):j])),method="euclidean")[1]
  }
  lengths[k] <- dist.sum
  k <- k + 1
}
df$curve.length.yz <- rep(lengths, each=3)


# -------------
# Create Area-under-curve column
library(zoo)

AUCs <- rep(NA, 4800)

for (i in 1:4800) {
  x <- seq(0,99)
  y <- as.matrix(df[i,1:100])
  id <- order(x)
  AUC <- sum(diff(x[id])*rollmean(y[id],2))
  AUCs[i] <- AUC - y[1]*100
}
df$auc <- abs(AUCs)


# -------------
# Create Area-under-z column _ SAME AS max.z !!
indices <- seq(3,4800, by=3)
z.vals <- df$auc[indices]
auc.z <- rep(z.vals,each=3)

df$auc.z <- auc.z


par(mar = c(5, 4, 4, 2) + 0.1)
# -------------
# Create with mean for each experiment per person for x,y,z to create boxplots
data.x <- df[df$dimension == 'x',]
data.y <- df[df$dimension == 'y',]
data.z <- df[df$dimension == 'z',]

par(mfrow = c(1,3))
boxplot(data.x$mean ~ data.x$person.id, main="Mean in X-dim per person", ylab = "Mean", xlab = "person.id")
boxplot(data.y$mean ~ data.y$person.id, main="Mean in Y-dim per person", ylab = "Mean", xlab = "person.id")
boxplot(data.z$mean ~ data.z$person.id, main="Mean in Z-dim per person", ylab = "Mean", xlab = "person.id")
par(mfrow = c(1,1))


# -------------
# Get mean data point of each person
# Create more boxplots (commented out)

x.means.person <- rep(NA,10)

for (i in 1:10) {
  x.means.person[i] <- mean(as.matrix(data.x[data.x$person.id == i,1:100]))
}

plot(x.means.person)
hist(x.means.person)

interest.person <- 1
par(mfrow = c(3,1),mar=c(2,5,2,2))
boxplot(as.matrix(data.x[data.x$person.id == interest.person,1:100]),ylab="X-coordinate", main="Timeseries variation in X,Y and Z for person 1")
boxplot(as.matrix(data.y[data.y$person.id == interest.person,1:100]),ylab="Y-coordinate")
boxplot(as.matrix(data.z[data.z$person.id == interest.person,1:100]),ylab="Z-coordinate")
par(mfrow = c(1,1))
par(mar = c(5, 4, 4, 2) + 0.1)

#Check for normal distribution in curve lengths and max.z
par(mfrow = c(1,2))
hist(df$curve.length)
hist(df$max.z)
par(mfrow = c(1,1))

#Check for normality in the mean of x,y,z
par(mfrow = c(1,3))
hist(df$mean[df$dimension == "x"],main="Histogram of X")
hist(df$mean[df$dimension == "y"],main="Histogram of Y")
hist(df$mean[df$dimension == "z"],main="Histogram of Z")
par(mfrow = c(1,1))


#Check for normality at individual timesteps
par(mfrow = c(1,3))
hist(df$X61[df$dimension == 'x'])
hist(df$X61[df$dimension == 'y'])
hist(df$X61[df$dimension == 'z'])
par(mfrow = c(1,1))


par(mfrow = c(1,3))
hist(df$dim.length[df$dimension == 'x'])
hist(df$dim.length[df$dimension == 'y'])
hist(df$dim.length[df$dimension == 'z'])


# -----------------------------
# 1. Assess if there is a significant effect of person on the observed trajectories.

# Select x-values
indices <- seq(1,4800, by=3)

L <- lm(auc[indices] ~ person.id[indices] + experiment.id[indices], data=df)

qqnorm(L$residuals)
qqline(L$residuals)
summary(L)

anova(L)


par(mfrow = c(2,2), mar = c(4, 4, 3, 1) + 0.1)
plot(L)
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1)

library(ggpubr)
ggqqplot(residuals(L),main="Q-Q plot of Residuals of lm")

df.xxxx <- df[df$dimension=='x',]
ggplot(df, aes(x=as.factor(person.id), y=auc)) + geom_boxplot()



# -----------------------------
# 2. Use one or more appropriate machine learning models to predict the size and 
# position of the cylinder from observation and evaluate this prediction.

# NOTE: Use NNET package and caret 
library(nnet)
library(caret)

# Splitting the data using a function from dplyr package
set.seed(123); index <- createDataPartition(df$experiment.id, p = .80, list = FALSE)

df.train <- df[index,]
df.test <- df[-index,]


# Setting the reference (not used)
# train$experiment.id <- relevel(train$experiment.id, ref = "1")

# Training the multinomial model
#multinom_model <- multinom(experiment.id ~ dimension + person.id + curve.length + mean + max.z, data = df)

# Checking the model
#summary(multinom_model)
#print(multinom_model)
#exp(coef(multinom_model))

#head(round(fitted(multinom_model), 2))

# Predicting the values for train dataset
#train.class.predicted <- predict(multinom_model, newdata = train, "class")

# Building classification table
#tab <- table(train$experiment.id, train.class.predicted)

# Calculating accuracy - sum of diagonal elements divided by total obs
#round((sum(diag(tab))/sum(tab))*100,2)


#----------
# Predicting the class on test dataset.

# Predicting the class for test dataset
#test.class.predicted <- predict(multinom_model, newdata = test, "class")

# Building classification table
#tab <- table(test$experiment.id, test.class.predicted)
#round((sum(diag(tab))/sum(tab))*100,2)



set.seed(123)
train.control <- trainControl(method = "cv", number = 10)

# Train the model

#  0.04833333: person.id
#  0.03791667: dimension
#**0.1910417 : curve.length
#  0.0625    : auc
#  0.09083333: dim.length
#  0.06125   : mean
#**0.1939583 : max.z
#  0.06708333: max.val

#  0.17875   : max.z + person.id
#  0.1679167 : max.z + dimension
#**0.2570833 : max.z + curve.length
#  0.18375   : max.z + auc
#  0.1816667 : max.z + dim.length
#  0.1835417 : max.z + mean
#  0.183125  : max.z + max.val

#**0.2610417 : max.z + curve.length + person.id
#  0.24875   : max.z + curve.length + dimension
#  0.2566667 : max.z + curve.length + auc
#  0.2527083 : max.z + curve.length + dim.length
#  0.2570833 : max.z + curve.length + mean
#  0.2535417 : max.z + curve.length + max.val

#  0.2475    : max.z + curve.length + person.id + dimension
#  0.2610417 : max.z + curve.length + person.id + curve.length
#  0.2541667 : max.z + curve.length + person.id + auc
#  0.2558333 : max.z + curve.length + person.id + dim.length
#  0.2595833 : max.z + curve.length + person.id + mean
#  0.2552083 : max.z + curve.length + person.id + max.val


set.seed(123); model.exp <- train(as.factor(experiment.id) ~ max.val + curve.length + person.id,#curve.length + max.z + argmax.z + max.val,
               data = df.train, method = "multinom",
               trControl = train.control, trace = FALSE); max(model.exp$results[2])
#X/Y/Z ACC. : 0.1713322

pred.exp <- predict(model.exp, df.test)
confusionMatrix(table(df.test[,"experiment.id"],pred))


wilcox.test(table(df.test[,"experiment.id"],pred))
t.test(table(df.test[,"experiment.id"],pred))

wilcox.test(df.test[,"experiment.id"],as.numeric(pred),paired=TRUE)
t.test(df.test[,"experiment.id"],as.numeric(pred),paired=TRUE)




# EXPERIMENT IN X/Y PLANE
df.xy <- df.train[df.train$dimension == 'x' | df.train$dimension == 'y',]
set.seed(123); model.exp.xy <- train(as.factor(experiment.id) ~ max.val + curve.length.xy + person.id,
                              data = df.xy, method = "multinom",
                              trControl = train.control, trace = FALSE); max(model.exp.xy$results[2])
pred.exp.xy <- predict(model.exp.xy, df.test[df.test$dimension == 'x' | df.test$dimension == 'y',])
#X/Y ACC. : 0.1313806

# EXPERIMENT IN X/Z PLANE
df.xz <- df.train[df.train$dimension == 'x' | df.train$dimension == 'z',]
set.seed(123); model.exp.xz <- train(as.factor(experiment.id) ~ max.val + curve.length.xz + person.id,
                              data = df.xz, method = "multinom",
                              trControl = train.control, trace = FALSE); max(model.exp.xz$results[2])
pred.exp.xz <- predict(model.exp.xz, df.test[df.test$dimension == 'x' | df.test$dimension == 'z',])
#X/Z ACC. : 0.1699365

# EXPERIMENT IN Y/Z PLANE
df.yz <- df[df$dimension == 'y' | df$dimension == 'z',]
set.seed(123); model.exp.yz <- train(as.factor(experiment.id) ~ max.val + curve.length.yz + person.id,
                              data = df.yz, method = "multinom",
                              trControl = train.control, trace = FALSE); max(model.exp.yz$results[2])
pred.exp.yz <- predict(model.exp.yz, df.test[df.test$dimension == 'y' | df.test$dimension == 'z',])
#Y/Z ACC. : 0.168125

# Summarize the results
print(model)
summary(model)




# 0.3010417 : person.id
# 0.2822917 : dimension
# 0.6820833 : curve.length
# 0.3402083 : auc
# 0.376875  : dim.length
# 0.3125    : mean
#*0.7264583 : max.z
# 0.3127083 : max.val
# 0.31625   : argmax.z

# 0.725     : max.z + person.id
# 0.7260417 : max.z + dimension
# 0.7327083 : max.z + curve.length
# 0.7264583 : max.z + auc
# 0.726875  : max.z + dim.length
# 0.7266667 : max.z + mean
# 0.726875  : max.z + max.val
#*0.7458333 : max.z + argmax.z

# 0.7464583 : max.z + argmax.z + person.id
# 0.7464583 : max.z + argmax.z + dimension
# 0.7460417 : max.z + argmax.z + curve.length
# 0.7464583 : max.z + argmax.z + auc
# 0.7466667 : max.z + argmax.z + dim.length
# 0.7458333 : max.z + argmax.z + mean
#*0.7470833 : max.z + argmax.z + max.val

# 0.74625   : max.z + argmax.z + max.val + person.id
# 0.7460417 : max.z + argmax.z + max.val + dimension
# 0.7464583 : max.z + argmax.z + max.val + curve.length
# 0.7470833 : max.z + argmax.z + max.val + auc
# 0.7441667 : max.z + argmax.z + max.val + dim.length
# 0.7466667 : max.z + argmax.z + max.val + mean


set.seed(123); model.height <- train(as.factor(obstacle.height) ~ max.val + curve.length.xy + person.id,# max.z + argmax.z + max.val + mean,
                              data = df.train, method = "multinom",
                              trControl = train.control, trace = FALSE); max(model.height$results[2])

# Summarize the results
print(model.height)
summary(model.height)

# Perform McNemar test on the predictions
pred.height <- predict(model.height, df.test)
confusionMatrix(table(df.test[,"obstacle.height"],pred))
print(model)


wilcox.test(as.numeric(as.factor(df.test$obstacle.height)),as.numeric(pred),paired=TRUE)
t.test(as.numeric(as.factor(df.test$obstacle.height)),as.numeric(pred),paired=TRUE)


# EXPERIMENT IN X/Y PLANE
df.xy <- df.train[df.train$dimension == 'x' | df.train$dimension == 'y',]
set.seed(123); model.height.xy <- train(as.factor(obstacle.height) ~ max.val + curve.length.xy + person.id,
                                     data = df.xy, method = "multinom",
                                     trControl = train.control, trace = FALSE); max(model.height.xy$results[2])
pred.height.xy <- predict(model.height.xy, df.test[df.test$dimension == 'x' | df.test$dimension == 'y',])
#X/Y ACC. : 0.4278471

# EXPERIMENT IN X/Z PLANE
df.xz <- df.train[df.train$dimension == 'x' | df.train$dimension == 'z',]
set.seed(123); model.height.xz <- train(as.factor(obstacle.height) ~ max.val + curve.length.xz + person.id,
                                     data = df.xz, method = "multinom",
                                     trControl = train.control, trace = FALSE); max(model.height.xz$results[2])
pred.height.xz <- predict(model.height.xz, df.test[df.test$dimension == 'x' | df.test$dimension == 'z',])
#X/Z ACC. : 0.6797503

# EXPERIMENT IN Y/Z PLANE
df.yz <- df[df$dimension == 'y' | df$dimension == 'z',]
set.seed(123); model.height.yz <- train(as.factor(obstacle.height) ~ max.val + curve.length.yz + person.id,
                                     data = df.yz, method = "multinom",
                                     trControl = train.control, trace = FALSE); max(model.height.yz$results[2])
pred.height.yz <- predict(model.height.yz, df.test[df.test$dimension == 'y' | df.test$dimension == 'z',])
#Y/Z ACC. : 0.7259375



# 0.1677083 : person.id
# 0.1591667 : dimension
#*0.2545833 : curve.length
# 0.1810417 : auc
# 0.1845833 : dim.length
# 0.1845833 : mean
# 0.2447917 : max.z
# 0.1745833 : max.val
# 0.2166667 : argmax.z

# 0.24375   : curve.length + person.id
# 0.234375  : curve.length + dimension
# 0.251875  : curve.length + auc
# 0.2508333 : curve.length + dim.length
# 0.2541667 : curve.length + mean
#*0.3410417 : curve.length + max.z
# 0.2460417 : curve.length + max.val
# 0.2818750 : curve.length + argmax.z

# 0.32875   : curve.length + max.z + person.id
# 0.3329167 : curve.length + max.z + dimension
# 0.3395833 : curve.length + max.z + auc
# 0.3360417 : curve.length + max.z + dim.length
# 0.3435417 : curve.length + max.z + mean
# 0.331875  : curve.length + max.z + max.val
#*0.368125  : curve.length + max.z + argmax.z

# 0.3652083 : curve.length + max.z + argmax.z + person.id
# 0.3622917 : curve.length + max.z + argmax.z + dimension
# 0.36375   : curve.length + max.z + argmax.z + auc
# 0.3616667 : curve.length + max.z + argmax.z + dim.length
# 0.3620833 : curve.length + max.z + argmax.z + mean
# 0.3641667 : curve.length + max.z + argmax.z + max.val

 

# set.seed(123); model <- train(as.factor(obstacle.distance) ~ .,#curve.length + max.z + argmax.z, # + max.val
#                               method = "multinom", data=df.train,#[, !names(df.train) %in% c("experiment.id","obstacle.height")],
#                               trControl = train.control, trace = FALSE); max(model$results[2])

set.seed(123); model.dist <- train(as.factor(obstacle.distance) ~ curve.length + max.z + argmax.z + max.val,
                              method = "multinom", data=df.train,
                              trControl = train.control, trace = FALSE); max(model$results[2])


# Summarize the results
print(model.dist)
summary(model.dist)


# Perform McNemar test on the predictions
pred.dist <- predict(model.dist, df.test)
confusionMatrix(table(df.test[,"obstacle.distance"],pred))
print(model)

wilcox.test(as.numeric(as.factor(df.test$obstacle.distance)),as.numeric(pred),paired=TRUE)
t.test(as.numeric(as.factor(df.test$obstacle.distance)),as.numeric(pred),paired=TRUE)




# EXPERIMENT IN X/Y PLANE
df.xy <- df.train[df.train$dimension == 'x' | df.train$dimension == 'y',]
set.seed(123); model.dist.xy <- train(as.factor(obstacle.distance) ~ max.val + curve.length.xy + person.id,
                                        data = df.xy, method = "multinom",
                                        trControl = train.control, trace = FALSE); max(model.dist.xy$results[2])
pred.dist.xy <- predict(model.dist.xy, df.test[df.test$dimension == 'x' | df.test$dimension == 'y',])
#X/Y ACC. : 0.2764987

# EXPERIMENT IN X/Z PLANE
df.xz <- df.train[df.train$dimension == 'x' | df.train$dimension == 'z',]
set.seed(123); model.dist.xz <- train(as.factor(obstacle.distance) ~ max.val + curve.length.xz + person.id,
                                        data = df.xz, method = "multinom",
                                        trControl = train.control, trace = FALSE); max(model.dist.xz$results[2])
pred.dist.xz <- predict(model.dist.xz, df.test[df.test$dimension == 'x' | df.test$dimension == 'z',])
#X/Z ACC. : 0.2388319

# EXPERIMENT IN Y/Z PLANE
df.yz <- df[df$dimension == 'y' | df$dimension == 'z',]
set.seed(123); model.dist.yz <- train(as.factor(obstacle.distance) ~ max.val + curve.length.yz + person.id,
                                        data = df.yz, method = "multinom",
                                        trControl = train.control, trace = FALSE); max(model.dist.yz$results[2])
pred.dist.yz <- predict(model.dist.yz, df.test[df.test$dimension == 'z' | df.test$dimension == 'y',])
#Y/Z ACC. : 0.24125










# --------------------------------------------
# 4. There is a potential bias and fairness issue since the participants are all right-handed, however a minority 
# of the population are left-handed. Discuss this issue from a statistical evaluation perspective. 
# You may assume a left hand/right hand mirror symmetry across the x/z plane.

#Create the dataframe for the mirrored by multiplying Y by -1
df.mirrored <- df
df.mirrored[seq(2,4800,by=3),1:100] <- df.mirrored[seq(2,4800,by=3),1:100] * (-1)



# -----------------------
## COMPARE THE MODELS X/Y/Z

heights.dict < -data.frame(row.names=c("S","M","T","-") , val=c(0,1,2,-1))
dists.dict <- data.frame(row.names=c("15","22.5","30,","37.5","45", "-") , val=c(1,4,7,10,13,-1))

combined.preds <- rep(NA,960)
for (i in 1:960) {
  h <- heights.dict[toString(pred.height[i]),]
  d <- dists.dict[toString(pred.dist[i]),]
  
  if (is.na(h) | is.na(d)) {
    print(i)
  }
  
  if(isTRUE(h == -1) | isTRUE(d == -1)) {
    combined.preds[i] <- 16
  } else {
    combined.preds[i] <- h+d
  }
}
hist(combined.preds)

combi.correct <- combined.preds == df.test$experiment.id
single.correct <- pred.exp == df.test$experiment.id

n11 <- sum(single.correct & combi.correct)
n22 <- sum(!single.correct & !combi.correct)

n12 <- sum(combi.correct & !single.correct)
n21 <- sum(!combi.correct & single.correct)

(n21-n12)/960

confusion.xyz <- matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE)
confusion.xyz

#mcnemar.test(confusion.xyz)
#mcnemar.test(combi.correct, single.correct)

library(epibasix)
mcNemar(confusion, alpha= 0.05, force=FALSE, digits=3)
#p-value of: 0.129
#95% Confidence Limits for the OR are: [0.431, 1.093]

# -----------------------
## COMPARE THE MODELS X/Y


len<- length(pred.height.xy)
combined.preds <- rep(NA,len)

for (i in 1:len) {
  h <- heights.dict[toString(pred.height.xy[i]),]
  d <- dists.dict[toString(pred.dist.xy[i]),]

  if(isTRUE(h == -1) | isTRUE(d == -1)) {
    combined.preds[i] <- 16
  } else {
    combined.preds[i] <- h+d
  }
}
combi.correct <- combined.preds == df.test$experiment.id[df.test$dimension == 'x' | df.test$dimension == 'y']
single.correct <- pred.exp.xy == df.test$experiment.id[df.test$dimension == 'x' | df.test$dimension == 'y']

n11 <- sum(single.correct & combi.correct)
n22 <- sum(!single.correct & !combi.correct)

n12 <- sum(combi.correct & !single.correct)
n21 <- sum(!combi.correct & single.correct)

confusion.xy <- matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE)
confusion.xy

mcNemar(confusion.xy, alpha= 0.05, force=FALSE, digits=3)
#mcnemar.test(confusion.xy)
#mcnemar.test(combi.correct, single.correct)
#mcNemar(matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE), alpha= 0.05, force=FALSE, digits=3)



# -----------------------
## COMPARE THE MODELS X/Z

len<- length(pred.height.xz)
combined.preds <- rep(NA,len)

for (i in 1:len) {
  h <- heights.dict[toString(pred.height.xz[i]),]
  d <- dists.dict[toString(pred.dist.xz[i]),]
  
  if(isTRUE(h == -1) | isTRUE(d == -1)) {
    combined.preds[i] <- 16
  } else {
    combined.preds[i] <- h+d
  }
}
combi.correct <- combined.preds == df.test$experiment.id[df.test$dimension == 'x' | df.test$dimension == 'z']
single.correct <- pred.exp.xz == df.test$experiment.id[df.test$dimension == 'x' | df.test$dimension == 'z']

n11 <- sum(single.correct & combi.correct)
n22 <- sum(!single.correct & !combi.correct)

n12 <- sum(combi.correct & !single.correct)
n21 <- sum(!combi.correct & single.correct)

confusion.xz <- matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE)
confusion.xz

mcNemar(matrix(confusion.xz,nrow=2,byrow=TRUE), alpha= 0.05, force=FALSE, digits=3)
#mcnemar.test(confusion.xz)
#mcnemar.test(combi.correct, single.correct)
#mcNemar(matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE), alpha= 0.05, force=FALSE, digits=3)


# -----------------------
## COMPARE THE MODELS Y/Z

len <- length(pred.height.yz)
combined.preds <- rep(NA,len)

for (i in 1:len) {
  h <- heights.dict[toString(pred.height.yz[i]),]
  d <- dists.dict[toString(pred.dist.yz[i]),]
  
  if(isTRUE(h == -1) | isTRUE(d == -1)) {
    combined.preds[i] <- 16
  } else {
    combined.preds[i] <- h+d
  }
}
combi.correct <- combined.preds == df.test$experiment.id[df.test$dimension == 'y' | df.test$dimension == 'z']
single.correct <- pred.exp.yz == df.test$experiment.id[df.test$dimension == 'y' | df.test$dimension == 'z']


n11 <- sum(single.correct & combi.correct)
n22 <- sum(!single.correct & !combi.correct)

n12 <- sum(combi.correct & !single.correct)
n21 <- sum(!combi.correct & single.correct)

confusion.yz <- matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE)
confusion.yz

mcNemar(confusion.yz, alpha= 0.05, force=FALSE, digits=3)


#mcnemar.test(confusion.yz)
#mcnemar.test(combi.correct, single.correct)
#mcNemar(matrix(c(n11,n12,n21,n22),nrow=2,byrow=TRUE), alpha= 0.05, force=FALSE, digits=3)




mcNemar(confusion, alpha= 0.05, force=FALSE, digits=3)
mcNemar(confusion.xy, alpha= 0.05, force=FALSE, digits=3)
mcNemar(confusion.xz, alpha= 0.05, force=FALSE, digits=3)
mcNemar(confusion.yz, alpha= 0.05, force=FALSE, digits=3)


confusion
confusion.xy
confusion.xz
confusion.yz




# -------------
# CLASSIFICATION: Deep Neural Network in R

# df$dimension <- as.numeric(factor(df$dimension))
# df$experiment.id <- as.numeric(df$experiment.id)
# 
# # Scaling / Normalize
# m <- colMeans(df)
# s <- apply(df, 2, sd)
# df <- scale(df, center = m, scale = s)
# 
# 
# # NOTE: using packages; keras, mlbench, dplyr, magrittr, neuralnet
# library(neuralnet)
# set.seed(123)
# #as.numeric(factor(df$dimension))
# 
# ids <- df$experiment.id
# df$experiment.id <- as.factor(ids)
# train$experiment.id <- as.factor(train$experiment.id)
# 
# 
# nn <- neuralnet(experiment.id ~ person.id + curve.length + mean + max.z, data = train,
#                hidden = c(3,3),
#                threshold = 0.5,
#                linear.output = FALSE,
#                lifesign = 'full',
#                stepmax = 1e+06,
#                learningrate = 0.01,
#                rep=1)
# plot(nn)
# 
# # Results
# # hidden = c(3,3), stepmax = 1e+06, learningrate = 0.01, threshold = 0.5:
# #189585	error: 1958.27191	time: 8.38 mins
# 
# prediction <- compute(nn, test)
# prob <- prediction$net.result
# 
# #Calculate hit rate
# id.predictions <- rep(NA, 960)
# for (i in 1:960) {
#   id.predictions[i] <- which.max(prob[i,])
# }
# 
# hist(id.predictions)
# sum(id.predictions == test$experiment.id)


# df.matrix <- as.matrix(df)
# dimnames(df.matrix) <- NULL
# 
# set.seed(123)
# index <- createDataPartition(df$experiment.id, p = .80, list = FALSE)
# train <- df[index,]
# test <- df[-index,]
# 
# # Convert x,y,z to factors
# train$dimension <- as.numeric(factor(train$dimension))
# test$dimension <- as.numeric(factor(test$dimension))
# 
# # Scaling
# m <- colMeans(train)
# s <- apply(train, 2, sd)
# train <- scale(train, center = m, scale = s)
# test <- scale(test, center = m, scale = s)
# 
# # Model Creation
# # DOES NOT WORK DONT CALl
# model <- keras_model_sequential()
# model %>%
#   layer_dense(units = 5, activation = 'relu', input_shape = c(13)) %>%
#   layer_dense(units = 1)

