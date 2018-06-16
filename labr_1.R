#1a
a <- c(1:20)
a
#1b
b <- c(20:1)
b
#1c
c <- c(1:20,19:1)
c
#1d
tmp <- c(4,6,3)
tmp
#1e 
e <- rep(tmp,times = 10 )
e
#1f
f <- c(rep(tmp, times = 10),rep(tmp, len = 1))
f
#1g
g <- rep(tmp, c(10,20,30))
g
###########################################################################

#4a
ax <- 10:100
sum4a <- sum(ax^3 + 4*ax^2)
sum4a

#4b
bx <- 1:25
sum4b <- sum((2^bx)/bx +  (3^bx)/(bx^2) )
sum4b

############################################################################

set.seed(50)
xVec <- sample(0:999 , 250 , replace =T)
yVec <- sample(0:999 , 250 , replace =T)
#6a
result6a = yVec[2:length(yVec)] - xVec[1:length(xVec)-1]
result6a

#6b
result6b =  sin(yVec[1:length(yVec)-1])/cos(xVec[1:length(xVec)])
result6b

#6c
result6c =  xVec[1: (length(xVec)-2)] + (2*xVec[2:(length(xVec)-1)]) - xVec[3:length(xVec)]
result6c    

#6d
# e <- exp(xVec[1]) 
# e
roundX <- 1:(length(xVec)-1)

result6d <- sum(exp(xVec[roundX])^(-xVec[roundX] + 1 )/ (xVec[roundX]+10) )
result6d

############################################################################

#### 
##7a
result7a <- matrix(c(10,-10,10), nrow = 15, ncol = 3, byrow = TRUE)
result7a
