##### DISTRIBUTIONS OVERVIEW #####################
require(ggplot2)
##### NORMAL DISTRIBUTION
pnorm(q=70, mean=75, sd=5)               #prob x<=70 with mean and sd as set
# 0.159
pnorm(q=85, mean=75, sd=5, lower.tail=F) #prob x>=85 with mean and sd as set
# 0.023

pnorm(q=1, mean=0, sd=1, lower.tail=F)   #prob z-score>=1
# 0.159
qnorm(p=0.25, mean=75, sd=5)             #find 1st quartile for z
# 71.62755

x <- seq(from=55,to=95,0.25)             #create vector
dens <- dnorm(x,mean=75,sd=5)            #plot,assuming a normal distribution with mean and sd as set
plot(x,dens)
plot(x,dens,type='l')

rand <- rnorm(n=40, mean=75, sd=5)       #select random samples from a normal distribution
hist(rand,breaks=10)

##### POISSON DISTRIBUTION
dpois(x=4, lambda=7)                     #prob X=4 occurences with LAMBDA as set
# 0.091
dpois(x=4, lambda=1.5)                   #prob X=4 occurences with LAMBDA as set
# 0.047
dpois(x=0:3, lambda=0.5)                 #prob x in 0,1,2,3 with LAMBDA as set
# 0.607 0.303 0.076 0.013

##### PLOT RFM DISTRIBUTION ##########################
library(sm)
### R RECENCY - Hist Plot & Filled Density Plot
# Filled Density Plot
hist(rec$recency, breaks=100, col="red", xlim=c(0,800))
d2 <- density(rec$recency)
#d <- density(rec$LTV)

N <- 800
d <- density(rnbinom(N, 7, .03))
plot(d)
plot(d2, main="DISTRIBUTION OF R OVER TIME (Days since last visit)", ylim=c(0,0.005))
polygon(d2, col="red", border="blue")
lines(d)
polygon(d, col=rgb(0, 1, 0, 0.5), border="blue")

sd(recency$recency)
###################
### F FREQUENCY - Hist Plot & Filled Density Plot
dd <- freqwithcounts
hist(dd$times, breaks=200, col="red", xlim=c(0,50))
dd <- freqwithcounts[freqwithcounts$times!=1,]       # NO one-timers
hist(dd$times, breaks=200, col="red", xlim=c(0,20))

m <- mean(dd$times)
std <- sd(dd$times)

d <- density(log(dd$times))
plot(d, main="Kernel Density of VISITS IN RESTAURANT")
polygon(d, col="red", border="blue")
#with ggplot2 and log y
ggplot(dd) + 
  geom_histogram(aes(x=times), bins=100, fill="red", color="black") + 
  xlim(0,100) + 
  xlab("Times at the restaurant") +
  ylab("Nr.of Customers (LOG)") +
  ggtitle("DISTRIBUTION OF F") +
  scale_y_log10()
summary(dd$times)

### Cumulative frequency
# plot asap
obj <- as.data.frame(table(freqwithcounts$times))
plot(obj$Var1,obj$Freq,type='l')
# plot hist
obj$Var1 <- as.integer(obj$Var1)
obj$Freq <- as.numeric(obj$Freq) 
hist(obj$Var1, breaks=68, col="red")
# plot density
d <- dpois(x=0:100, lambda=0.5)
plot(d,type='l',color="blue")
# plot 
cumul <- cumsum(obj$Freq)
cumperc <- cumul/nrow(freqwithcounts)
obj <- cbind(obj,cumperc)
ggplot(obj) + geom_line(aes(x=Var1, y=cumperc)) + xlim(0,10)

# Shapiro-Wilk test of normality (p<0.05 = reject normality) 
shapiro.test(obj$Freq)

##################
### M MONETARY 
lowrec <- rec[rec$LTV<500,]
highrec <- rec[rec$LTV>=500,]
hist(lowrec$LTV, breaks=20, col="red", xlim=c(0,500))
hist(highrec$LTV, breaks=38, col="red", xlim=c(0,10000))

hist(rec$LTV, breaks=5, col="red")
hist(rec$LTV, breaks=5000, col="red", xlim=c(0,1000))
summary(v3$LTV)
# Filled Density Plot
d <- density(payments$total)
d <- density 
# dd <- orders[(orders$seats>0),]
#d <- density(dd)
plot(d, main="Kernel Density of SPENT OF CUSTOMER", xlim=c(0,600))
polygon(d, col="red", border="blue")

# Positive Skewed (left) Distribution
N <- 10000
x <- (rnbinom(N, 10, .7))
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue', xlab=' ', ylab=' ', 
     main='Positive Skewed')
lines(density(x,bw=1), col='red', lwd=3)

#with ggplot2
ggplot(rec) + 
  geom_histogram(aes(x=LTV), bins=100, fill="red", color="black") + 
  xlim(0,1000) + 
  xlab("LTV (LifeTimeValue) $$$") +
  ylab("Nr.of Customers") +
  ggtitle("DISTRIBUTION OF M")
summary(rec$LTV)





# F FREQUENCY - Hist Plot & Filled Density Plot
dd <- freqwithcounts
hist(dd$times, breaks=440, col="red", xlim=c(0,20))
dd <- freqwithcounts[freqwithcounts$times!=1,]       # NO one-timers
hist(dd$times, breaks=440, col="red", xlim=c(0,20))
d <- density(log(dd$times))
plot(d, main="Kernel Density of VISITS IN RESTAURANT")
polygon(d, col="red", border="blue")

qpois(freqwithcounts$times,lambda=1)
d <- ppois(x=freqwithcounts$times, lambda=1)
plot(d)


# M MONETARY 
# Filled Density Plot
d <- density(payments$total)
# dd <- orders[(orders$seats>0),]
#d <- density(dd)
plot(d, main="Kernel Density of SPENT OF CUSTOMER", xlim=c(0,600))
polygon(d, col="red", border="blue")

# Positive Skewed (left) Distribution
N <- 10000
x <- (rnbinom(N, 10, .7))
hist(x, 
     xlim=c(min(x),max(x)), probability=T, nclass=max(x)-min(x)+1, 
     col='lightblue', xlab=' ', ylab=' ', 
     main='Positive Skewed')
lines(density(x,bw=1), col='red', lwd=3)

# Filled Density Plot
hist(recency$recency, breaks=20, col="red", xlim=c(0,720))
d <- density(recency$recency)
plot(d, main="Kernel Density of DAYS SINCE LAST VISIT")
polygon(d, col="red", border="blue")
sd(recency$recency)


# Filled Density Plot
d <- density(payments$total)
# dd <- orders[(orders$seats>0),]
#d <- density(dd)
plot(d, main="Kernel Density of SPENT OF CUSTOMER", xlim=c(0,600))
polygon(d, col="red", border="blue")

######## HISTOGRAM AND DENSITY PLOTS
# Colored Histogram with Different Number of Bins
#hist(payments$total, breaks=100, col="red")
hist(freqwithcounts$times, breaks=200, col="red", xlim=c(0,10))

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- freqwithcounts$times 
#x <- mtcars$mpg
h<-hist(x, breaks=200, col="red", xlab="Times in Restaurant", 
        main="Histogram with Normal Curve"
        , xlim=c(0,10)
) 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

# Filled Density Plot
#d <- density(mtcars$mpg)
d <- density(freqwithcounts$times)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")

##### Compare MPG distributions for cars with 
# 4,6, or 8 cylinders
library(sm)
attach(mtcars)

# create value labels 
cyl.f <- factor(cyl, levels= c(4,6,8),
                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 

# plot densities 
sm.density.compare(mpg, cyl, xlab="Miles Per Gallon")
title(main="MPG Distribution by Car Cylinders")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)

##### Compare times distributions for customers with M 1 to 5
library(sm)

# create value labels 
#cyl.f <- factor(cyl, levels= c(4,6,8),
#                labels = c("4 cylinder", "6 cylinder", "8 cylinder")) 

# plot densities 
sm.density.compare(freq$times, RFM$M, xlab="Times at Restaurant")
title(main="Times Distribution by M")

# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)


################################################
V <- rpois(43678, 0.5)
hist(V,breaks = 220,xlim=c(0:220))
dpois(c(0:2),0.5)
#barplot(table(V))
p <- qpois(c(0.2,0.2,0.2,0.2,0.2),lambda=0.5,lower.tail=TRUE)
plot(p)
##############
lambda <- 0.5
tMax <- 100

## find the number 'n' of exponential r.vs required by imposing that
## Pr{N(t) <= n} <= 1 - eps for a small 'eps'
n <- qpois(1 - 1e-8, lambda = lambda * tMax)

## simulate exponential interarrivals the
X <- rexp(n = n, rate = lambda)
S <- c(0, cumsum(X))
plot(x = S, y = 0:n, type = "s", xlim = c(0, tMax)) 

## several paths?
nSamp <- 50
## simulate exponential interarrivals
X <- matrix(rexp(n * nSamp, rate = lambda), ncol = nSamp,
            dimnames = list(paste("S", 1:n, sep = ""), paste("samp", 1:nSamp)))
## compute arrivals, and add a fictive arrival 'T0' for t = 0
S <- apply(X, 2, cumsum)
S <- rbind("T0" = rep(0, nSamp), S)
head(S)
## plot using steps
matplot(x = S, y = 0:n, type = "s", col = "darkgray",
        xlim = c(0, tMax),
        main = "Homogeneous Poisson Process paths", xlab = "t", ylab = "N(t)")
