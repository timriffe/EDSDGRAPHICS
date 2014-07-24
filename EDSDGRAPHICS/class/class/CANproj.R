# we're going to do a population projection assuming constancy of jump-off conditions,
# female dominace, and a constant 1.05 sex ratio at birth.

# don't get too confused at the projection code itself, the graphics part is
# the important bit: you'll do projections in a later course block.

# -----
# 1) load data in: male and female lifetables, and population counts

# change the path to point to where you have the file
flt <- read.table("fltper_1x1.txt",na.string=".",skip=2,header=TRUE,as.is=TRUE)
mlt <- read.table("mltper_1x1.txt",na.string=".",skip=2,header=TRUE,as.is=TRUE)
pop <- read.table("Population.txt",na.string=".",skip=2,header=TRUE,as.is=TRUE)
# clean the age columns like we did manually before:
removeplus <- function(x){
	x$Age <- as.numeric(gsub(pattern="\\+","",x$Age))
	return(x)
}
flt <- removeplus(flt)
mlt <- removeplus(mlt)
pop <- removeplus(pop)
# year 1949 pop has a high and low pop estimate, due to territorial changes, we take
# the high estimate:
indrm <- pop$Year == "1949-"
pop <- pop[!indrm,]
pop$Year <- as.numeric(gsub(pattern="\\+","",pop$Year))



# remove 2008, since last rate data from 2007, not a necessary step
indrm <- pop$Year==2008
pop <- pop[!indrm,]
# let's do a quick animation of Canada's historical population

# this time, instead of iterating over indices, we iterate over years
#library(Pyramid)
#for (i in years){
#	Pyramid(pop$Male[pop$Year==i],
#			pop$Female[pop$Year==i],
#			widths=rep(1,111),
#			xlim=c(-1.5,1.5),
#			year=i,
#			border.males="black",
#			border.females="black")
#	Sys.sleep(.1)
#}
#
## let's make our projection matrix:
library(Matrix)
# chances of surviving to end of each age interval given that you started the interval:
# could have also taken 1-qx, basically same thing
mpx <- mlt$lx[mlt$Year==2007][2:111]/mlt$lx[mlt$Year==2007][1:110]
fpx <- flt$lx[flt$Year==2007][2:111]/flt$lx[flt$Year==2007][1:110]
# fertility, imputing zeros for ages with no fertility (length 111)
fx <- as.numeric(c(rep(0,12),CAN[,"2007"],rep(0,55)))
# assume 1.05 Sex Ratio at Birth
SRB <- 1.05
# pm = proportion male
pm <- SRB/(1+SRB)
# a male Leslie matrix (no fert)
mL <- cbind(rbind(0,diag(mpx)),0)
fL <- cbind(rbind((1-pm)*fx[1:110],diag(fpx)),0)
# TSL stands for Two-Sex Leslie (a cheap one)
TSL <- bdiag(mL,fL)
TSL <- as.matrix(TSL)
TSL[1,112:222] <- pm*fx # insert the fx for boys

# a rough outline of a two-sex female-dominant Leslie matrix:
#image(Matrix(TSL))
# notice how the fertility is in 2 lines: one for boy births (top) and another for girl births (middle horizontal)

# stack males and females end on end:
p0 <- c(pop$Male[pop$Year==2007],pop$Female[pop$Year==2007])
# p0 is jump-off population

# unlike in Excel, when you do projections in R, there's no practical limit to the
# number of years forward you can go. You can seriously project forward to a state 
# of stability. 250 years will do just fine. First we get a matrix to hold the population
# in, then we iterate through and fill it up with the projected population.
popproj <- matrix(nrow=222,ncol=250)
popproj[,1] <- p0
for (i in 2:250){
	popproj[,i] <- TSL%*%popproj[,(i-1)]
}
males <- popproj[1:111,2:250]
females <- popproj[112:222,2:250]
colnames(females) <- colnames(males) <- 2008:2256
rownames(females) <- rownames(males) <- 0:110	

#library(Pyramid)
#for (i in 1:250){
#	Pyramid(males[,i],
#			females[,i],
#			widths=rep(1,111),
#			xlim=c(-1.5,1.5),
#			year=i+2006,
#			border.males="black",
#			border.females="black")
#	Sys.sleep(.1)
#}

# go from wide to long:
library(reshape)
males <- melt(males)
males <- cbind(as.numeric(males$X2),as.numeric(males$X1),males$value)
females <- melt(females)
females <- cbind(as.numeric(females$X2),as.numeric(females$X1),females$value)
# bind projected years to historical years
pop2 <- cbind(females,males[,3],males[,3]+females[,3])
colnames(pop2) <- colnames(pop)
pop <- rbind(pop,pop2)
pop <- as.matrix(pop)


# 'pop' is all we wanted for the pyramid animation to stability. clean up:
rm("females","males","flt","mlt","fx","pop2","popproj","TSL","SRB","pm","fL","mL","mpx","fpx","p0")

