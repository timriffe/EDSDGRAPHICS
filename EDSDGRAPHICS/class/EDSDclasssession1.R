# Some syntax showing an easy way to animate plots in R.

#    this makes sense if you have time series data: 
#    instead of showing a few snapshots, or having to 
#    choose 1, you can flip through year-by-year changes in data

# I'm going to offer some unsolicited advice at various points in this code regarding good and bad
# form for animations in presentations (learned the hard way)

# let's get some time series data from the HFD:

# these are age-specific period rates, downloaded 'as-is' from the HMD:
# change the path to point to where you have the file
setwd("/home/triffe/git/EDSDGRAPHICS/EDSDGRAPHICS/class")
CAN <- read.table("CANasfrRR.txt",na.string=".",skip=2,header=TRUE,as.is=TRUE)

# look at first few lines
head(CAN)

# look at summary
summary(CAN)
# notice that the 'Age' column is not numeric. This is annoying, but easy to fix
unique(CAN$Age)
# it's due to the '12-' and '55+', which indicate open age groups.
# the HMD puts those in there so that you don't get careless, but we'd
# much rather just have 12 and 55 so we can treat them as numeric.

# we first remove the - and +
CAN$Age <- gsub(pattern="\\-",replace="",CAN$Age)
CAN$Age <- gsub(pattern="\\+","",CAN$Age)
# the '\\' means that the symbol can be anywhere in the string

# then we change the column class:
CAN$Age <- as.numeric(CAN$Age)

#  FOR YOUR INFORMATION: you'll always have to do something like this for the age column
#                        with HMD or HFD data... If you use it frequently, then write yourself 
#                        a function to make like easy

# for convenience save vectors of the ages and years
ages <- unique(CAN$Age)
years <- unique(CAN$Year)

#    we can rearrange the data or not, but it's easier to think of the looping
#    if we have the data arranged in a matrix with years in columns
#    and ages going down rows.
library(reshape)
CAN <- cast(CAN, Age ~ Year, value='ASFR')
# the opposite of cast() is melt()...

# for a few reasons not worth getting into now, I prefer to have data in a matrix:
CAN <- as.matrix(CAN)

# by now, you know your way around plot()
plot(ages,CAN[,"1921"],type='l')

# get the plot looking nice:
plot(ages,
		CAN[,"1921"],
		type='l',
		main="Canada ASFR, 1921",
		xlab="Age",
		ylab="Fertility Rate",
		col="blue",
		lwd=2)
# add a summary statistic:
TFR <- round(sum(CAN[,"1921"]),digits=3) # rounded to 3 digits
# mean age at birth (given no mortality
# this is neat: we treat he fertility rates themselves as weights!
# assume birth as age midpoints, i.e. 15.5 instead of 15...
w <- CAN[,"1921"]/sum(CAN[,"1921"])
MAB <- round(sum((ages+.5)*w),digits=3)
# that could have been done in 1 line, but I'm trying to make this legible!
legend("topright",
		legend=c(paste("TFR =",TFR),
		paste("MAB =",MAB)))

# OK, now we want to do this for every year. There are 2 things
# you need to know:
# 1) set the y limit to be the same at each iteration so that the axes don't auto-resize
# 2) use Sys.sleep() to adjust the speed, otherwise R shoots through the whole thing at lightning speed...

# we loop down the columns of CAN, each of which is a year of data, always the same length...
for (i in 1:ncol(CAN)){
	plot(ages,
			CAN[,i],
			type='l',
			main=paste("Canada ASFR,",colnames(CAN)[i]), # pick out year name
			xlab="Age",
			ylab="Fertility Rate",
			col="blue",
			lwd=2,
			ylim=c(0,.28))
	# This part looks pretty complex, but you can recognize the steps from above:
	legend("topright",
			legend=c(paste("TFR =",round(sum(CAN[,i]),digits=3)),
					paste("MAB =",round(sum((ages+.5)*(CAN[,i]/sum(CAN[,i]))),digits=3))))
	# tell it to pause before jumping to next frame (seconds), to adjust the speed:
	#Sys.sleep(.5)
}

###### but what do you do with this?
# it's tempting to do this for fertility, but I wouldn't recommend it for a few reasons
# 1) a lexis surface plot would give you the same info and more
# 2) you save yourself the trouble of putting an animation in your presentation- no tech problems, 
# 3) in general, running an animation during a presentation captures attention = good / but,
#    you have to wait for them to finish = it can break your tempo, plus there's a good chance you'll
#    have to fiddle around with it, rewind, etc, to make a point, and you lose time that way

# So, first think if there's a better way to present the data, then you need to decide what format:
# for a compact format, use the 'animation' package from Yihui Xie. Follow the manual closely, 
# and you may need to download additional free software to compile it. I used to do that, but now
# I don't really think it's worth the trouble. Instead, you're better off using low-tech: save the 
# loop as a multipage pdf and flip through it manually: here's an example just for the syntax:
pdf(height=7,width=7,file="CAN_fert.pdf")
# same code inside:
for (i in 1:ncol(CAN)){
	plot(ages,
			CAN[,i],
			type='l',
			main=paste("Canada ASFR,",colnames(CAN)[i]), # pick out year name
			xlab="Age",
			ylab="Fertility Rate",
			col="blue",
			lwd=2,
			ylim=c(0,.28))
	# This part looks pretty complex, but you can recognize the steps from above:
	legend("topright",
			legend=c(paste("TFR =",round(sum(CAN[,i]),digits=3)),
					paste("MAB =",round(sum((ages+.5)*(CAN[,i]/sum(CAN[,i]))),digits=3))))
	# tell it to pause before jumping to next frame (seconds), to adjust the speed:
	#Sys.sleep(.5)
}
dev.off() # important to close the pdf device and save the file
# you'll find the pdf in your working directory getwd()
# -------------------------------------------------------------
# The same thing as a surface plot
#--------------------------------------------------------------
# Better because you can choose between age, period and cohort perspectives
# the details behind how this kind of surface plot are discussed in the tutorial I sent you
#install.packages("fields")
library(grDevices) # for the colorRampPalette() function
library(fields)    # for image.plot(), because it gives a legend automatically
fxcols <- colorRampPalette(c("white","blue", "green", "yellow", "orange"), space = "rgb")
image.plot(x=years+.5,
		y=ages+.5, 		# + .5 to center the cells
		t(CAN),         # transponse to orient surface correctly
		col=fxcols(200),# 200 evenly spaced colors over gradient
		asp=1,			# set aspect ratio to 1 (important)
		xlim=c(1921,2008), # last year = highest year +1
		ylim=c(12,56),  # highest age + 1
		ylab="Age",    
		xlab="Year",
		axes=FALSE)
rect(1921,12,2008,56)
axis(1,pos=12,at=seq(1920,2000,by=10),cex=.8)
axis(2,pos=1921,at=seq(10,55,by=5),cex=8)

# -------------------------------------------------------------
# So when is it worth doing an animation?
# 1) for didactic reasons (e.g. explain weak ergodicity)
# 2) for diagnostics: e.g. you're fitting parametric curves that include age:
#    loop over different values of your parameters, updating the graphics device
# 3) if it points out something otherwise hard to see in the data

# -------------------------------------------------------------
# Animate a projected population toward stability:
# -------------------------------------------------------------

# run external R script using source("")
source("CANproj.R")
# this generated a couple new objects
ls()
# pop is the one we care about, it's the historical and projected population of Canada:
# 1921:2256, where the last data year is 2007.

# if you're curious about the code behind it, the assumptions, etc, look at CANproj.R

# this loads the function from my Pyramid package. If you want to actually install the package
# and be able to load it by using library(Pyramid), then you need to download the zip file from
# my website and do install.packages("path to downloaded file.zip")
source("Pyramid.R")
for (i in 1921:2256){
	Pyramid(pop[pop[,"Year"]==i,"Male"],
			pop[pop[,"Year"]==i,"Female"],
			widths=rep(1,111),
			xlim=c(-1.5,1.5),
			year=i,
			border.males="black",
			border.females="black")
}


# and to save it as a pdf (just from 2007 until 2207)

pdf(height=7,width=7,"CanadaProj.pdf")
for (i in 2007:2207){
	Pyramid(pop[pop[,"Year"]==i,"Male"],
			pop[pop[,"Year"]==i,"Female"],
			widths=rep(1,111),
			xlim=c(-1,1),
			year=i,
			border.males="transparent",
			border.females="transparent")
	# this lets us plot a new thing over the old plot
	par(new=T)
	# this will be the pseudo-stable distribution (per the projection)
	# that way you can see the approach from the start. Just plot empty
	# bars, no fills.
	Pyramid(pop[pop[,"Year"]==2256,"Male"],
			pop[pop[,"Year"]==2256,"Female"],
			widths=rep(1,111),
			xlim=c(-1,1),
			year=i,
			border.males="black",
			border.females="black",
			fill.males="transparent",
			fill.females="transparent",
			main="",
			generations=FALSE,
			xax.lab="",
			xaxat=0,
			yaxat=0,
			yax.lab="",
			xlab="")
}
dev.off()









