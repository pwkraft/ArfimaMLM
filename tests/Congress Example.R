###Analysis file to accompany Lebo and Weber (AJPS) RCS paper
###Last modification: September 3, 2013

rm(list=ls())
#### load libraries
require(lme4)
library(fracdiff)
library(fractal)
library(ggplot2)
library(tseries)
#install.packages("/data/Dropbox/packages/ArfimaMLM/ArfimaMLM_1.2.tar.gz", repos=NULL)
library(ArfimaMLM)

#### load data set
#Two datafiles are merged -- an aggregate data and an individual "roll call" level data file. The uploaded data file is the full.house file below, which is the file we analyze. Note tha the file only consists of House or Representatives data and the only votes analyzed are party votes.
setwd("/data/Dropbox/packages/ArfimaMLM/ArfimaMLM/tests")
load("full.house.replication.Rdata")

# # # ##Create density plots for supplementary materials. This figure is reported in Figure S5, Uncomment to run.

# full.house<-subset(full.house, full.house$year>1900)
 # full.house$decade<-NA
  # full.house$decade[full.house$year>=1900 & full.house$year<=1910]<-"1900-1910"
	  # full.house$decade[full.house$year>=1911 & full.house$year<=1920]<-"1911-1920"
	  # full.house$decade[full.house$year>=1921 & full.house$year<=1930]<-"1921-1930"
	  # full.house$decade[full.house$year>=1931 & full.house$year<=1940]<-"1931-1940"
	  # full.house$decade[full.house$year>=1941 & full.house$year<=1950]<-"1941-1950"
	  # full.house$decade[full.house$year>=1951 & full.house$year<=1960]<-"1951-1960"
	  # full.house$decade[full.house$year>=1961 & full.house$year<=1970]<-"1961-1970"
	  # full.house$decade[full.house$year>=1971 & full.house$year<=1980]<-"1971-1980"
	  # full.house$decade[full.house$year>=1981 & full.house$year<=1990]<-"1981-1990"
	 # full.house$decade[full.house$year>=1991 & full.house$year<=2000]<-"1991-2000"
	 # full.house$decade[full.house$year>=2001 & full.house$year<=2010]<-"2001-2010"

 # par(mfrow=c(5,2))
 # par(mar=c(1, 1.1,1.1, 1))
 # par(oma=c(1,1,1,1))

 # plot(density(full.house$majpu[full.house$decade=="1911-1920"& !is.na(full.house$decade)],  from=0, to=100), main="Majority/Minority Unity (1911-1920)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1911-1920"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1921-1930"& !is.na(full.house$decade)],  from=0, to=100), main="(1921-1930)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1921-1930"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1931-1940"& !is.na(full.house$decade)],  from=0, to=100), main="(1931-1940)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1931-1940"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1941-1950"& !is.na(full.house$decade)],  from=0, to=100), main="(1941-1950)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1941-1950"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1951-1960"& !is.na(full.house$decade)],  from=0, to=100), main="(1951-1960)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1951-1960"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1961-1970"& !is.na(full.house$decade)],  from=0, to=100), main="(1961-1970)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1961-1970"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1971-1980"& !is.na(full.house$decade)],  from=0, to=100), main="(1971-1980)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1971-1980"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1981-1990"& !is.na(full.house$decade)],  from=0, to=100), main="(1981-1990)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.08), xlim=c(0,100), xaxt='n', )
 # lines(density(full.house$minpu[full.house$decade=="1981-1990"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.08, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="1991-2000"& !is.na(full.house$decade)],  from=0, to=100), main="(1991-2000)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.10), xlim=c(0,100) )
 # lines(density(full.house$minpu[full.house$decade=="1991-2000"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.10, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)

 # plot(density(full.house$majpu[full.house$decade=="2001-2010"& !is.na(full.house$decade)],  from=0, to=100), main="(2001-2010)", xlab="Party Unity", col="darkgray",  ylim=c(0,0.10), xlim=c(0,100) )
 # lines(density(full.house$minpu[full.house$decade=="2001-2010"& !is.na(full.house$decade)],  from=0, to=100), col="black" )
 # legend(0,0.10, legend=c("Majority", "Minority"), col=c("darkgray", "black"), lty=1, cex=0.9)


# #Create year level aggregates of individual data sets

 year.house<-aggregate(cbind(full.house$majpu, full.house$minpu, full.house$wrdq2, full.house$longsession, full.house$shortsession, full.house$session1, full.house$session2, full.house$predMajPU, full.house$policyvote, full.house$procvote, full.house$daterankpercent, full.house$medmedn1, full.house$medmedn2, full.house$majn1s, full.house$majn2, full.house$majsize), list(full.house$year), mean, na.rm=T)

 year.house.sd<-aggregate(cbind(full.house$majpu, full.house$minpu), list(full.house$year), sd, na.rm=T)

 names(year.house.sd)<-c("year", "majpu", "minpu")
 names(year.house)<-c("year", "majpu", "minpu", "wrdq2", "longsession", "shortsession", "session1", "session2", "predMajPU", "policyvote", "procvote", "daterankpercent", "medmedn1", "medmedn2", "majn1s", "majn2s", "majsize")


majority<-rep("Majority", length(year.house$majpu))
minority<-rep("Minority", length(year.house$minpu))

graph.data<-data.frame(unity=c(year.house$majpu, year.house$minpu), type=c(majority, minority), year=as.Date(as.character(c(year.house$year,year.house$year)), "%Y"))



###A graph of party unity over time. This is Figure S4

pdf("partyunity1.pdf", width=8, height=7)
colours<-c(Majority="darkred", Minority="darkblue")
d2<-ggplot(graph.data, aes(x=year, y=unity, group=type))
d2+geom_line(aes(group=factor(type), colour=factor(type)))+scale_colour_manual(name="Unity", values=colours)+scale_x_date("Year")+labs(title="Majority and Minority Party Unity")+scale_y_continuous("Party Unity")
dev.off()

##A graph of standard deviation in unity over time. This is figure S4
majority<-rep("Majority", length(year.house.sd$majpu))
minority<-rep("Minority", length(year.house.sd$minpu))
graph.data<-data.frame(unity=c(year.house.sd$majpu, year.house.sd$minpu), type=c(majority, minority), year=as.Date(as.character(c(year.house.sd$year,year.house.sd$year)), "%Y"))

pdf("partyunity2.pdf", width=8, height=7)
colours<-c(Majority="darkred", Minority="darkblue")
d2<-ggplot(graph.data, aes(x=year, y=unity, group=type))
d2+geom_line(aes(group=factor(type), colour=factor(type)))+scale_x_date("Year")+labs(title="Standard Deviation - Party Unity")+scale_y_continuous("Party Unity (SD)")+scale_colour_manual(name="SD Unity", values=colours)
dev.off()

### Generate variables for analysis. Following the convention in RATS, to calculate h, we estimate h on the first differenced series, and then difference the series by d=1+H-0.5.

#error correction mechanism
ecm<-residuals(lm(year.house$majpu~year.house$minpu))
d <- hurstSpec(diff(ecm),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(ecm, d=d)
N=length(year.house$majpu)
ecm <- c(NA,x.fit[1:(N-1)])

#majpu
plot(as.ts(year.house$majpu))
d <- hurstSpec(diff(year.house$majpu),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$majpu, d=d)
residuals.majpu<-x.fit #majpu is (0, 0.77, 0)

#minpu
plot(as.ts(year.house$minpu))
d <- hurstSpec(diff(year.house$minpu),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$minpu, d=d)
residuals.minpu<-x.fit #minpu is (0, 0.81, 0)


#medmedn1
plot(as.ts(year.house$medmedn1))
d <- hurstSpec(diff(year.house$medmedn1),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$medmedn1, d=1)
residuals.medmedn1<-x.fit #medmedn1 is (0, 1.17, 0)

#medmedn2
plot(as.ts(year.house$medmedn2))
d <- hurstSpec(diff(year.house$medmedn2),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$medmedn2, d=d)
residuals.medmedn2 <-x.fit #medmedn2 is (0, 0.63, 0)

#majn1s
plot(as.ts(year.house$majn1s))
d <- hurstSpec(diff(year.house$majn1s),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$majn1s, d=d)
residuals.majn1s <-x.fit #majn1s is (0, 0.85, 0)

#majn2s
plot(as.ts(year.house$majn2s))
d <- hurstSpec(diff(year.house$majn2s),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$majn2s, d=d)
residuals.majn2s <-x.fit #majn2 is (0, 0.67, 0)


#majsize
plot(as.ts(year.house$majsize))
d <- hurstSpec(diff(year.house$majsize),  method="standard", sdf.method="multitaper")[1] + .5
x.fit<-diffseries(year.house$majsize, d=d)
residuals.majsize <-x.fit #majn1s is (0, 0.53, 0)

#Manually generate a lagged DV
N=length(year.house$majpu)
lag.y <- c(NA,year.house$majpu[1:(N-1)])

# #Auxiliary file to examine standard deviations from minority and majority party means across years to explore party cohesion
# sub.data<-data.frame(year=full.house$year, majpu=full.house$majpu, minpu=full.house$minpu)
# sub.data<-na.omit(sub.data)
# sd.min<-tapply(sub.data$minpu, sub.data$year, sd)
# sd.maj<-tapply(sub.data$majpu, sub.data$year, sd)



#The data file with all the predicted values, etc
agg.data<-data.frame(year.house,
 residuals.majn1s,          residuals.majn2s,
 residuals.majpu,           residuals.medmedn1,
 residuals.medmedn2,        residuals.minpu,
 residuals.majsize,         lag.y,
 ecm)

 merged.data<-merge(full.house, agg.data, by="year") #This is the final, analyzable data file. It is the same as the full.data, but with the white noise error based on the fractional differencing above.

#### Model Specification

#calculate individual effects
merged.data$ydif<-merged.data$majpu.x-(merged.data$majpu.y-merged.data$residuals.majpu) #The dependent variable, majpu
merged.data$minpu.within<-merged.data$minpu.x-merged.data$minpu.y #The key independent variable, minppu

##Drop the first five years
merged.data<-subset(merged.data, merged.data$year>1793)


#within variables. We do not fractionally difference these. We keep them in the same form as in full.data.house
#policyvote.x, session1.x, longsession.x

#z variables. Variables that do not vary within years, but they have been fractionally differenced

# "residuals.majn1s"   "residuals.majn2s"
# "residuals.majpu"    "residuals.medmedn1" "residuals.medmedn2"
# "residuals.minpu"    "residuals.majsize"

##Random interecepts model

mlm.fi<-lmer(ydif~
				minpu.within+ policyvote.x+longsession.x+session1.x+ #within effect
				residuals.minpu+ #Between year effects
				residuals.majn1s+residuals.majn2s+residuals.medmedn1+
				residuals.medmedn2+residuals.majsize+ecm+(1|year), data=merged.data)

# mlm.fi<-lmer(ydif~
				# minpu.within+ policyvote.x+longsession.x+session1.x+ #within effect
				# residuals.minpu+ #Between year effects
				# residuals.majn1s+residuals.majn2s+residuals.medmedn1+
				# residuals.medmedn2+residuals.majsize+ecm+(1|year), data=merged.data, REML=FALSE)

options("scipen"=100, "digits"=4)
summary(mlm.fi)

#Also generate a random coefficient model

mlm.house.re<-lmer(ydif~
				minpu.within+ policyvote.x+longsession.x+session1.x+ #within effect
				residuals.minpu+
				residuals.majn1s+residuals.majn2s+residuals.medmedn1+
				residuals.medmedn2+residuals.majsize+ecm+(1+minpu.within|year), data=merged.data)
#LR test
-2*logLik(mlm.fi)[1] - - 2*logLik(mlm.house.re)[1]


#retrieve slopes and generate Figure 4
t<-coef(mlm.house.re)$year
#Plot the slopes

Date<-c(1794:2010)
#cdate<-date(as.character(e))
slope.minpu<-t[,2]
cbind(Date, slope.minpu)

caption1<-paste(strwrap("Average Slope",  0.13))
data1<-data.frame(Date, slope.minpu)

pdf("randomeffects.pdf", width=8, height=7)
d1<-ggplot(data1, aes(x=Date, y=slope.minpu))
d1+geom_line(aes(), colour="darkgray")+scale_y_continuous("Slope for Minority Party Unity")+geom_abline(intercept=0.14, slope=0, colour="black")+geom_smooth(aes(), method="loess", size=1, colour="gray")+labs(title="Varying Slope (Minority Party Unity)")
dev.off()


##############################################################################################
### comparison of results

m1 <-lmer(ydif~minpu.within+ policyvote.x+longsession.x+session1.x+ #within effect
                     residuals.minpu+
                     residuals.majn1s+residuals.majn2s+residuals.medmedn1+
                     residuals.medmedn2+residuals.majsize+(1|year), data=merged.data)

m2 <-arfimaMLM(majpu.ydif~minpu.xdif + policyvote + longsession + session1 +
             minpu.fd + majn1s.fd + majn2s.fd+medmedn1.fd +
             medmedn2.fd + majsize.fd + (1|year),
             data=full.house, timevar="year")

m3 <-arfimaMLM(majpu.ydif~minpu.xdif + policyvote + longsession + session1 +
                 minpu.fd + majn1s.fd + majn2s.fd+medmedn1.fd +
                 medmedn2.fd + majsize.fd + (1|year),
               data=full.house[sample(1:nrow(full.house),nrow(full.house)),], timevar="year")

summary(m1)
summary(m2)
summary(m3)

### compare transformation of variables

m1var <- with(merged.data, cbind(ydif, minpu.within, policyvote.x, longsession.x, session1.x
                                  , residuals.minpu, residuals.majn1s, residuals.majn2s, residuals.medmedn1, residuals.medmedn2, residuals.majsize, year))
m2var <- with(m2$data.merged, cbind(majpu.ydif, minpu.xdif, policyvote, longsession, session1
                                    , minpu.fd, majn1s.fd, majn2s.fd, medmedn1.fd, medmedn2.fd, majsize.fd, year))
res <- NULL
for (i in 1:ncol(m1var)){
  res <- rbind(res, c(summary(na.omit(m1var[,i]-m2var[,i]))
                      ,sum(1*is.na(m1var[,i]-m2var[,i]))))
}
rownames(res) <- paste(colnames(m1var),colnames(m2var),sep=" - ")
colnames(res)[length(colnames(res))] <- "NA's"
res # there are no deviations at all if the exact values for the differencing parameters are used

# investigate the missing case in the difference of the dependent variable
head(cbind(merged.data$ydif,m2$data.merged$majpu.ydif),10)
casenum <- which(is.na(merged.data$ydif-m2$data.merged$majpu.ydif)); casenum
merged.data[(casenum-2):(casenum+2),]
m2$data.merged[(casenum-2):(casenum+2),]
# seems to be ok, the case is missing with both approaches


##############################################################################################
### including ecm and random coefficient model

m3 <-lmer(ydif~ minpu.within+ policyvote.x+longsession.x+session1.x+ #within effect
                     residuals.minpu+
                     residuals.majn1s+residuals.majn2s+residuals.medmedn1+
                     residuals.medmedn2+residuals.majsize+ecm+(1+minpu.within|year), data=merged.data)

m4 <-arfimaMLM(majpu.ydif~minpu.xdif + policyvote + longsession + session1 +
                 minpu.fd + majn1s.fd + majn2s.fd+medmedn1.fd +
                 medmedn2.fd + majsize.fd + ecm + (1 + minpu.xdif|year),
               data=full.house, timevar="year"
               , ecm=majpu.mean~minpu.mean)

summary(m3)
summary(m4)

