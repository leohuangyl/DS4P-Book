library(foreign)
#install.packages("scatterplot3d")
library(scatterplot3d)
library(car)

#install.packages("HH")
library(HH)

setwd("~/Dropbox/Classes/PS363/QPM2016/Lectures/Lecture18")
dir()
Abram <- read.dta("gdpvote.dta")


Abram$JuneApp<-c(-8, -26, 50, 30, 59, -7, 23, 5, -27, 19, 16, -17, 10, 18, -1, -30, 2)
Abram$Inc<-c(0, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1)
Abram<-rbind(Abram, c(year=2016, q2gdp=1.4, vote=50.5, term=0, JuneApp=6, Inc=0))
model1<-lm(formula = vote ~ q2gdp + Inc, data = Abram[1:18,])
Abram
summary(model1)
predict(model1, newdata=Abram[18,])
mean(Abram$vote[Abram$Inc==1])
mean(Abram$vote[Abram$Inc==0])

setwd("~/Dropbox/Classes/PS363/QPM2016/Lectures/Lecture20")
pdf("AbramsPred2016.pdf", width=5, height=3)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=1)
plot(Abram$q2gdp, Abram$vote, ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch="", xlim=c(-8, 10.5), cex=.8)
abline(h=seq(45, 65, by=5), col="gray80")
abline(lm(Abram$vote~Abram$q2gdp), lwd=2)
points(Abram$q2gdp[1:17], Abram$vote[1:17], ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch=19, xlim=c(-8, 10.5), cex=.8)
text(Abram$q2gdp[1:17]+.65, Abram$vote[1:17], Abram$year[1:17], cex=.6)
points(Abram$q2gdp[18], Abram$vote[18], ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch=19, xlim=c(-8, 10.5), cex=.8, col="red")
text(Abram$q2gdp[18]+.65, Abram$vote[18], Abram$year[18], cex=.6, col="red")
dev.off()

setwd("~/Dropbox/Classes/PS363/QPM2016/Lectures/Lecture20")
pdf("AbramsPred20162.pdf", width=5, height=3)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=1)
plot(Abram$JuneApp, Abram$vote, ylab="Incumbent party share of vote", xlab="Net Approval in June", ylim=c(44, 64), pch="", cex=.8, xlim=c(-40, 65))
abline(h=seq(45, 65, by=5), col="gray80")
abline(lm(Abram$vote~Abram$JuneApp), lwd=2)
points(Abram$JuneApp[1:17], Abram$vote[1:17], ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch=19, xlim=c(-8, 10.5), cex=.8)
text(Abram$JuneApp[1:17]+4, Abram$vote[1:17], Abram$year[1:17], cex=.6)
points(Abram$JuneApp[18], Abram$vote[18], ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch=19, xlim=c(-8, 10.5), cex=.8, col="red")
text(Abram$JuneApp[18]+4, Abram$vote[18], Abram$year[18], cex=.6, col="red")
dev.off()





pdf("TFC.pdf", width=5, height=3)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=1)
plot(Abram$q2gdp, Abram$vote, ylab="Incumbent party share of vote", xlab="Q2 GDP Growth", ylim=c(44, 64), pch=19, xlim=c(-8, 10.5), cex=.8)
abline(h=seq(45, 65, by=5), col="gray80")
points(Abram$q2gdp[Abram$Inc==0], Abram$vote[Abram$Inc==0], col="red", pch=19)
points(Abram$q2gdp[Abram$Inc==1], Abram$vote[Abram$Inc==1], col="blue", pch=19)
text(Abram$q2gdp[Abram$Inc==1]+.65, Abram$vote[Abram$Inc==1], Abram$year[Abram$Inc==1], col="blue", cex=.6)
text(Abram$q2gdp[Abram$Inc==0]+.65, Abram$vote[Abram$Inc==0], Abram$year[Abram$Inc==0], col="red", cex=.6)
abline(lm(Abram$vote~Abram$q2gdp), lwd=1, lty=2, col="gray70")
abline(a=model1$coef[1]+model1$coef[3], b=model1$coef[2], lwd=2, col="blue")
abline(a=model1$coef[1], b=model1$coef[2], col="red", lwd=2)
dev.off()


pdf("ThreeScatter.pdf", width=5, height=4)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=1)
scatterplotMatrix(~vote+ q2gdp+JuneApp, data=Abram, var.labels = c("Vote Share", "2nd Quarter GDP", "June Approval"), smooth=F)
dev.off()

pdf("3DScatter.pdf", width=5, height=4)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=.8, cex.axis=.5)
regr2.plot(Abram$q2gdp, xlab="2nd Quarter GDP", Abram$JuneApp,
           ylab="June approval", Abram$vote, zlab="Vote share",
           main="", theta=20, phi=5, plot.back.planes=F)
dev.off()


this.sp<-scatterplot3d(Abram$q2gdp, Abram$JuneApp, Abram$vote , xlab="2nd quarter GDP Growth",
                       ylab="June Trial-Heat Polling", zlab="Incumbent party vote share", angle=30,
                       highlight.3d=TRUE, pch=19, 4)
this.sp$plane3d(a08.m, lty.box = "solid")

pdf("AVPlot.pdf", width=5, height=4)
par(mar=c(2,2,.5,.5), mgp=c(1,0,0), tcl=0, cex.lab=.8, cex.axis=.5)
avPlots(model2)
dev.off()

Abram

plot(Abram$q2gdp, Abram$vote)
abline(lm(Abram$vote~Abram$q2gdp))
points(Abram$q2gdp[18], Abram$vote[18], col="red", pch=19)
points(Abram$q2gdp[Abram$Inc==0], Abram$vote[Abram$Inc==0], col="red", pch=19)
points(Abram$q2gdp[Abram$Inc==1], Abram$vote[Abram$Inc==1], col="blue", pch=19)
text(Abram$q2gdp[Abram$Inc==1], Abram$vote[Abram$Inc==1], Abram$year[Abram$Inc==1], col="blue")
text(Abram$q2gdp[Abram$Inc==0], Abram$vote[Abram$Inc==0], Abram$year[Abram$Inc==0], col="red")
summary(model1)
abline(a=model1$coef[1]+model1$coef[3], b=model1$coef[2])
abline(a=model1$coef[1], b=model1$coef[2], col="red")







summary(lm(vote~q2gdp, data=Abram))
model2<-lm(vote ~ q2gdp + JuneApp, data = Abram[1:18,])
summary(model2)
Abram
plot()

Abram$Year



?predict
