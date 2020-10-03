install.packages('robustbase',repos='http://cran.us.r-project.org')
install.packages('ggplot2',repos='http://cran.us.r-project.org')

library(foreign)
library(robustbase)
library(ggplot2) 

podaci <- read.spss("C:\\user\\savesnost_i_konzumacija_voca.sav", to.data.frame=T)

OLS <- lm(Konzumacija_voca ~ Emocionalnost + Ekstraverzija + Saradljivost + Savesnost + Otvorenost,
	data=podaci, na.action=na.exclude)
	summary(OLS)

MM <- lmrob(Konzumacija_voca ~ Emocionalnost + Ekstraverzija + Saradljivost + Savesnost + Otvorenost,
	data=podaci,na.action=na.exclude,method ="MM")
	summary(MM)

# Slika 2

ggplot(data=podaci, aes(x=Savesnost, y=Konzumacija_voca, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lm, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lm, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(0,5,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija voca")

ggplot(data=podaci, aes(x=Savesnost, y=Konzumacija_voca, color="gray"), legend=FALSE) + 
  geom_point(color="gray") + guides(color=FALSE) +
  geom_smooth(method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=.5, color="gray40") +
  geom_smooth(aes(group=1), method=lmrob, se=FALSE, fullrange=FALSE, lty=1, size=2, color="red") +
  xlab("Savesnost") + scale_x_continuous(breaks=seq(0,5,by=1)) +
  ylab("Broj porcija voca sedmicno")  + ylim(0,150) +
  ggtitle("Savesnost i kozumacija voca")

# slika 1

library(MASS)
x <- seq(-5,5,0.1)

plot(x, psi.huber(x)*x, lwd=2, col='red', type='l', ylab = "Huberova psi-funkcija")
plot(x, psi.bisquare(x)*x, lwd=2, col='red', type='l', ylab = "Takijeva bikvadratna psi-funkcija")
