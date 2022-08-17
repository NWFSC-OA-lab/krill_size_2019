library(ggplot2)
#Read in data
krill <- read.csv("C:/Users/Danielle.Perez/Documents/Krill 2019/krillSize/EndofStudyKrill_organizedbyFASTINGhours_edit.csv")

#Remove unnecessary columns
krill <- krill[-c(2, 5:7, 10:11, 13:26)]

#Remove ambient treatment
krill <- krill[!(krill$Treatment=="AMB"),]

#Rename columns
names(krill)[4] <- "telsonLength"
names(krill)[5] <- "wetWeight"

#Change MOATS to factor
krill$MOATS<-as.factor(krill$MOATS)
krill$telsonLength<-as.numeric(krill$telsonLength)

#Basic plot comparing telson length and wet weight
plot(krill$telsonLength, krill$wetWeight*1000)

lm(formula = krill$telsonLength ~ krill$wetWeight)

#Creating a Body condition score as the residual from a regression of log krill mass against telson length
body_condition <- lm(log(wetWeight) ~ telsonLength, data = krill)
print(body_condition)
summary(body_condition)
plot(body_condition)

#Add new column to krill data frame with residuals/body condition values 
krill$resid <- body_condition$residuals

#Add Fulton's K
krill$fulton <- krill$wetWeight*1000 / krill$telsonLength^3
##Ok, but did I do this right?? Only have telson length, not complete length, may need multiplier

#######Boxplots, all points#########

#Showing what it would look like without jitter
p <- ggplot(krill, aes(Treatment, fulton))
p + geom_point()
#With jitter
p <- ggplot(krill, aes(Treatment, fulton))
p + geom_jitter()

#Most basic boxplot
ggplot(krill, aes(Treatment, fulton)) +
  geom_boxplot()  +
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Basic boxplot with jitter
ggplot(krill, aes(Treatment, fulton)) +
  geom_boxplot()  + geom_jitter(shape=16) +
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Notched, no jitter
ggplot(krill, aes(Treatment, fulton)) +
  geom_boxplot(notch = TRUE) + 
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Fulton's K by treatment
ggplot(krill, aes(Treatment, fulton)) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape=16) +
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#######Boxplots without a few outliers###########

ggplot(krill, aes(Treatment, fulton))  + ylim(0, 4) +
  geom_boxplot()  +
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Basic boxplot with jitter
ggplot(krill, aes(Treatment, fulton)) + ylim(0, 4) +
  geom_boxplot()  + geom_jitter(shape=16) +
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Notched, no jitter
ggplot(krill, aes(Treatment, fulton)) + 
  geom_boxplot(notch = TRUE) + 
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")

#Fulton's K taking out that larger outliers
ggplot(krill, aes(Treatment, fulton)) + ylim(0, 4) +
  geom_boxplot(notch = TRUE) + geom_jitter(shape=16) + 
  ggtitle("Fulton's K at treatment") + xlab("Treatment") + ylab("K")