library(tidyverse)
library(car)
library(ggplot2)
library(gridExtra)

pancake <- read.csv("pancake.csv",stringsAsFactors=T)


#assumptions tests for anova
bartlett.test(Flavor~Fat, data=pancake)
bartlett.test(Flavor~Flour, data=pancake)
leveneTest(Flavor ~ Fat*Flour, data = pancake) 

#data distribution
plot1 <- pancake %>% ggplot(aes(x=Fat,y=Flavor)) +
                geom_jitter(width = 0.2) + theme_bw()
plot2 <- pancake %>% ggplot(aes(x=Flour,y=Flavor)) +
  geom_jitter(width = 0.2) + theme_bw()
grid.arrange(plot1, plot2, ncol=2)

#model
model <- aov(Flavor~Fat*Flour*Block,data=pancake)
summary(model)
plot(model)


#interaction plot
with(pancake, {
  interaction.plot(Fat,Block,Flavor,col=1:2, legend=F)
  interaction.plot(Flour,Block,Flavor,col=1:2, legend=F)
})

#Tukey test to compare treatments
TukeyHSD(model, which = "Flour")
TukeyHSD(model, which = "Fat")
