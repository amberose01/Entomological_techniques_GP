#Entomolgoical Techniques Group Project
#Testing the impact of fungal supplements to bumble bee offspring, size, and survival


library(readr)
map_brd <- read_csv("map_brd.csv")
summary(map_brd)

# Categorical variables 

##Total number of offspring
ggplot(map_brd, mapping=aes(y=total.offspring, x=treatment, fill=treatment))+
  geom_boxplot()+ 
  scale_fill_manual(values=c("cornflowerblue","forestgreen","hotpink1"))+
  theme_classic() + 
  xlab("Pollen Treatment") + 
  ylab("Total Number of Offspring") + 
  theme(axis.text.x = element_text(size = 13, face = "italic"), axis.title.x = element_text(size = 15, face = "bold"), 
        axis.title.y = element_text(size = 15, face = "bold")) + scale_x_discrete(labels=c('P. columbinus', 'Control', 'P. djamor'))+
  theme_classic()

model_total<-glm(total.offspring~treatment, family = poisson(link = "log"), map_brd)
summary(model_total)

library(emmeans)
emmeans(model_total, pairwise~treatment)

## Number of emerged males
ggplot(map_brd, mapping=aes(y=num.males, x=treatment, fill=treatment))+
  geom_boxplot()+
  scale_fill_manual(values=c("cornflowerblue","forestgreen","hotpink1"))+
  theme_classic() + 
  xlab("Pollen Treatment") + 
  ylab("Total Number of Emerged Males") + 
  theme(axis.text.x = element_text(size = 13, face = "italic"), axis.title.x = element_text(size = 15, face = "bold"), 
        axis.title.y = element_text(size = 15, face = "bold")) + scale_x_discrete(labels=c('P. columbinus', 'Control', 'P. djamor'))+
  theme_classic()
  
model_males<-glm(num.males~treatment, family = poisson(link = "log"), map_brd)
summary(model_males)

library(emmeans)
emmeans(model_males, pairwise~treatment)

## Larval ejection
ggplot(map_brd, mapping=aes(y=larval.ejection, x=treatment, fill=treatment))+
  scale_fill_manual(values=c("cornflowerblue","forestgreen","hotpink1"))+
  theme_classic() + 
  xlab("Pollen Treatment") + 
  ylab("Total Number of Larval Ejection") + 
  theme(axis.text.x = element_text(size = 13, face = "italic"), axis.title.x = element_text(size = 15, face = "bold"), 
        axis.title.y = element_text(size = 15, face = "bold")) + scale_x_discrete(labels=c('P. columbinus', 'Control', 'P. djamor'))+
  geom_boxplot()+
  theme_classic()

model_larvae<-glm(larval.ejection~treatment, family = poisson(link = "log"), map_brd)
summary(model_larvae)

library(emmeans)
emmeans(model_larvae, pairwise~treatment)


#Continuous Variables (Performing tests of assumption, evaluating using anova)

##Average male body size
ggplot(map_brd, mapping=aes(y=male.avg.size, x=treatment))+
  geom_boxplot()+
  theme_classic()+
  xlab("treatment")+
  ylab("average male size (cm)")
#Continuous
plot(map_brd$male.avg.size)

#normality
hist(map_brd$male.avg.size)
qqnorm(map_brd$male.avg.size)
qqnorm(log(map_brd$male.avg.size))
shapiro.test(map_brd$male.avg.size)
  #not normal, need to log transform

# Equal variance
library(tidyverse)
boxplot(male.avg.size ~ treatment, data = map_brd)
stripchart(male.avg.size ~ treatment, data = map_brd, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)
bartlett.test(map_brd$male.avg.size ~ map_brd$treatment)
  #variance is equal
fit_mas<-lm(log(male.avg.size+1)~treatment, data=map_brd)
anova(fit_mas)
    #no treatment effect



## Average Worker body size
#Continuous
ggplot(map_brd, mapping=aes(y=worker.avg.size, x=treatment))+
  geom_boxplot()+
  theme_classic()+
  xlab("treatment")+
  ylab("average worker size (cm)")

plot(map_brd$worker.avg.size)

#normality
hist(map_brd$worker.avg.size)
qqnorm(map_brd$worker.avg.size)
    #data is normal

# Equal variance
library(tidyverse)
boxplot(worker.avg.size ~ treatment, data = map_brd)
stripchart(worker.avg.size ~ treatment, data = map_brd, pch = 19, add = TRUE,
           vertical = TRUE, method = "jitter", jitter = 0.2)
bartlett.test(map_brd$worker.avg.size ~ map_brd$treatment)
    #no difference in variance
fit_was<-lm(worker.avg.size~treatment, data=map_brd)
anova(fit_was)
  #no treatment effect



## Average male fat content

