#For this assignment I am using a real data of round goy (small invasive fish) that I collected from the Caspian, the native habitat of this species.
#The data file has some body and gonad measurements, and information about the reproductive status and morph of this fish.
#The question I am trying to answer is if there is difference in investemnt in accessory glands in different male morphs of round goby (guarder and sneaker) in its native population.
#To answer the question I decided to fit an ANCOVA model to my dataset, with male morph type as a factor and total body mass as a covariate (to account for allometry in fish).

library(tidyverse)
library(car)
library(DHARMa)
library(dotwhisker)
library(stargazer)

NM_csv <- read_csv("https://raw.githubusercontent.com/SZarini/Stat744/main/NM-csv.csv")

#making sure male morph type is a factor
NM_csv$status <- as.factor(NM_csv$status)
class(NM_csv$status)

#Log transfroming accessory gland and total mass measurements 
NM_csv$logmass=log(NM_csv$total_mass)
NM_csv$log_acc_gland=log(NM_csv$acc_gland)

#Extracting the rows that have guarder (PM) and sneaker male (SM) morphs 
mydata <- NM_csv %>% filter(NM_csv$status == 'SM'|NM_csv$status == 'PM')

#Fitting the ANCOVA model
model1 <- lm(log_acc_gland~logmass*status, mydata)
summary(model1)

#Making a DHARMa diagnostic plot to make sure there is no issue with the model 
g1 <- DHARMa::simulateResiduals(model1)
plot(g1)
#The residual vs. predicted value plot shows an almost unified distribution of data points. The QQ plot also suggests that our model shouldn't have any significant issue.


#Making a coefficient plot
gg2 <- dotwhisker::dwplot(model1, by_2sd=TRUE)
g2 <- gg2 + geom_vline(xintercept=0,lty=2)
print(g2)
#The coefficient plot shows that the interaction between logmass (covariate) and status is not significant (confidence interval line crosses zero)
#It also shows the positive correlation between body mass and accessory gland mass (or investment), and that sneaker males invest significantly less than guarders on accessory glands


#I dropped the interaction term from my model and plotted the second model next to the first model for comparison
model2 <- lm(log_acc_gland~logmass+status, mydata)
summary(model2)
g3 <- dwplot(list(model1, model2))+ geom_vline(xintercept=0,lty=2)
print(g3)

#I am not quite sure, but does the model show the effect of body mass is bigger than the effect of status (male morph) on accessory gland investment? 


