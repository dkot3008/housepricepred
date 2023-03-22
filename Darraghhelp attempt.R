##other
library(stargazer)
library(ggplot2)
library(tidyverse)
install.packages("broom")
library(broom)
dat <- (example)
boxplot(Salary_9_mo ~ Gender, data = dat)
mod1 <- lm(Salary_9_mo ~ Avg_Cont_Grants, data = dat)
stargazer(mod1, type = "html", title = "Salary and Grant Contribution")


ggplot(dat, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_smooth(method = "lm", aes(colour = Gender))

mod2 <- lm(Salary_9_mo ~ Avg_Cont_Grants + Gender, data = dat)
dat_add <- augment(mod2)
stargazer(mod1, mod2, type = "html", title = "Salary and Grants, Gender")

ggplot(dat, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = dat_add, aes(y = .fitted, colour = Gender)) # we change our data to the fitted values of the additive model


mod3 <- lm(Salary_9_mo ~ Avg_Cont_Grants * Gender, data = dat)
stargazer(mod1, mod2, mod3, type = "html", title = "Three models compared")

dat_add <- augment(mod2, interval = "confidence")

ggplot(dat, aes(Avg_Cont_Grants, Salary_9_mo, group = Gender)) +
  geom_point(alpha = 0.5, aes(colour = Gender)) +
  geom_line(data = dat_add, aes(y = .fitted, colour = Gender)) +
  geom_ribbon(data = dat_add, 
              aes(ymin=.lower, ymax=.upper), alpha=0.2)
################more useful work

library(tidyverse)
library(stargazer)
library(dplyr)


dat <- train

summary(dat)
glimpse(dat)
dat %>%
  plot(AdjSalePrice,TrafficNoise)

plot()



ggplot()

dat%>%
  plot(AdjSalePrice$TrafficNoise)

dat%>%
  cor.test(AdjSalePrice,TrafficNoise)
summary(dat$TrafficNoise)
  
  ##traffic and house value
t <- lm(dat$AdjSalePrice~dat$TrafficNoise)
summary(t)
plot(t)

lm()

dat %>%
  ggplot(dat, mapping= aes(AdjSalePrice,BldgGrade)) +
  geom_smooth(method = "loess")

####
size <- lm(AdjSalePrice~SqFtTotLiving,data = dat)
bathroom <- lm(AdjSalePrice~Bathrooms,data = dat)
bedrooms <- lm(AdjSalePrice~Bedrooms,data = dat)
traffic <- lm(AdjSalePrice~TrafficNoise,data = dat)
BldgGrade <- lm(AdjSalePrice~BldgGrade,data = dat)
 <- lm(AdjSalePrice~,data = dat)
 <- lm(AdjSalePrice~,data = dat)


summary(lm(size~BldgGrade))
ggplot(dat, mapping = aes(size,traffic)) +
  geom_point() +
  geom_line() 



#wrong  
tr <- lm(dat1$AdjSalePrice~dat1$TrafficNoise)
summary(t)
plot(t)

dat1 <- (dat$AdjSalePrice > 1000000)





