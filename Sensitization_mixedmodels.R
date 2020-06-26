rm(list = ls())
setwd("C:/Users/Ritwika Mukherjee/Documents/StrikeExperiments/Strikeexperiments")

library(lme4) # for mixed models
library(car) # for statistical hypothesis tests
library(bbmle)
dat = read.csv("Sensitization_mixedmodels.csv")
str(dat)

table(dat$Drugs)
table(dat$Side)
with(dat, table(Drugs, Side))

dat$Treatment <- NA
for(i in 1:nrow(dat)){
  if(dat$Drugs[i] == "DMSO" | dat$Drugs[i] == "Saline") { dat$Treatment[i] <- "Control" } else { 
    dat$Treatment[i] <- "Drugs"}
}
table(dat$Treatment)


# Approach 1: We looked at everything 

m1 <- glmer(Strike~Drugs*Side+(1|UID)+(1|delta.t),family=binomial,data=dat)
m2 <- glmer(Strike~Drugs*Side+(1|UID),family=binomial,data=dat)
m3 <- glmer(Strike~Drugs*Side+(1|delta.t),family=binomial,data=dat)
m4 <- glm(Strike~Drugs*Side,family=binomial,data=dat)
ICtab(m1,m2,m3,m4, # any models within 2 dAIC are winning models, 
      #BUT random effects it's best to go with the most parsimonious model. In this case is model 2. 
      base=T, # Base gives you the AIC values
      weights = T)  # weights gives you the weights of the model
# For fixed effects, you can do model averaging for models within 2dAIC. 

summary(m2)
Anova(m2)

# Marginal hypothesis testing for your fixed effects
m5 <- glmer(Strike~Drugs+Side+Drugs:Side + (1|UID), family=binomial, data=dat) # this is the same model as m2
m6 <- glmer(Strike~Drugs+Side + (1|UID), family=binomial, data=dat)

lmtest::lrtest(m5,m6)
#m1 still wins
Anova(m5)
summary(m5)
###
fixef(m1)
coef(m1)

fixef(m6)

dat$DrugSide <- paste(dat$Drugs, dat$Side, sep = ".")
m6a <- glmer(Strike~ - 1 + DrugSide + (1|UID), family=binomial, data=dat)

plogis(fixef(m6a))

predict <- predict(m6, newdata = data.frame(Drugs = "QNB", Side = c("Same", "Opp")), type = "response")

confint(m6a)


## Approach 2: just looking at the two drugs

dat1 <- subset(dat, Drugs != "DMSO" & Drugs != "Saline")


d1 <- glmer(Strike~Drugs*Side+(1|UID)+(1|delta.t),family=binomial,data=dat1)
d2 <- glmer(Strike~Drugs*Side+(1|UID),family=binomial,data=dat1)
d3 <- glmer(Strike~Drugs*Side+(1|delta.t),family=binomial,data=dat1)
d4 <- glm(Strike~Drugs*Side,family=binomial,data=dat1)
ICtab(d1,d2,d3,d4, # any models within 2 dAIC are winning models, 
      #BUT random effects it's best to go with the most parsimonious model. In this case is model 2. 
      base=T, # Base gives you the AIC values
      weights = T)  # weights gives you the weights of the model
# For fixed effects, you can do model averaging for models within 2dAIC. 

summary(d2)
Anova(d2)

# Marginal hypothesis testing for your fixed effects
d5 <- glmer(Strike~Drugs+Side+Drugs:Side + (1|UID), family=binomial, data=dat1) # this is the same model as m2
d6 <- glmer(Strike~Drugs+Side + (1|UID), family=binomial, data=dat1)

lmtest::lrtest(d5,d6)

d7 <- glmer(Strike~Drugs + (1|UID), family=binomial, data=dat1)
d8 <- glmer(Strike~Side + (1|UID), family=binomial, data=dat1)

lrtest(d6, d7)


## Approach 3: "drug" vs "control"

cd1 <- glmer(Strike~Treatment*Side+(1|UID)+(1|delta.t),family=binomial,data=dat)
cd2 <- glmer(Strike~Treatment*Side+(1|UID),family=binomial,data=dat)
cd3 <- glmer(Strike~Treatment*Side+(1|delta.t),family=binomial,data=dat)
cd4 <- glm(Strike~Treatment*Side,family=binomial,data=dat)
ICtab(cd1,cd2,cd3,cd4, # any models within 2 dAIC are winning models, 
      #BUT random effects it's best to go with the most parsimonious model. In this case is model 2. 
      base=T, # Base gives you the AIC values
      weights = T)  # weights gives you the weights of the model
# For fixed effects, you can do model averaging for models within 2dAIC. 

summary(cd2)
Anova(cd2) # only look at the interaction stat, should be the same as lrtest(). 
#If no interactions, then these stats are the same as lrtest()

# Marginal hypothesis testing for your fixed effects
cd5 <- glmer(Strike~ Treatment+Side+Treatment:Side + (1|UID), family=binomial, data=dat) # this is the same model as m2
cd6 <- glmer(Strike~Treatment+Side + (1|UID), family=binomial, data=dat)
cd7 <- glmer(Strike~Treatment + (1|UID), family=binomial, data=dat)
cd8 <- glmer(Strike~Side + (1|UID), family=binomial, data=dat)

lmtest::lrtest(cd5,cd6,cd7,cd8)
ICtab(cd5,cd6,cd7,cd8)
Anova(cd5)
Anova(cd6)
Anova(cd7)
Anova(cd8)

#didnt work!
#library (multcomp)
#summary(glht(cd5, mcp(Treatment:Side="Tukey")))

#Posthoc test for the differences
library(emmeans)
emm = emmeans(cd5, ~ Treatment * Side)
pairs(emm)
# or for simple comparisons
pairs(emm, simple = 'each')


cd5a <- glmer(Strike~Treatment+Side+Treatment:Side + (1|UID), family=binomial, data=dat) 

confint(cd5a)


plogis(fixef(cd5))#intercept is control.opp and add whatever is important to it after in form of 1*fixef(cd2[2])

control.opp <- fixef(cd2)[1]*1 +  fixef(cd2)[2]*0 + fixef(cd2)[3]*0 + fixef(cd2)[4]*0
drug.opp <- fixef(cd2)[1]*1 +  fixef(cd2)[2]*1 + fixef(cd2)[3]*0 + fixef(cd2)[4]*0
control.same <- fixef(cd2)[1]*1 +  fixef(cd2)[2]*0 + fixef(cd2)[3]*1 + fixef(cd2)[4]*0
drug.same <- fixef(cd2)[1]*1 +  fixef(cd2)[2]*1 + fixef(cd2)[3]*1 + fixef(cd2)[4]*1

control.opp ; drug.opp ; control.same ; drug.same

# Estimate 95% CI
library(msm)
se.est1 <- deltamethod(~ x1*1 + x2*0 + x3*0 + x4*0, fixef(cd2), vcov(cd2))
se.est2 <- deltamethod(~ x1*1 + x2*1 + x3*0 + x4*0, fixef(cd2), vcov(cd2))
se.est3 <- deltamethod(~ x1*1 + x2*0 + x3*1 + x4*0, fixef(cd2), vcov(cd2))
se.est4 <- deltamethod(~ x1*1 + x2*1 + x3*1 + x4*1, fixef(cd2), vcov(cd2))

est.dat <- data.frame(Treatment = c("Control", "Drug", "Control", "Drug"), 
                      Side = c("Same", "Same","Opposite", "Opposite" ), 
                      Strike.prob = c(control.same, drug.same, control.opp, drug.opp),
                      lower.CI = c(control.same - 1.96*se.est3, drug.same - 1.96*se.est4,
                                   control.opp - 1.96*se.est1, drug.opp - 1.96*se.est2),
                      upper.CI = c(control.same + 1.96*se.est3, drug.same + 1.96*se.est4,
                                   control.opp + 1.96*se.est1, drug.opp + 1.96*se.est2))

library(ggplot2)

ggplot(est.dat, aes(x = Treatment, y = plogis(Strike.prob),  fill=Side)) + 
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = plogis(lower.CI), ymax = plogis(upper.CI)),width = 0.2,
                position = position_dodge(width = 0.9)) +
  scale_y_continuous(limits = c(0,1)) + ylab("Probability of strike responses")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_grey(start=0.4)+
  guides(fill = guide_legend()) 
   

