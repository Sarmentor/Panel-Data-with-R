Panel <-read.csv("data.csv")
Milk <- read.csv("data_milk.csv")
head(Panel)

#correlation
library("Hmisc")

correlation_matrix <- rcorr(as.matrix(Panel[,3:7]))

correlation_matrix

###########################################################################
############################ LSDV package #################################
###########################################################################

library(lsdv)

fe.panel <- Lsdv(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, n=246, t=6)
summary(fe.panel)

fe.panel <- Lsdv(MILK ~ LABOR + LAND + COWS + FEED, data = Milk, n=247, t=6)
summary(fe.panel)

###########################################################################
####################### Paneldata package #################################
###########################################################################

library(Paneldata)

fe.panel <- Paneldata(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, n=246, t=6, model="fe")
summary(fe.panel)

fe.panel <- Paneldata(MILK ~ LABOR + LAND + COWS + FEED, data = Milk, n=247, t=6, model="fe")
summary(fe.panel)

re.panel <- Paneldata(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, n=246, t=6, model="re")
summary(re.panel)


###########################################################################
############################# plm package #################################
###########################################################################

library(plm) 

inst.fixed.lm = lm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel)
summary(inst.fixed.lm)

#ONE in nlogit
CONST <- rep(as.numeric(inst.fixed.lm$coefficients["(Intercept)"]),nrow(Panel))
CONST <- rep(1,nrow(Panel))

#One-way fixed effects
inst.fixed = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Institution"), model = "within")
summary(inst.fixed)

year.fixed = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Year"), model = "within")
summary(year.fixed)

#farm.fixed = plm(MILK ~ LABOR + LAND + COWS + FEED, data = Milk, index = c("FARM"), model = "within")
#summary(farm.fixed)

#Two-way error
two.fixed = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Institution", "Year"),effect = "twoways", model = "within") 
summary(two.fixed)

#One-way random effects
inst.random = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Institution"), model = "random")
summary(inst.random)

year.random = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Year"), model = "random")
summary(year.random)

#Two-way error with random effects
two.random = plm(Publications ~ Researchers + Supervisors + Rcenters + Projects, data = Panel, index = c("Institution", "Year"),effect = "twoways", model = "random") 
summary(two.random)



#######statistical tests############

#Chow F-test
pFtest(inst.fixed, inst.fixed.lm )
pFtest(year.fixed, inst.fixed.lm )
pFtest(two.fixed, inst.fixed.lm )

##Lagrange Multiplier
plmtest(inst.random, type=c("bp"))
plmtest(year.random, type=c("bp"))
plmtest(two.random, type=c("bp"))

##Hausman test
phtest(inst.fixed, inst.random)
phtest(year.fixed, year.random)
phtest(two.fixed, two.random)

###################



phtest(random, fixed)
library(lavaan)
head(Demo.growth)

model = "i =~ 1*t1 + 1*t2 + 1*t3 + 1*t4
s =~ 0*t1 + 1*t2 + 2*t3 + 3*t4"

fit = growth(model, data = Demo.growth)
summary(fit)

################################################################