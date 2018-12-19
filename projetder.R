#chargement des librairies
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)

#insertion de la base
insurance <- read.csv("d:/insurance.csv")

#voir le dataset
view(insurance)

# Decrire les statistiques
summary(insurance)

# Par region
describeBy(insurance$charges,insurance$region)
ggplot(data = insurance,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("frais par region")

# fumeurs
describeBy(insurance$charges,insurance$smoker)
ggplot(data = insurance,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("frais par statut de fumeur")

# sexe
describeBy(insurance$charges,insurance$sex)
ggplot(data = insurance,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("frais par sexe")

# par nombre d'enfant
describeBy(insurance$charges,insurance$children)
ggplot(data = insurance,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("frais par nombre d'enfants")


#creation de nouvelle variable
insurance$bmi30 <- ifelse(insurance$bmi>=30,"yes","no")

# par obésité
describeBy(insurance$charges,insurance$bmi30)
ggplot(data = insurance,aes(bmi30,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("frais par statut d'obésité ")

#analyse de corrélation
pairs.panels(insurance[c("age", "bmi", "children", "charges")])


#model_1
ins_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = insurance)
summary(ins_model)


# creation de variable age 
insurance$age2 <- insurance$age^2

#model_2
ins_model2 <- lm(charges ~ age  + children + bmi + smoker , data = insurance)
summary(ins_model2)

#model_3
ins_model3 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model3)

#model_4
ins_model4 <- lm(charges ~  age2 + bmi30 + smoker, data = insurance)
summary(ins_model4)

