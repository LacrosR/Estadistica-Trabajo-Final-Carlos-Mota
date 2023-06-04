

dat <- read_excel(path = "Base de datos Pileta_modif.xlsx")

dat$Figuracion <- as.factor(dat$Figuracion)
dat$Color <- as.factor(dat$Color)
dat$Estilo <- as.factor(dat$Estilo)



table(dat$Figuracion, dat$Color) 


dat$Amarillo <- ifelse(dat$Color=="Amarillo", 1,0)
dat$Rojo <- ifelse(dat$Color=="Rojo", 1,0)
dat$Negro <- ifelse(dat$Color=="Negro", 1,0)
dat$Simbolo <- ifelse(dat$Figuracion=="Sim", 1,0)
dat$Zoomorfo <- ifelse(dat$Figuracion=="Zoo", 1,0)
dat$Amarillo


summary(glm(formula = Figuracion ~ Amarillo, data = dat, family = "binomial")) 
summary(glm(formula = Figuracion ~ Rojo, data = dat, family = "binomial"))
summary(glm(formula = Figuracion ~ Negro, data = dat, family = "binomial"))

summary(glm(formula = Zoomorfo ~ Amarillo, data = dat, family = "binomial"))
summary(glm(formula = Simbolo ~ Amarillo, data = dat, family = "binomial"))


exp(summary(glm(formula = Figuracion ~ Amarillo, data = dat, family = "binomial"))$coefficients[,1])
exp(summary(glm(formula = Figuracion ~ Rojo, data = dat, family = "binomial"))$coefficients[,1]) 

exp(coefficients(modeloA))
exp(coefficients(modeloR))

summary(glm(formula = relevel(Figuracion, ref = "Sim") ~ Amarillo, data = dat, family = "binomial")) 



dat2 <- dat[dat$Estilo!=5,] 
dat2 <- dat[dat$Figuracion!= "Sim",]
dat3 <- dat[dat$Figuracion!= "Sim",]

table(dat2$Estilo, dat2$Color)

ggplot(data = dat, aes(x=Figuracion, fill=Color)) + geom_bar(position = "dodge") + theme_bw() + 
  scale_fill_manual(values = c("Yellow", "Black", "Red")) + labs( x= "Figuración", y="Número de Figuras", title = "Número de figuras según figuración y color en Pileta")

ggplot(data = dat, aes(x=Estilo, fill=Color)) + geom_bar(position = "dodge") + theme_bw() + 
  scale_fill_manual(values = c("Yellow", "Black", "Red")) + labs( x= "Estilo", y="Número de Figuras", title = "Número de figuras según estilo y color en Pileta")



summary(lm(formula = Estilo ~ Amarillo, data=dat2)) 
summary(lm(formula = Estilo ~ Negro, data=dat2)) 
summary(lm(formula = Estilo ~ Rojo, data=dat2))


exp(summary(lm(formula = Estilo ~ Amarillo, data=dat2))$coefficients[,1]) 

ModeloR2 <- summary(clm(formula = Estilo ~ Rojo, data=dat))
exp(coefficients(ModeloR2))

modeloN2 <- summary(clm(formula = Estilo ~ Negro, data=dat))
exp(coefficients(modeloN2))

summary(clm(formula = Estilo ~ Zoomorfo, data=dat))

install.packages("MASS")
library(MASS)
library(readxl)
library(ggplot2)

summary(polr(Estilo ~ Simbolo, data = dat))
summary(polr(Estilo ~ Zoomorfo, data = dat))

summa

library(latticeExtra)
library(tinytex)

library (ordinal)

library(ggplot2)
install.packages("rlang")
