library(tidyverse)
library(Epi)
library(hnp)
library(car)

df <- read.table('coverageX.txt', header = T)

set.seed(10844226)

baseprincipal <- sample_n(df, 2000)

#write.csv(baseprincipal, "baseprincipal.csv")

set.seed(10844226)
smp_size <- floor(0.75 * nrow(baseprincipal))
train_ind <- sample(seq_len(nrow(baseprincipal)), size = smp_size)

train <- baseprincipal[train_ind, ]
test <- baseprincipal[-train_ind, ]

#write.csv(test, "baseteste.csv")
#write.csv(train, "basetreino.csv")

summary(df)

mod <- glm(formula = y ~ MEN + URBAN + PRIVATE + AGE + SENIORITY, 
           family = binomial(link = 'logit'), data = train)

summary(mod)

(IC1 <- confint.default(mod, level=0.95))

anova(mod,test = 'Chisq')

ROC(mod$fitted.values, train$y, plot= "ROC")

hnp.mod = hnp(mod, print.on=TRUE, plot=FALSE,halfnormal=F)

plot(hnp.mod,main="Modelo Logito",las=1,pch=20,cex=1,col=c(1,1,1,2))

#Começando com diferentes ligações
#Probito
modp <- glm(formula = y ~ MEN + URBAN + PRIVATE + AGE + SENIORITY, 
           family = binomial(link = 'probit'), data = train)

summary(modp)

anova(modp,test = 'Chisq')

#Cauchito
modc <- glm(formula = y ~ MEN + URBAN + PRIVATE + AGE + SENIORITY, 
            family = binomial(link = 'cauchit'), data = train)

summary(modc)

anova(modc,test = 'Chisq')

#cloglog
modcl <- glm(formula = y ~ MEN + URBAN + PRIVATE + AGE + SENIORITY, 
            family = binomial(link = 'cloglog'), data = train)

summary(modcl)

anova(modcl,test = 'Chisq')

#loglog

loglog <- function( ) structure(list(
  linkfun = function(mu) -log(-log(mu)),
  linkinv = function(eta)
    pmax(pmin(exp(-exp(-eta)), 1 - .Machine$double.eps),
         .Machine$double.eps),
  mu.eta = function(eta) {
    eta <- pmin(eta, 700)
    pmax(exp(-eta - exp(-eta)), .Machine$double.eps)
  },
  dmu.deta = function(eta)
    pmax(exp(-exp(-eta) - eta) * expm1(-eta),
         .Machine$double.eps),
  valideta = function(eta) TRUE,
  name = "loglog"
), class = "link-glm")

modl <- glm(formula = y ~ MEN + URBAN + PRIVATE + AGE + SENIORITY, 
             family = binomial(link = loglog()), data = train)

summary(modl)

anova(modl,test = 'Chisq')

#Escolhendo o melhor modelo

data.frame(Modelo=c("Modelo logito","Modelo probito","Modelo cauchito","Modelo cloglog","Modelo loglog"),
           AIC = c(AIC(mod),AIC(modp),AIC(modc),
                   AIC(modcl), AIC(modl)))

#modelo escolhido foi o loglog
stepAIC(modl)

modlr <- glm(formula = y ~ MEN + URBAN + AGE + SENIORITY, 
             family = binomial(link = loglog()), data = train)

summary(modlr)

influenceIndexPlot(modlr,col='blue')

influencePlot(modlr)

ajuste1<-glm(y ~ MEN + URBAN + AGE + SENIORITY,
             subset = -c(124),
             family = binomial(link=loglog()),
             data=train)

ajuste2<-glm(y ~ MEN + URBAN + AGE + SENIORITY,
             subset = -c(135),
             family = binomial(link=loglog()),
             data=train)

ajuste3<-glm(y ~ MEN + URBAN + AGE + SENIORITY,
             subset = -c(124,135),
             family = binomial(link=loglog()),
             data=train)

compareCoefs(modlr,ajuste1, ajuste2, ajuste3)

data.frame(
  Modelo= c("Completo", "Removendo 124", "Removendo 135", "Removendo 124 e 135"),
  AIC = c(AIC(modlr),AIC(ajuste1), AIC(ajuste2), AIC(ajuste3)))

ROC(modlr$fitted.values, train$y, plot= "ROC")

library(hnp)
hnp.fit.modell = hnp(modlr, print.on=TRUE, plot=FALSE,
                     halfnormal=F)
## Binomial model
plot(hnp.fit.modell,main="Modelo Logito",las=1,pch=20,cex=1,col=c(1,1,
                                                                  1,2))
### acuracia do modelo
set.seed(10844226)
library(caret)
library(e1071)

test<- as.factor(test)

pred<-predict(modlr, type='response', newdata=test)

confusionMatrix(as.factor(as.numeric(pred>0.5)),as.factor(test$y))

pred1 <- predict(mod, type='response', newdata=test)

confusionMatrix(as.factor(as.numeric(pred1>0.5)),as.factor(test$y))
