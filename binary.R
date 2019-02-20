## Set directory
setwd("E:/ADI BUANA/Analisis Regresi/Notes/Log Regression")

## Import dataset

logistik <- read.csv("binary log.csv")
attach(logistik)
head(logistik)

## Summarry 
summary(logistik)

## Build the logistic regression model

model <- glm(WORKSTAT ~ CHILDREN + RACE + CONTROL +  ATTMAR + ATTROLE +
             SEL + ATTHOUSE + AGE + EDUC,
             data= logistik, family = binomial(link = "logit"))

summary(model)

## Overral model
options(scripen = 999)
chidiff = model$null.deviance - model$deviance
chidiff
dfdiff = model$df.null - model$df.residual
dfdiff

pchisq(chidiff, dfdiff, lower.tail = FALSE)
   # (1.13018e-08) overall model signifikan

library(BaylorEdPsych) ## Please intall this package untuk mencari nilai Nagelkerke
PseudoR2(model)

## Look at correct percentage

correct <- model$fitted.values
binarycorect <- ifelse( correct > 0.5, 1, 0)
binarycorect <- factor(binarycorect,
                       levels = c(0,1),
                       labels = c("Not working Pred", "working Pred"))
tab1 <- table(logistik$WORKSTAT, binarycorect)

benar_klasifikasi <- sum(diag(tab1))/sum(tab1)
benar_klasifikasi

salah <- 1-benar_klasifikasi
salah

## ODDS RATIO
exp(coef(model))

library(ggplot2)

theme <- theme(panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               panel.background = element_blank(),
               axis.text.y = element_blank(),
               axis.ticks = element_blank(),
               axis.line.x = element_line(color = "black"),
               axis.line.y = element_line(color = "black"),
               text = element_text(size = 20),
               legend.key = element_blank())

hist <- ggplot(logistik,
               aes(correct, color = WORKSTAT, fill = WORKSTAT))
hist +
  theme +
  geom_dotplot(binwidth = 0.01, position = "jitter") +
  coord_cartesian(xlim = c(0,1)) +
  xlab("All Predictor in model") +
  ylab("Frequency") +
  scale_color_manual(values = c("Maroon", "#2C3539"),
                     labels = c("Not Working", "Working"),
                     name = "Working Category")+
  scale_fill_manual(values = c("Maroon","#2C3539" ),
                    labels = c("Not Working", "Working"),
                    name = "Working Category")+
  geom_vline(xintercept = c(0.50),linetype = "dotdash", size = 1)


table(model$data$WORKSTAT)
