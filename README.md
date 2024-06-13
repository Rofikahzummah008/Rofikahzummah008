#SOAL3
library(readxl)
library(readr)
library(generalhoslem)
library(pscl)
library(performance)
library(car)
library(rcompanion)
getwd()
setwd("C:/Users/hp/Documents/Semester 6/Prak ANDATKAT")
Soal3 = read_excel("C:/Users/hp/Documents/Semester 6/Prak ANDATKAT/Data UAS Andat Kategorik 2024.xlsx")
Soal3
X1 = Soal3$`Weight in pounds`
X2 = Soal3$`Height  in inches`
X3 = Soal3$`Waist  in inches`
X4 = Soal3$`Hips  in inches`
X5 = Soal3$`Chest  in inches`
X6 = Soal3$`Hand in inches`
X7 = Soal3$`Shoe (US)`
Y = Soal3$Gender
hired = data.frame(X1, X2, X3, X4, X5,X6, X7, Y)
str(hired)
# Membuat Model
#man = 1
#Women = 0
logit = glm(Y~.,data = hired, family = binomial(link = "logit"))
summary(logit)
#Uji signifikansi model
pR2(logit)
qchisq(0.95,3)
#Uji Parsial
logit = glm(Y~., data = hired, family = binomial(link = "logit"))
summary(logit)
logit2 = glm(Y~X6+X7, data = hired, family = binomial(link = "logit"))
summary(logit2)
#Memilih Model Terbaik
model = c("Model 1", "Model 2")
AIC = c(logit$aic, logit2$aic)
kri = data.frame(model, AIC)
kri
#UJI nagelkerke
nagelkerke(logit2)
#tabel hasil klarifikasi
table(true = hired$Y, pred=round(fitted(logit2)))
#OR
beta = (coef(logit2))
OR_beta = exp (beta)
cbind(beta,OR_beta)

# Buat grid untuk variabel independen
x_grid <- seq(min(X6), max(X7), length.out = 100)
x_grid
# Buat plot
plot(Soal3$`Hand in inches`, Soal3$Gender, pch = 20, xlab = "Variabel Independen", ylab = "Variabel Dependen")
lines(x_grid, prob, col = "red", lwd = 2)
title("Plot Regresi Logistik")
legend("topleft", legend = c("Data", "Regresi Logistik"), pch = c(20, -1), lty = c(0, 1), col = c("black", "red"))

