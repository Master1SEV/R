library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(urca)
library(ggcorrplot)
library(forecast)
library(xts)
library(caret)
library(corrgram)
library(corrplot)
library(RColorBrewer)
library(tidyverse)
library(MASS)
library(rpart)
library(ggplot2)
library(corrr)
library(broom)
library(lifecycle)
library(mctest)
library(car)
library(gvlma)
library(leaps)
library(openxlsx)
library(BAS)
library(sandwich)
library(modelsummary)
library(tibble)
library(nortest)
library(recipe)
library(vegan)

### DR РБ Макропоправка для розничного бизнеса --------
set.seed(101)
pd <- read_excel("P:\\Документы подразделений\\Служба управления рисками\\Задачи отдела\\1Сунцов Е. В\\DR-RB-Q2014-2022 — копия — копия.xlsx", 
                 sheet = "data_DR_RB")
mydata <- na.omit(tab)
nume <- ncol(pd) 
tab = pd[2:nume]
corr_tab = cor(tab[1:nume - 1], use = "complete.obs")
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
par(mfrow = c(1, 1))
corrplot(corr_tab, method="color", col=col(200),  
         type="lower", 
         # order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", 
         tl.srt=.01, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",  # hide correlation coefficient on the principal diagonal
         diag=TRUE , 
         number.cex = 0.6,
         tl.cex = 0.6)

full <- lm(DR ~ Avg_unemp + Avg_inf, data = tab)  # k = 0.8036116, aR2 = 0.6 ------ в отчет
summary(full)

testd <- read_excel("P:\\Документы подразделений\\Служба управления рисками\\Задачи отдела\\1Сунцов Е. В\\DR-RB-Q2014-2022 — копия — копия.xlsx", 
                    sheet = "test_data_DR_RB")
numet <- nrow(testd) 
numet <- numet+3
numet

modt <-
  summary(full)$coefficients[2, 1] * testd$Avg_unemp +
  summary(full)$coefficients[3, 1] * testd$Avg_inf +
  summary(full)$coefficients[1, 1]
modt
# Расчет макропоправки ---------------
n <- c(1:numet)
m <- data.frame(modt[n + 1] / modt[n])
m

mk1 <- 0.01655130905808 #09 2022 fact
mk3 <- 0.02005798 # 01 2023 model
mk <- mk3/mk1
mk 

# Тесты модели -----
# Гетероскедастичность "неодинаковый разброс"
# Тест на непостоянную дисперсию - тест Уайта
#install.packages('skedastic', type="binary")
# library(skedastic)
skedastic::white_lm(full) # 0,05 меньше 0,05 в регрессионой модели отсутсвует гетероскедастичность +


#install.packages('trafo')
# library(trafo)
# assumptions(full, method = "ml", plotit = TRUE)
# xt <- logshiftopt(object = full, plotit = F)

# Независимость ошибок - тест Дарбина-Уотсона
durbinWatsonTest((full)) # автокорреляция отсутствует при ро = 0 + 

# тест Дикки-Фуллера рассматривает гипотезу о нестационарности ряда. Тест ADF является односторонним: в
#качестве альтернативной гипотезы по умолчанию считается гипотеза о стационарности ряда.
library(tseries)

adf.test(tab$DR)#p-value = 0.5981>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен
adf.test(tab$Avg_unemp)#p-value = 0.3203>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен
adf.test(tab$Avg_inf)#p-value = 0.6688>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен

library(GGally)

vif(full) # мультиколлерность отсутсвтует +

# тест KPSS в первых разностях
library(urca)
kpss.test(tab$DR) # p-value = 0.08 > 0.05 ряд нестационарен
plot(d[1:35], tab$DR, type='l') 
kpss.test(tab$Avg_unemp) # p-value = 0.1 > 0.05 ряд нестационарен
plot(d[1:35], tab$Avg_unemp, type='l') 
kpss.test(pd$Avg_inf) # p-value = 0.1 > 0.05 ряд нестационарен
plot(d[1:35], tab$Avg_inf, type='l') 

kpss.test(tab$DR, null = "Trend")
kpss.test(tab$Avg_unemp, null = "Trend")
kpss.test(tab$Avg_inf, null = "Trend")
# summary(ur.kpss(pd$DR)) # 0.3842 < 0.739  h0 ряд нестационарен

# Тест Йохансена
# Коинтеграция
set.seed(101)
# pd
ff = ts(pd[c(2, 3, 6)],
        frequency = 4,
        start = c(2014, 1),
        end = c(2022,3))
ff
ff = ts(pd[c(3, 6)],
        frequency = 4,
        start = c(2014, 1),
        end = c(2022,3))
ff

jotest2=ca.jo(ff)
# summary(jo1)
# summary(jotest)
summary(jotest2) 

# install.packages('aTSA')
# library(aTSA)
# coint.test(tab$DR, tab$Avg_unemp)

### График модели -----
d = as.Date(pd$Q, format="%Y-%m-%d")
par(mfrow=c(1,1)) 
plot(
  d[1:35],
  pd$`DR`[1:35],
  xlab = "Дата",
  ylab = "DR",
  main = "Линейная модель для розничного бизнеса",
  type = "l",
  xlim = as.Date(c("2014-03-01", "2023-03-31")),
  ylim = c(0.015, 0.06)
)

dt = as.Date(testd$Q, format = "%Y-%m-%d")
lines(dt[1:35], modt[1:35], type = "l", col = "red")
lines(dt[35:40], modt[35:40], type = "l", col = "dodgerblue")






# DB KB Макропоправка для корпоративного бизнеса --------
set.seed(101)
pd <- read_excel("P:\\Документы подразделений\\Служба управления рисками\\Задачи отдела\\1Сунцов Е. В\\DR_NEW_Q2014-2022.xlsx",
                 sheet = "KB")
nume <- ncol(pd)
corr_tab = cor(pd[2:nume])
tab = pd[2:nume]
corr_tab = cor(tab[1:nume - 1], use = "complete.obs", method = "pearson")
corr_tab = cor(tab[1:nume - 1], use = "complete.obs", method = "kendall")
corr_tab = cor(tab[1:nume - 1], use = "complete.obs", method = "spearman")
par(mfrow = c(1, 1))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(corr_tab, method="color", col=col(200),  
         type="lower", 
         # order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=.01, #Text label color and rotation
         # Combine with significance
         sig.level = 0.01, insig = "blank",  # hide correlation coefficient on the principal diagonal
         diag=TRUE , 
         number.cex = 0.65,
         tl.cex = 0.8)

# full <- lm(DR ~ Avg_unemp + Avg_inf + GDP_IV_YOY, data = tab) # +
# summary(full)
full <- lm(DR ~ Avg_unemp + Avg_inf, data = tab)  
summary(full)

testd <- read_excel("P:\\Документы подразделений\\Служба управления рисками\\Задачи отдела\\1Сунцов Е. В\\DR_NEW_Q2014-2022.xlsx",
                    sheet = "test_KB")
# modt <-
#   summary(full)$coefficients[2, 1] * testd$Avg_unemp +
#   summary(full)$coefficients[3, 1] * testd$Avg_inf +
#   summary(full)$coefficients[4, 1] * testd$GDP_IV_YOY +
#   summary(full)$coefficients[1, 1]
# modt

modt <-
  summary(full)$coefficients[2, 1] * testd$Avg_unemp +
  summary(full)$coefficients[3, 1] * testd$Avg_inf +
  summary(full)$coefficients[1, 1]
modt

# Расчет макропоправки -----
n <- c(1:40)
m <- data.frame(modt[n + 1] / modt[n])
m


mk1 <- 0.01 #09 2022 fact
mk3 <- 0.01805495 # 01 2023 model
mk <- mk3/mk1
mk

# Тесты модели -----
# Гетероскедастичность "неодинаковый разброс"
# Тест на непостоянную дисперсию - тест Уайта
#install.packages('skedastic', type="binary")
# library(skedastic)
skedastic::white_lm(full) # 0,05 меньше 0,05 в регрессионой модели отсутсвует гетероскедастичность +


#install.packages('trafo')
# library(trafo)
# assumptions(full, method = "ml", plotit = TRUE)
# xt <- logshiftopt(object = full, plotit = F)

# Независимость ошибок - тест Дарбина-Уотсона
durbinWatsonTest((full)) # автокорреляция отсутствует при ро = 0 + 

# тест Дикки-Фуллера рассматривает гипотезу о нестационарности ряда. Тест ADF является односторонним: в
#качестве альтернативной гипотезы по умолчанию считается гипотеза о стационарности ряда.
library(tseries)

adf.test(tab$DR)#p-value = 0.5981>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен
adf.test(tab$Avg_unemp)#p-value = 0.3203>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен
adf.test(tab$Avg_inf)#p-value = 0.6688>0.05 Стационарность временного ряда в первых разностях не доказанна. временной рял нестационарен

library(GGally)

vif(full) # мультиколлерность отсутсвтует +

# тест KPSS в первых разностях
library(urca)
kpss.test(tab$DR) # p-value = 0.06 > 0.05 ряд нестационарен
plot(d[1:35], tab$DR, type='l') 
kpss.test(tab$Avg_unemp) # p-value = 0.1 > 0.05 ряд нестационарен
plot(d[1:35], tab$Avg_unemp, type='l') 
kpss.test(pd$Avg_inf) # p-value = 0.1 > 0.05 ряд нестационарен
plot(d[1:35], tab$Avg_inf, type='l') 

kpss.test(tab$DR, null = "Trend")
kpss.test(tab$Avg_unemp, null = "Trend")
kpss.test(tab$Avg_inf, null = "Trend")
# summary(ur.kpss(pd$DR)) # 0.3842 < 0.739  h0 ряд нестационарен

# Тест Йохансена
# Коинтеграция
set.seed(101)
# pd
ff = ts(pd[c(2, 3, 6)],
        frequency = 4,
        start = c(2014, 1),
        end = c(2022,3))
ff
ff = ts(pd[c(3, 6)],
        frequency = 4,
        start = c(2014, 1),
        end = c(2022,3))
ff

jotest2=ca.jo(ff)
# summary(jo1)
# summary(jotest)
summary(jotest2) 

### График модели -----
d = as.Date(pd$Q, format="%Y-%m-%d")
par(mfrow=c(1,1)) 
plot(
  d[1:35],
  pd$`DR`[1:35],
  xlab = "Дата",
  ylab = "DR",
  main = "Линейная модель для корпоративного бизнеса",
  type = "l",
  xlim = as.Date(c("2014-03-01", "2023-03-31")),
  ylim = c(0.0, 0.07)
)

dt = as.Date(testd$Q, format = "%Y-%m-%d")

lines(dt[1:35], modt[1:35], type = "l", col = "red")
lines(dt[35:40], modt[35:40], type = "l", col = "dodgerblue")
