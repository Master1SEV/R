d <- read.csv("...\\dataset.csv", sep = ";", header = TRUE, dec = ",")
# Присвоенние данных переменным: y - Скорость сварки, м/час, х - ширина катодных пятен, м
x <- as.double(d$Bo)
y <- as.double(d$Vsv)
e <- data.frame(x, y)
n <- 15
z <- length(y) 
w <- length(x)
x1 <- (1/z)*sum(y) 
x2 <- (1/w)*sum(x)
# Математическое ожидание
t <- sum((y-x1)*(x-x2)) 
# Среднеквадратичное отклонение x и y 
u <- sqrt(sum((y-x1)^2)*sum((x-x2)^2)) 
# Корреляционный момент
cow <- t/u
cow
b <- -0.25
a <- 38.6
Yx <- b*x+a
Xy <- b*y+a
cor(x,y)
# коэффициент
coeff <- corr$estimate 
# p-value
pvalue <- corr$p.value
# линейный коэффициент корреляции Пирсона
# коэффициент ранговой корреляции Спирмена
# коэффициент ранговой корреляции Кендалла 
Pe <- cor(x, y, method = "pearson")
Sp <- cor(x, y, method = "spearman")
Ke <- cor(x, y, method = "kendall")
tab_1 <- data.frame(x,y,Pe,Sp,Ke)
tab_2 <- data.frame(Pe,Sp,Ke)
hist(x)
hist(y)
x1 <- c(1:12)
tab_3 <- data.frame(x1,x)
cor(x)
cor(y)
boxplot(x)
pairs(tab_2)
ce<-cor(e)
pairs(ce, type ="l")
xs <- -1
GrX <- c(-1, 2)
GrY <- c(-1, -1)
tab_3 <- data.frame(GrX,GrY) 
# Графики
# Корреляционног момента
# линейного коэффициента корреляции Пирсона
# коэффициента ранговой корреляции Спирмена
# коэффициента ранговой корреляции Кендалла 
plot(cow)
points(Pe, type = "p", col = "red")
points(Sp, type = "p", col = "green")
points(Ke, type = "p", col = "orange")
# Коэффициент корреляции (cow) характеризует величину отражающую степень взаимосвязи двух переменных между собой. Он может варьировать в пределах от -1 (отрицательная корреляция) до +1 (положительная корреляция).
# Предел корреляции -1
points(GrX, GrY, type = "l")


# Аппроксимирующая прямая
plot(x, y, type = "p", col = 3, xlab = "x", ylab = "y", main = paste("Корреляция"))
regress=lm(formula = y ~ x)
abline(regress,col="orange",lwd="3",add=TRUE)

################################################################################################
d2 <- read.csv("C:\\Users\\Evgenii\\Documents\\SMIA\\1\\1del.csv", sep = ";", header = TRUE, dec = ",")
# Присвоенние данных переменным: y - Скорость сварки, м/час, х - ширина катодных пятен, м
x2 <- as.double(d$Bo)
# y2 <- c(35:48, 1)
y2 <- sample(35:48, 15, replace=T)
e2 <- data.frame(x2, y2)
n <- 15
z2 <- length(y) 
w2 <- length(x)
x12 <- (1/z)*sum(y2) 
x22 <- (1/w)*sum(x2)
# Математическое ожидание
t2 <- sum((y2-x12)*(x2-x22)) 
# Среднеквадратичное отклонение x и y 
u2 <- sqrt(sum((y2-x12)^2)*sum((x2-x22)^2)) 
# Корреляционный момент
cow2 <- t2/u2
cow2
e3 <- data.frame(cow, cow2)
# Аппроксимирующая прямая
points(x2, y2, type = "p", xlab = "x", ylab = "y", main = paste("Корреляция 2"))
regress2=lm(formula = y2 ~ x2)
abline(regress2,col="red",lwd="3",add=TRUE)

b <- -0.25
a <- 38.6
Yx <- b*x+a
Xy <- b*y+a
cor(x,y)
# коэффициент
coeff <- corr$estimate 
# p-value
pvalue <- corr$p.value
# линейный коэффициент корреляции Пирсона
# коэффициент ранговой корреляции Спирмена
# коэффициент ранговой корреляции Кендалла 
Pe2 <- cor(x2, y2, method = "pearson")
Pe2
Sp2 <- cor(x2, y2, method = "spearman")
Sp2
Ke2 <- cor(x2, y2, method = "kendall")
Ke2
tab_4 <- data.frame(x,y,Pe2,Sp2,Ke2)
tab_5 <- data.frame(Pe2,Sp2,Ke2)
tab_6 <- data.frame(Pe,Sp,Ke,Pe2,Sp2,Ke2)
hist(x)
hist(y)
x1 <- c(1:12)
tab_3 <- data.frame(x1,x)
cor(x)
cor(y)
boxplot(x)
pairs(tab_2)
ce<-cor(e)
pairs(ce, type ="l")
xs <- -1

GrX <- c(-0.2, 0.2)
GrY <- c(-0.2, -0.2)
tab_3 <- data.frame(GrX,GrY) 
# Графики
# Корреляционног момента
# линейного коэффициента корреляции Пирсона
# коэффициента ранговой корреляции Спирмена
# коэффициента ранговой корреляции Кендалла 
cow
plot(cow)
points(Pe2, type = "p", col = "red")
points(Sp2, type = "p", col = "green")
points(Ke2, type = "p", col = "orange")
# Коэффициент корреляции (cow) характеризует величину отражающую степень взаимосвязи двух переменных между собой. Он может варьировать в пределах от -1 (отрицательная корреляция) до +1 (положительная корреляция).
# Предел корреляции -1
points(GrX, GrY, type = "l")




# normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

# Евклидово расстояние
EUC <- (x-y)^2
EUC <- sum(EUC)
EUC <- sqrt(EUC)

# график зависимости квантилей y от квантилей x
qqplot(x, y)

# ?? - корреляция Кендалла
Rx <- c(1:15, 1)
Ry <- c(15:11, 1)
# число совпадений
#P <- 
# Число инверсий
#Q <- 
#data.frame(x,y,Rx,Ry,P,Q)

# Диаграмма рассеяния/Поле корреляции/
plot (x, y, type = "p")
# Линия тренда
points(x, Yx, type = "l", col = "green")

plot (x, y, type = "l", col = "red")
points(x, Yx, type = "l", col = "green")

points(x, Xy, type = "l", col = "red")
Tkrit <- 2.16
Di <- (cow-Tkrit*(sqrt((1-cow^2)/(n-2))))
Di
Di2 <- (cow+Tkrit*(sqrt((1-cow^2)/(n-2))))
Di2

# Диаграмма рассеяния 
plot (x, y, type = "p")

# Линейный коэф. корреляции 
# -0,93 < 0 => негативная корреляция

