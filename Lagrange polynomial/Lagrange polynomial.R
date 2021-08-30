# Данные: Зависимость катодных пятен от скорости наплавки 
myData <- read.csv("...\\dataset.csv", sep = ";", header = TRUE, dec = ",")
# Присвоенние данных переменным: y - Скорость сварки, м/час, х - ширина катодных пятен, м
z <- as.double(myData$Vsv)
y2 <- as.double(myData$Bo)
# Новые данные {a, b} с шагом 0.1
x <- seq(20, 44, 0.1)
# Аппроксимация Лагранжа - описание фунции - формула многочлена Лагранжа 
a.l <- function(x, y2, z){
  # Присвоение переменным m1, m2, m3 матрицы  
  m1 <- matrix(1,length(y2),length(x)) 
  m2 <- matrix(2,length(y2),length(x)) 
  m3 <- matrix(3,length(y2),length(y2))
  # создаем массив данных 
  m3p <- c(NULL)
  # Выполнение цикла заполнения матриц m1,m3,m3p значениями x - x1[i], y2 - y2[i], prod(m3[-i,i]
  for (i in 1:length(y2)){m1[i,] <- x - y2[i]}
  for (i in 1:length(y2)){m3[i,] <- y2 - y2[i]}
  for (i in 1:length(y2)){m3p[i] <- prod(m3[-i,i])}
  # для каждой строки m2[i] высчитываем prod(m1[-i,j])
  for (i in 1:length(y2)){for (j in 1:length(x)){m2[i,j] <- prod(m1[-i,j])}}
  #вычисляем y, путем суммирования 24 значений y1[i]*m2[i]/m3p[i]
  y <- apply(z*m2/m3p, 2, sum)
  y
  # Строим график  y(x)
  plot(x, y, type = "l", col = "black")
  # Добавляем заданные точки (y2, z) на график y(x)
  points(y2, z, col= 10, pch = 10)
}
a.l(x, y2, z)


