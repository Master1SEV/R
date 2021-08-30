# Зависимость катодных пятен от скорости наплавки 
myData <- read.csv("...\\dataset.csv", sep = ";", header = TRUE, dec = ",")
# Присвоенние данных переменным: y - Скорость сварки, м/час, х - ширина катодных пятен, м
y <- as.double(myData$Vsv)
x <- as.double(myData$Bo)
# Построение графика: Зависимость катодных пятен от скорости наплавки.
plot(y, x, type = "p", xlab = "time", ylab = "close")
# Регрессионный анализ. Подбор кф регр модели. Степенная функция f(x)=a+b*x+c*x*x - полином 2-го порядка
func <- nls(y~a+b*x+c*x*x, start=list(a=0.1, b=0.1, c=0.1))
# значение коэффициента
coef_func <- coef(func)
coef_func
# Построение регрессионной модели
yfunc <- coef_func[1]+coef_func[2]*x+coef_func[3]*x*x
# строим график yfunc
lines(x, yfunc, type = "l", col="green")
# Проверка адекватности
# тест Фишера для 2-х векторов параметров
var.test(x[y], yfunc)
#Критерий Стьюдента
t.test(x[y], yfunc)