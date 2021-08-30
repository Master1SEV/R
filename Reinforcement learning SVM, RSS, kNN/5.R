library(class)
library(gmodels)
library(kernlab)
library(rpart)
library(dplyr) 
library(ggplot2) 
library(lattice) 
library(caret) 

# 1.	Алгоритм k ближайших соседей для решения задачи регрессии
myData <- read.csv2("...\\5dataset.csv")
set.seed(777)
data_kNN <- data.frame(x = myData[1:4, 2], y = myData[1:4, 1]) 
data_test_kNN <- data.frame(x = myData[1:14, 2], y = myData[1:14, 1]) 
model_kNN <- train(y~x, data = data_test_kNN, method = "knn") 
model_kNN
y.pred.kNN <- predict(model_kNN, newdata = data_test_kNN)
y.pred.kNN
################################################
# 2.	Метод наименьших квадратов для решения задачи регрессии
# RSS
set.seed(777)
RSS <- data.frame(x = myData[1:4, 2], y = myData[1:4, 1]) 
res <- nls(y~b*x+a, data = RSS, start=list(a=0.1, b=0.1), control=nls.control(maxiter=10000)) 
coef_func <- coef(res) 
model_RSS <- coef_func[2]*myData[1:14, 2]+coef_func[1]
model_RSS
################################################
# 3.	Метод опорных векторов / Support Vector Machine (SVM)
set.seed(777)
# Обучающая выборка
myData_train <- myData[1:4, 1:2]
# Тестовая выборка 
myData_test <- myData[1:14, 1:2]
svmy <- ksvm(Vsv~Bo, myData_test,epsilon = 0.5, C = 5)
svmy
# Прогноз y = Vsv
svm_py <- predict(svmy, myData_test)
svm_py <- as.double(svm_py)
svm_py
# Построение графика
plot(myData[1:9, 2], myData[1:9, 1], col = "blue", lwd = 2,
     xlab = "Bo - ширина катодных пятен, м", 
     ylab = "Vsv- Скорость сварки, м/час", 
     main = paste("Методы"),
     xlim=c(20, 45),
     ylim=c(-10, 80)
)
# 
points(myData[10:14, 2], myData[10:14, 1], type = "p", lwd = 2)
location = "topright"
labels = c("МНК", "kNN", "SVM")
colors = c("red", "orange", "green")
legend(location, labels, fill=colors)
# kNN
points(myData[1:14, 2], y.pred.kNN, type = "l", col = "orange", lwd = 2)
# SVM
points(myData[1:14, 2], svm_py, type = "l", lwd = 2, col = "green")
# RSS
points(myData[1:14, 2], model_RSS, type = "l", col = "red", lwd = 2)


