# Установка необходимых пакетов
library(caretEnsemble)
library(caret)
# Импорт файла с тренировочными данными и тестовыми
trainData <- read.csv("J:\\Новая папка\\DATA MINING CUP\\train.csv",sep="|",stringsAsFactors = FALSE)
testData <- read.csv("J:\\Новая папка\\DATA MINING CUP\\test.csv", sep="|",stringsAsFactors = FALSE)
set.seed(777)
# Конвертирование столбца fraud в формат фактора
trainData$fraud <- as.factor(trainData$fraud)
#levels(trainData$fraud) <- c("0","1")
levels(trainData$fraud) <- c("trusty","fraud")

### Анасмбль моделей
## Алгоритмы суммирования - запуск нескольких алгоритмов за один вызов
# Число повторений 3 и 
#деление набора тренировочных данных случайным образом на 10 частей, 
#а затем использование каждой из 10 частей в качестве набора тестовых 
#данных для модели, подготовленной на других 9 
trainControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# Модели: Рандомный лес, Бустинг, Метод опорных векторов
algorithmList <- c('rf','xgbDART', 'svmRadial')
set.seed(700)
models <- caretList(fraud~., data=trainData, trControl=trainControl, methodList=algorithmList) 

results <- resamples(models)
summary(results)
save(models, file="models_final.RData")

# Объединение прогнозов моделей
# для формирования окончательного прогноза
set.seed(777)
stackControl <- trainControl(method="repeatedcv", 
                             number=10, 
                             repeats=3,
                             savePredictions=TRUE, 
                             classProbs=TRUE)

# Ансамбль предсказаний моделей, 
# чтобы сформировать новый комбинированный прогноз на основе glm (Обобщенной линейной модели)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
save(stack.glm,file="combined_predictions.RData")
print(stack.glm)
# Прогноз testData
stack_predicteds <- predict(stack.glm, newdata=testData)
head(stack_predicteds)
write.csv(stack_predicteds,file="predictions.csv")
save.image("script.RData")
levels(stack_predicteds) <- c("0","1")
testData$fraud <- stack_predicteds
# Сохранение результатов в testData_predictions.csv
write.table(testData, file="testData_predictions.csv",row.names=FALSE,sep="|")

