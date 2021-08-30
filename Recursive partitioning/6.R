library(rpart)
library(rpart.plot)
tree <- read.csv2("...\\dataset.csv")
# Обучающая выборка
x <- tree[1:15, 2]
y <- tree[1:15, 1]
# minbucket - минимальное количество наблюдений в любом терминальном  узле
# минимальное количество наблюдений, которое должно существовать в узле, чтобы можно было сделать попытку разделения.
#tree_test <- rpart(tree$Category ~ Id + Qp + Qz + Vsv + His + Bo, data = tree, minbucket = 1, minsplit =1)
#tree_test2 <- rpart(tree$Category ~ Id + Qp + Qz + Vsv + His + Bo, data = tree, minbucket = 2, minsplit = 2)
tree_test <- rpart(y~x, data = tree, minbucket = 1, minsplit =1)
tree_test
tree_test2 <- rpart(y~x, data = tree, minbucket = 2, minsplit = 2)
tree_test2
tree_test
tree_test2
rpart.plot(tree_test)
rpart.plot(tree_test2)
