
#R 내장 데이터 가져오기
data(iris)

#iris 데이터 확인
str(iris)

#iris : 꽃받침, 꽃잎 데이터 추출
iris1 <- iris[,-5]
iris1
#기술통계량
 

#상관계수
cor(iris1, method="pearson")

#색의 농도로 상관계수 
install.packages("corrgram")
library(corrgram)

corrgram(iris1, upper.panel = panel.conf)

#상관계수 챠트
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

chart.Correlation(iris1)

#학습데이터와 테스트데이터 분리
x <- sample(1:nrow(iris1), 0.7 * nrow(iris1)) 
x

train <- iris[x, ]
test <- iris[-x, ]

nrow(train)
nrow(test)

#회귀모델 : 꽃받침 길이 예측 
names(iris1)

#학습
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data=train)
model
 
#예측
pred <- predict(model, test)
pred

#평가
#RMSE : sqrt((실제 - 예측)^2의 평균)




RMSE <-  sqrt(mean((test$Sepal.Length - pred) ^ 2))
RMSE


#분류모델 
library(ggplot2)
ggplot(iris, aes(Sepal.Length, Sepal.Width))  + 
  geom_point(aes(colour = Species))

ggplot(iris, aes(Petal.Length , Petal.Width))  + 
  geom_point(aes(colour = Species))

#학습데이터와 테스트데이터 나누기
x <- sample(1:nrow(iris1), 0.7 * nrow(iris1)) 
train <- iris1[x, ]
test <- iris1[-x, ]

#모델 학습
#트리 모델 
install.packages("party")
library(party)
names(iris)

model1 <- lm(formula =  Sepal.Length  ~ Sepal.Width+ Petal.Length + Petal.Width
             , data = train)
model2 <- lm(formula =  Sepal.Length  ~ Petal.Length + Petal.Width,
             data = train)


model3 <- lm(formula =  Sepal.Length ~ Sepal.Width + Petal.Length,
             data = train)
model4 <- lm(formula =  Sepal.Length ~ Sepal.Width + Petal.Width,
             data = train)

plot(model)

#예측 
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)
pred4 <- predict(model4, test)

#평가
RMSE1 <-  sqrt(mean((test$Sepal.Length - pred1) ^ 2))
RMSE2 <-  sqrt(mean((test$Sepal.Length - pred2) ^ 2))
RMSE3 <-  sqrt(mean((test$Sepal.Length - pred3) ^ 2))
RMSE4 <-  sqrt(mean((test$Sepal.Length - pred4) ^ 2))
cat("예측1: ", RMSE1)
cat("예측2: ", RMSE2)
cat("예측3: ", RMSE3)
cat("예측4: ", RMSE4)

#혼돈행렬
t<- table(test$Species,pred)
 
acc <- (t[1,1]+t[2,2]+t[3,3]) / sum(t)
acc


#iris data 저장

#트리 모델 
install.packages("party")
library(party)
data(iris)
names(iris)
model1 <- ctree(formula =  Sepal.Length  ~ Sepal.Width+ Petal.Length + Petal.Width
             , data = train)
model2 <- ctree(formula =  Sepal.Length  ~ Petal.Length + Petal.Width,
             data = train)


model3 <- ctree(formula =  Sepal.Length ~ Sepal.Width + Petal.Length,
             data = train)
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)

#혼돈행렬

t1 <- table(test$Species,pred1)
t2 <- table(test$Species,pred2)
t3 <- table(test$Species,pred3)

acc1 <- (t1[1,1]+t1[2,2]+t1[3,3]) / sum(t1)
acc2 <- (t2[1,1]+t2[2,2]+t2[3,3]) / sum(t2)
acc3 <- (t3[1,1]+t3[2,2]+t3[3,3]) / sum(t3)

acc1
acc2
acc3

plot(model1)
plot(model2)
plot(model3)
