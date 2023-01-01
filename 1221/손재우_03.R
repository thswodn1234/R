# 국민 건강보험 대사 증후군
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1221")
df <- read.csv("06_국민건강보험공단500.csv", header = TRUE)
names(df)
df1 <- df[,c("신장.5Cm단위." ,"체중.5Kg.단위.") ]
names(df1) <- c("키", "몸무게")
df1$BMI <- df1$몸무게 / (df1$키/100) ** 2

df1$비만도 <- ifelse(df1$BMI >= 30, "비만",
                     ifelse(df1$BMI >= 25 & df1$BMI < 30 , "과체중",
                            ifelse(df1$BMI >= 20 & df1$BMI < 25 , "정상","저체중")))
df1



#모델 학습
#트리 모델 
install.packages("party")
library(party)

x <- sample(1:nrow(df1), 0.7 * nrow(df1)) 
x


train <- df1[x, ]
test <- df1[-x, ]

model1 <- ctree(formula = 키 ~  몸무게 + BMI , data=train)
model2 <- ctree(formula = 몸무게 ~  키 + BMI , data=train)
model3 <- ctree(formula = BMI ~  키 + 몸무게 , data=train)


#예측 
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)
pred3 <- predict(model3, test)

#평가
RMSE1 <-  sqrt(mean((test$키 - pred1) ^ 2))
RMSE2 <-  sqrt(mean((test$몸무게 - pred2) ^ 2))
RMSE3 <-  sqrt(mean((test$BMI - pred3) ^ 2))


#혼돈행렬
t1<- table(test$비만도,pred1)
t2<- table(test$비만도,pred2)
t3<- table(test$비만도,pred3)

acc1 <- (t1[1,1]+t1[2,2]) / sum(t1)
acc2 <- (t2[1,1]+t2[2,2]) / sum(t2)
acc3 <- (t2[1,1]+t2[2,2]) / sum(t3)

acc1
acc2
acc3

plot(model1)
plot(model2)
plot(model3)
