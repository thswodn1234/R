#- 회귀모델
#(1) 모델 만들기
#model <- lm(formula = Y축열명~ X축 열명1+ X축 열명2+ X축 열명3, data=train)
#model

#(2) 예측
#pred <- predict(model, test)

#(3) 평가(정확도)
#RMSE <-  sqrt(mean((test$Y축열명 - pred)^2))


#- 분류모델
#(1) 모델 만들기
#model <- ctree(Y축열명 ~ X축 열명1 +X축 열명2 + 
#                 X축 열명3 + X축 열명4,
#               data = train)

#(2) 예측
#pred <- predict(model, test) 

#(3) 혼돈행렬
#t <- table(test$Y축 열명, pred)

#(3) 정확도
#accuracy <- (t[1,1] + t[2,2] + t[3,3]) / sum(t)