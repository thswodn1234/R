# 국민 건강보험 대사 증후군
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1221")
df3 <- read.csv("06_국민건강보험공단500.csv", header = TRUE)
df3
# 대사증후군에 필요한 열을 추출하여 열명을 변경
names(df3) <- c("기준년도","가입자일련번호","시도코드","성별코드","연령대코드", "신장단위",       
                "체중단위","허리둘레","시력좌","시력우","청력좌","청력우","수축기혈압","이완기혈압","공복혈당",  
                "총콜레스테롤","트리글리세라이드","HDL콜레스테롤","LDL.콜레스테롤","혈색소","요단백"              
                ,"혈청크레아티닌",       "X.혈청지오티.AST"    , "X.혈청지오티.ALT",    
                "감마.지티피",          "흡연상태"            , "음주여부"            ,
                "구강검진.수검여부" ,   "치아우식증유무"     ,  "치석"                ,
                "데이터.공개일자"     )
df31 <- df3[, c("시도코드", "성별코드", "수축기혈압","이완기혈압", "공복혈당","트리글리세라이드","HDL콜레스테롤","허리둘레") ]
df31 

# 결측치 자료가 있는 행을 모두 삭제
library(dplyr)
df31 <- df31 %>% replace(is.na(.),0)
summary(df31)


# 높은 혈압( 135/85mg 이상)
# 높은 혈당(공복 혈당 100mg/dL 이상)
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)

# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군
df31$대사증후군 <- 0
df31$대사증후군 <-as.numeric(df31$대사증후군)
df31$대사증후군  <- ifelse(df31$수축기혈압 >= 135 & df31$이완기혈압 >= 85, df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$공복혈당 >= 100,df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$트리글리세라이드 >= 150,df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$성별코드 == 1 & df31$HDL콜레스테롤 < 40,df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$성별코드 == 2 & df31$HDL콜레스테롤 < 50,df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$성별코드 == 1 & df31$허리둘레 >=90,df31$대사증후군 + 1,df31$대사증후군 )
df31$대사증후군  <- ifelse(df31$성별코드 == 2 & df31$허리둘레 >= 85,df31$대사증후군 + 1,df31$대사증후군 )

df31$대사증후군 <- ifelse(df31$대사증후군 == 0, "정상",
                     ifelse(df31$대사증후군 >= 1 & df31$대사증후군 <= 2 , "주의군", "위험군"))

install.packages("party")
library(party)

fomula <- 
df31$허리둘레 <- as.factor(df31$허리둘레)
h_ctree <- ctree(formula, data = df31[])

install.packages("party")
library(party)

x <- sample(1:nrow(df1), 0.7 * nrow(df1)) 
x


train <- df1[x, ]
test <- df1[-x, ]

model1 <- ctree(formula = 허리둘레 ~ + 수축기혈압 + 공복혈당 + 트리글리세라이드 , data=train)
model2 <- ctree(formula = 몸무게 ~  키 , data=train)


#예측 
pred1 <- predict(model1, test)
pred2 <- predict(model2, test)

#평가
RMSE1 <-  sqrt(mean((test$몸무게 - pred1) ^ 2))
RMSE2 <-  sqrt(mean((test$키 - pred2) ^ 2))


#혼돈행렬
t1<- table(test$비만도,pred1)
t2<- table(test$비만도,pred2)

acc1 <- (t1[1,1]+t1[2,2]+t1[3,3]) / sum(t1)
acc2 <- (t2[1,1]+t2[2,2]+t2[3,3]) / sum(t2)

acc1
acc2
