#암종류별 성별 분석
install.packages("dplyr")
library(dplyr)

#데이터 불러오기(암발생자수)
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1208/")
df1 <- read.csv("./03/03_암발생자수_.csv", header = TRUE)
df1




# 열명 변경
# "암종별", "성별", "연령별", "발생자수", "조발생률"
 
names(df1) <- c("암종별","성별","연령별","발생자수","조발생률")
names(df1)

df2
unique(df2$'암종별') 

# 데이터셋 조회
# 1) 특정 변수 조회
t1 <- df1$암종별
mode(t1)
class(t1)
is.vector(t1)

# 2) 특정 열명을 사용하여 조회
t2 <- df1['암종별']
mode(t2)
class(t2)

# 3) 특정 행 조회 :1행 조회
df1[1,] 
df1[c(2,4),]


# 4)특정행 제거 : 1행제거
# -(마이너스) 붙이면 제거
df <- df1[-1,] 
head(df)

# 암종류 확인
unique(df$'암종별') #중복제거거

# dplyr는 %>% 연산을 쓸수 있다
df2 <- df %>%
        filter(암종별 != "모든 암(C00-C96)") %>%
        filter(연령별 == "계")

# 성별 남
df21 <- df2 %>%
        filter(성별 == "계")
df21

# 성별 여여
df22 <- df2 %>%
  filter(!(성별 =="계"))
df22

# 특정 열 가져오기
df21 <- df21[,c('암종별', '발생자수')]
df21

df22 <- df22[,c('암종별','성별', '발생자수')]
df22

# 5) 특정행 열 조회
df[1:3,c('암종별','발생자수')] 


# 열 데이터 타입 확인
str(df) 

# 값 변경 : - => 0


df$발생자수 <- ifelse(df$발생자수 == '-',0, df$발생자수)
df$발생자수 <-as.numeric(df$발생자수)

df$조발생률 <- ifelse(df$조발생률 == '-',0, df$조발생률)
df$조발생률 <-as.numeric(df$조발생률)
df
str(df)

# 암종류 확인
unique(df$'암종별') #중복제거거

# dplyr는 %>% 연산을 쓸수 있다
df2 <- df %>%
  filter(암종별 != "모든 암(C00-C96)") %>%
  filter(연령별 == "계")

# 성별 남
df21 <- df2 %>%
  filter(성별 == "계")
df21

# 성별 여여
df22 <- df2 %>%
  filter(!(성별 =="계"))
df22

# 특정 열 가져오기
df21 <- df21[,c('암종별', '발생자수')]
df21

df22 <- df22[,c('암종별','성별', '발생자수')]
df22

# 5) 특정행 열 조회
df[1:3,c('암종별','발생자수')] 

df$조발생률 <-as.numeric(df$조발생률)
df22$발생자수 <- ifelse(df22$발생자수 == '-',0, df22$발생자수)
df22$발생자수 <-as.numeric(df22$발생자수)
df22
str(df22) 

df21
df21$발생자수 <-as.numeric(df21$발생자수)
str(df21)

str(df)
str(df21)
str(df22)


# 그래프

library(ggplot2)
df221 <- df22 %>%
          filter(df22$성별 == "남자")

plot(df21$발생자수, type='o', col="red")
par(new = T)
plot(df221$발생자수, type='o', col="blue", axes=F)
par(new = T)


cancer_gender <- table(df22$암종별,df22$성별,df22$발생자수)
cancer_gender
barplot(cancer_gender)
# 열 데이터타입 변경
 

# 모든암 제거하고 연령별이 계인 데이터 
 
# 그래프 
library(ggplot2)
 
 
 
 
 
 