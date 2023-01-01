#인구 동향
library(dplyr)

#데이터 불러오기(인구동향)
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1214/04/")
df <- read.csv("./04_인구동향.csv", header = TRUE)
df
# 열명 확인
names(df)

# "행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수"
names(df) <- c("행정구역별","시점","출생아수","사망자수","혼인건수","이혼건수")
names(df)

head(df) 

#EDA

#데이터 구조 확인
str(df)

#범주형 변경 : 시점 int -> category
df$시점 <- as.factor(df$시점)
mode(df$시점)
class(df$시점)

#결측치 확인
summary(df) 


df$출생아수


#결측치 내용 확인 
 is.na(df$출생아수)
 df[is.na(df$출생아수),]
 
 library(dplyr)
 # df$출생아수 <- df %>% filter(is.na(df$출생아수)) # 벡터에 데이터 프레임 넣으려고해서 오류
 df2 <- df %>% filter(is.na(df$출생아수))
 unique(df$행정구역별)
 unique(df$시점)
 df2   
   
#결측치 행 제거
sum((df$출생아수))
 
 library(dplyr)
df3 <- na.omit(df)
summary(df3) 
summary(df) 
df3

df <- df %>% replace(is.na(.),0)
summary(df) 
df$출생아수
 

#결측치 값 대체
df4 <- df 
df4$출생아수 <- ifelse(is.na(df4$출생아수),0,df4$출생아수)
summary(df4)

#반복을 통한 na값 처리
col <- names(df) 
col
is.vector(col)
col[3:6]
col <- names(df)[3:6] 
col

for(c in col) {
  print(df[,c])
}

for(c in col){
  temp <- df[,c]
  temp <- ifelse(is.na(df[,c]), 0, df[,c])
  df[,c] <- temp
}

summary(df)
summary(df4)
df4 <- df4 %>% replace(is.na(.),0)
summary(df4)


# 자연증가수열 추가
 df$자연증가수 <- df$출생아수 - df$사망자수
 df
 
 str(df)
 
 df5 <- df[df$행정구역별 == '전국' & df$자연증가수 < 0, ]
 df5
 str(df5)
 summary(df5)
 
 # case1
 df[df$행정구역별 == '전국' & df$자연증가수 < 0, ]
 # case2
 df[which(df$행정구역별 == '전국' & df$자연증가수 < 0),]
 # case3
 df %>% filter(df$행정구역별 == '전국' & df$자연증가수 < 0)
 
 
# 기술통계분석-범주형자료-빈도분석
 table(df$행정구역별)
 table(df$시점)
 
# 기술통계분석-연속형자료-산점도그래프
plot(df$출생아수,df$혼인건수)

#자료 나누기
dft <- df %>%
        filter(df$행정구역별 == '전국')
dfa <- df %>%
        filter(df$행정구역별 != '전국')



 
#데이터 분석


# 전국데이터 자연 증가수 그래프
dft 
dfa

x <- table(dfa$시점)

library(ggplot2)
names(dfa)
barplot(x,df$자연증가수)
barplot(dfa$자연증가수)

