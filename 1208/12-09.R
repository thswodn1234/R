df1 <- read.csv("./03/03_치매환자현황.csv", header = T, stringsAsFactors = F, fileEncoding ='euc-kr')

df1
names(df1)

# 거주지역별 치매환자 그래프
install.packages("ggplot2")
library(ggplot2)
qplot(거주지역,data = df1, fill=거주지역  )+ ggtitle("거주지역별 치매환자")+theme(plot.title = element_text(hjust = 0.5,face= 'bold', color='red'))


# 데이터 타입확인
class(df1$진단일자)
df1$진단일자 <- as.Date(df1$진단일자)

# 날짜처리 패키지
install.packages("lubridate")
library(lubridate)

year(df1$진단일자)
df1$데이터기준일자 <- as.Date(df1$데이터기준일자)
class(df1$데이터기준일자)
df1

df1$진단일수 <- difftime(df1$데이터기준일자,df1$진단일자,units ="days")
df1$진단일수 <- as.numeric(df1$진단일수)
mean(df1$진단일수) # 평균

df1

df1$나이 <- year(df1$데이터기준일자) - df1$출생년도
df1$나이



# 연령별 치매
age<-table(cut(df1$나이,breaks=(4:11)*10))
age
as.data.frame(age)

names(age) <- c("40대","50대","60대","70대","80대","90대","100대")
age

barplot(age)


install.packages("dplyr")
library(dplyr)
df2 <- read.csv("./03/03_암발생자수_.csv", header = T, stringsAsFactors = F, fileEncoding ='euc-kr')

df2  
names(df2)
# 열명
names(df2) = c("암종별" , "성별", "연령별", "2019년", "2019년_1")

#모든암
df21 <- subset(df2, df2$암종별 == "모든 암(C00-C96)")
df21

# %in% 포함 연산자
df2 %>%
  filter(!(연령별 %in% c("계", "연령미상")))


df22 <- df2 %>%
        filter(df2$암종별 == "모든 암(C00-C96)" & !(연령별 %in% c("계", "연령미상")))
df22

# 연령별
unique(df22$연령별)


df22$연령대 <- ifelse(df22$연령별 %in% c("0-4세",  "5-9세","10-14세" , "15-19세"  ,"20-24세" , "25-29세"  ,"30-34세" , "35-39세" ), "30대 이하",
               ifelse(df22$연령별 %in% c("40-44세" , "45-49세" , "50-54세",  "55-59세"), "40대~50대",
                      ifelse(df22$연령별 %in% c("60-64세" , "65-69세",  "70-74세" , "75-79세" ), "60대~70대","80대이상"))     )
df22
names(df22) <- c("암종별"  , "성별"  ,   "연령별" ,  "y2019년",   "2019년_1", "연령대"  )
names(df22)
df22$`y2019년` <- as.numeric(df22$`y2019년`)
class(df22$y2019)

qplot(연령대, data=df22, fill=연령대) 

df22g <- df22 %>%
          group_by(연령대, 성별) %>%
          summarise(계 = sum(y2019년))

df22g
