#한국환경공단_에어코리아_대기오염통계 현황
#시도별 실시간 평균정보 조회 상세기능명세
#최근 한달간 지역별 일평균 대기오염 정보
#기준초과인경우 
#https://www.airkorea.or.kr/web/contents/contentView/?pMENU_NO=132&cntnts_no=6
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1223")

#json처리
install.packages("jsonlite") 
library(jsonlite)

#자료처리
library(dplyr)

key <- "86HmDc9G1Y%2FiZoJxjyRDUlPeNAZ3IPSdRifnBnxMzWPW8T2msURZBxSXzcriGwPztUwI%2BEhvWUQkwbxEZxwqZA%3D%3D"
url <- paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?itemCode=PM10&dataGubun=HOUR&searchCondition=MONTH&pageNo=1&numOfRows=100&returnType=json&serviceKey=",key,sep="")  
air <- fromJSON(url) 
air
air$response$body$items


  


#데이터 가져오기함수
getData <- function(item, gubun) {
item <-  
url <- paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?itemCode=PM10&dataGubun=HOUR&searchCondition=MONTH&pageNo=1&numOfRows=100&returnType=json&serviceKey=",key,sep="")  
data <- fromJSON(url)

return(data$response$body$items)
}


getData <- function(item, gubun) {
  
  key <- "86HmDc9G1Y%2FiZoJxjyRDUlPeNAZ3IPSdRifnBnxMzWPW8T2msURZBxSXzcriGwPztUwI%2BEhvWUQkwbxEZxwqZA%3D%3D"
  url <- paste("http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst?",
               "itemCode=",item,
               "&dataGubun=",gubun,
               "&searchCondition=MONTH&pageNo=1&numOfRows=100&returnType=json&serviceKey=",key, sep='')
  data <- fromJSON(url)
  return(data$response$body$items)
}


#1. pm10, O3 데이터 추출하여 합치기
 
pm10 <- getData("PM10","DAILY")
o3 <- getData("o3","DAILY")
names(pm10) 
names(o3)
str(pm10)
str(o3)

df <- bind_rows(pm10,o3)
df
length(df)

#2. 지역명 벡터
area <- c("seoul", "busan", "daegu","incheon","gwangju",
          "daejeon", "ulsan", "gyeonggi", "gangwon",
          "chungbuk", "chungnam", "jeonbuk", "jeonnam",
          "gyeongbuk", "gyeongnam", "jeju", "sejong")

areaname <- c("서울","부산","대구","인천","광주","대전",
              "울산","경기","강원","충북","충남","전북",
              "전남","경북","경남","제주","세종")

names(df)
 

#3. 통합데이터프레임 만들기
dft <- data.frame()


for(i in 1 : length(area)) {
  temp <- c()

  temp$dataTime <- unlist(df["dataTime"])
  temp$itemCode <- unlist(df["itemCode"])
  temp$area <- areaname[i]
  temp$item <- unlist(df[area[i]])
  
  dft <- bind_rows(dft,temp)
}

for (i in 1: length(area)){
  t <- df[c("dataTime", "itemCode", area[i])]
  t$area <- areaname[i]
  print(t)
}

dft


dft2 <- data.frame()
for (i in 1: length(area)){
  t <- df[c("dataTime", "itemCode", area[i])]
  t$area <- areaname[i]
  names(t) <- c("dataTime", "itemCode", "item", "area" )
  dft2 <- bind_rows(dft2,t)
}
dft2

dft$item <- as.numeric(dft$item)
#4.주의보
#https://www.airkorea.or.kr/web/dustForecast?pMENU_NO=113
dft$기준 <- ifelse(dft$itemCode == "PM10",
                 ifelse(dft$item <= 30, "좋음", 
                 ifelse(dft$item <= 80, "보통", 
                 ifelse(dft$item <= 150, "나쁨", "매우나쁨"))),
                 ifelse(dft$item <= 0.03, "좋음",
                 ifelse(dft$item <= 0.09, "보통",
                 ifelse(dft$item <= 0.15, "나쁨", "매우나쁨"))))

str(dft)

#5.일자별 주의보정보
names(dft2)
dfn <- table(dft$dataTime,dft$기준)
dfn

dfn1 <- as.data.frame.matrix(dfn)
summary(dfn1)
dfn2 <- as.data.frame(dfn)
summary(dfn2)

names(dfn2) <- c("일자", "기준","기준수")

library(ggplot2)
ggplot(dfn2, aes(x=일자, y =기준수, group=기준, color=기준)) +
  geom_line() +
  geom_point() +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))

#5.지역별 PM10이 좋음인 날 수 
names(dft)
dfPM10 <- dft[dft$itemCode == "PM10" & dft$기준 =="좋음",]
dfPM10n <- table(dfPM10$dataTime,dfPM10$area,dfPM10$기준)
dfPM10n2 <- as.data.frame(dfPM10n)
names(dfPM10n2) <- c("일자","지역", "기준","기준수")


ggplot(dfPM10n2, aes(x=지역, y =기준수, group=지역 , fill=지역)) +
  geom_bar(stat="identity") +
  labs(x="", y="")+
  theme(axis.text.x=element_text(angle=90, hjust=1))
