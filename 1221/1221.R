install.packages("jsonlite")
library(jsonlite)

#일일박스 오피스 자료 가져오기
# url <- "http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=f5eef3421c602c6cb7ea224104795888&targetDt=20120101"

apikey <- "f5eef3421c602c6cb7ea224104795888"
dt <- "20221220"

url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=", apikey ,"&targetDt=" , dt, sep="")

mv <- fromJSON(url)
mv

class(mv)
mode(mv)


# 박스오피스 목록 추출
BoxOfficeList <- mv$boxOfficeResult$dailyBoxOfficeList
names(BoxOfficeList)
str(BoxOfficeList)

col <- c("rnum","rank","rankInten",
         "salesAmt","salesShare","salesInten","salesChange","salesAcc",
          "audiCnt","audiInten","audiChange","audiAcc","scrnCnt","showCnt")

for (c in col){
  BoxOfficeList[c] <- as.numeric(unlist(BoxOfficeList[c]))
}
str(BoxOfficeList)

# 매출평균보다 매출이 높은 영화
library(dplyr)
BoxOfficeList %>%
  filter(salesAmt > mean(salesAmt)) %>%
  select(rank, movieNm, salesAmt)

mean(BoxOfficeList$salesAmt)


# 날짜를 입력하면 영화목록 출력
box <- function(x){
  apikey <- "f5eef3421c602c6cb7ea224104795888"
  dt <- x
  
  url <- paste("http://kobis.or.kr/kobisopenapi/webservice/rest/boxoffice/searchDailyBoxOfficeList.json?key=", apikey ,"&targetDt=" , dt, sep="")
  
  mv <- fromJSON(url)
  BoxOfficeList <- mv$boxOfficeResult$dailyBoxOfficeList
  return(BoxOfficeList$movieNm)
}

box("20221219")

# 요일별 음식물쓰레기

key <- "86HmDc9G1Y%2FiZoJxjyRDUlPeNAZ3IPSdRifnBnxMzWPW8T2msURZBxSXzcriGwPztUwI%2BEhvWUQkwbxEZxwqZA%3D%3D"
page <- 1
rowNum <- 10
disYear <- 2019
disMonth <- "05"

api <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?ServiceKey=",key,"&type=json&page=",page,"&rowNum=",rowNum,"&disYear=",disYear,"&disMonth=",disMonth, sep="")

g <- fromJSON(api)
g

# 1222
install.packages("jsonlite")
library(jsonlite)

garbage <- function(z,w){
  key <- "86HmDc9G1Y%2FiZoJxjyRDUlPeNAZ3IPSdRifnBnxMzWPW8T2msURZBxSXzcriGwPztUwI%2BEhvWUQkwbxEZxwqZA%3D%3D"
  page <- 1
  rowNum <- 10
  #disYear <- z
  #disMonth <- w
  
  api <- paste("http://apis.data.go.kr/B552584/RfidFoodWasteServiceNew/getTotalDayList?ServiceKey=",key,"&type=json&page=",page,"&rowNum=",rowNum,"&disYear=",z,"&disMonth=",w, sep="")
  
  g <- fromJSON(api)
  return(g$data$list)
}

g4 <- garbage(2020,"01")

g1 <- garbage(1,10,2020,"01")
g2 <- garbage(1,10,2021,"01")
g3 <- garbage(1,10,2022,"01")

g1$disDay <- c("일","월","화","수","목","금","토")
g2$disDay <- c("일","월","화","수","목","금","토")
g3$disDay <- c("일","월","화","수","목","금","토")
g <- rbind(g1,g2,g3)


# 정렬하기
order_v <- c(c("일","월","화","수","목","금","토"))
# 컬럼을 factor 자료형으로 변경하고, levels에 사용자지정 정렬용 벡터 지정
g$disDay <- factor(g$disDay, levels = order_v)
unique(g$disDay)

g_orderd <- g[order(g$disDay, decreasing = FALSE), ]


g$disDay 


install.packages("ggplot2")
library(ggplot2)

g$dayAverQuantity
  
ggplot(aes(x=disDay, y=dayAverCount  , fill=disYear), data=g) +
  geom_bar(stat="identity", position='dodge') +
  ggtitle("요일별 평균 배출 횟수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))

ggplot(aes(x=disDay, y=dayAverQuantity  , fill=disYear), data=g) +
  geom_bar(stat="identity", position='dodge') +
  ggtitle("요일별 평균 배출량")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


# 달별
library(dplyr)

dft <- garbage("2021",1)
for( i in 2:12){
  if (i < 10) {m = paste("0",i,sep="")}
  else m = as.character(i);
  
  temp <- garbage("2021",m)
  dft <- bind_rows(dft,temp)
}
dft

# 데이터 열 생성
dft <- dft %>%
  mutate(disWeek = case_when(disDay == 1 ~ "일",
                             disDay == 2 ~ "월",
                             disDay == 3 ~ "화",
                             disDay == 4 ~ "수",
                             disDay == 5 ~ "목",
                             disDay == 6 ~ "금",
                             disDay == 7 ~ "토",
                             ))
dft



g$disDay 
ggplot(aes(x=disWeek, y=dayAverCount  , fill=disYear), data=dft) +
  geom_bar(stat="identity", position='dodge') +
  ggtitle("요일별 평균 배출량")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
