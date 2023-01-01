getwd()
install.packages("readxl")
library(readxl)
dfxl <- read_xlsx("../02/02_역주행사고.xlsx")
dfxl


# 테이블 분리
df1 <- subset(dfxl, dfxl$구분 == "전체")
df1

df2 <- dfxl[dfxl$구분 == "전체",]
df2

df2 <- dfxl[dfxl$구분 == "역주행",]
df2

# 일반 교통사고
df3 <- df1
df3$구분 <- "일반"
df3

df3[c("사고", "사망")]  <- df1[c("사고", "사망")] - df2[c("사고", "사망")]
df3

# 치명률 계산
df1$치명률 <- round(df1$사망 / df1$사고 * 100,2)
df2$치명률 <- round(df2$사망 / df2$사고 * 100,2)
df3$치명률 <- round(df3$사망 / df3$사고 * 100,2)

df2

#기초 통계값
summary(df2)
summary(df3)

mean(df2$치명률)
mean(df3$치명률)

cat("최근 3년간 역주행 교통사고의 치명률이 ", 
    round(mean(df2$치명률),1),
    "%로 일반 교통사고(",
    round(mean(df3$치명률),1),
    "%)보다 ",
    round(round(mean(df2$치명률),1) / round(mean(df3$치명률),1)),
    "배 높은 것으로 나타났다.")

#시각화
install.packages("ggplot2")
library(ggplot2)

ggplot(mapping =aes(x=년도, y=사고, fill=구분), data=dfxl) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle('년도별 사고건수')+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))


dftax <- read.csv("./02_부산광역시_지방세 체납현황.csv", header = T, stringsAsFactors = F, fileEncoding = 'euc-kr')
dftax

names(dftax)
#세목명c
cols = unique(dftax$세목명)
cols

#함수 만들기
makedf <- function(item){
  return(item)
}

makedf("주민세")


names(dftax)
dftax <- dftax[c("과세년도", "세목명", "체납액구간", "누적체납건수","누적체납금액")]
dftax

# 세목명
cols = unique(dftax$세목명)
cols
#과세년도 범주형
dftax$과세년도 <- as.factor((dftax$과세년도))

# 함수 만들기
makedf <- function(item) {
  temp <- subset(dftax, dftax$세목명 == item)
  ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=체납액구간), data=dftax) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle(item)+
    theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
}


makedf("주민세")
makedf("지방소득세")
makedf("등록면허세")

ggplot(mapping =aes(x=과세년도, y=누적체납건수, fill=과세년도), data=dftax) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("자동차세")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))




wdf <- read.csv("../02/02_기상개황.csv" , fileEncoding = "euc-kr")
names(wdf)
wdf

#필요한 열 추출
wdf <- wdf[c("월별.1.", "평균기온....", "평균상대습도....")]
names(wdf) <- c("월별", "평균기온", "평균상대습도")

wdf$불쾌지수 <- 0.81 * wdf$평균기온 + 0.01 * wdf$평균상대습도*(0.99 * wdf$평균기온 - 14.3) + 46.3

wdf$불쾌지수 <- ifelse(wdf$불쾌지수 < 68, "낮음",
                     ifelse(wdf$불쾌지수 < 75, "보통",
                            ifelse(wdf$불쾌지수 < 80, "높음", "매우높음")))
wdf <- wdf[2:13, ] 
wdft <- table(wdf$불쾌지수)
wdft
class(wdft)
barplot(wdft, col=c("red","pink","blue") )

wdft2 <- as.data.frame(wdf)
class(wdft2)
wdft2

ggplot(mapping =aes(x=Var1, y=Freq, fill=Var1), data=wdft2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("불쾌지수")+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'))
