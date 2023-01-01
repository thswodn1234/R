setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1221")
# 체감온도
df <- read.csv("06_지상관측.csv", header = TRUE)
df

names(df) <- c("지점","지점명","일시","기온","풍속","상대습도")
names(df)

df$풍속 <-  df$풍속 / 0.277778 
df$체감온도 <- 13.12 + 0.6215*df$기온 - 11.37*df$풍속*0.16 + 0.3965*df$풍속*0.16*df$기온
df

df1 <- df[df$기온 <= 10 & df$풍속 > 1.3 & df$지점명 == "부산", ]


x <- c(df1$일시)
y <- c(df1$체감온도)

# 체감온도
barplot(y, names=x,angle=90,las=2,col=rainbow(10), main="부산지역 겨울철 체감온도")


df$열지수 <- 0.5 * (df$기온 + 61.0 + ((df$기온-68.0)*1.2)+ (df$상대습도*0.094))
df2 <- df[df$지점명 == "서울" | df$지점명 == "제주" | df$지점명 == "부산", ]
df2
install.packages("ggplot2")
library(ggplot2)

#평균기온
z <- mean(df2$기온)
z

# 2-1기온현황
ggplot(mapping =aes(x=일시, y=기온, fill=지점명), data=df2) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("기온현황")+
  geom_hline(yintercept = z, color = 'black', linetype = 20)+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

#2-2 열지수

ggplot(mapping =aes(x=일시, y=열지수, fill=지점명, color = 지점명), data=df2) +
  geom_point(stat="identity", position=position_dodge()) +
  ggtitle("열지수 현황")+
  geom_hline(yintercept = 5, color = 'red')+
  geom_line(mapping=aes(group=지점), size=1.2)+
  theme(plot.title = element_text(hjust = 0.5,size=20,face='bold'),axis.text.x=element_text(angle=90, hjust=1))

# 국민 건강보험 대사 증후군

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

fomula <- 허리둘레 ~ + 수축기혈압 + 공복혈당 + 트리글리세라이드
df31$허리둘레 <- as.factor(df31$허리둘레)
h_ctree <- ctree(formula, data = df31[])

