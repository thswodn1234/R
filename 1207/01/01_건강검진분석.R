# 해결문제 
# BMI는 몸무게와 키를 이용하여 체지방율을 측정하는 지수이다. 
# 자신의 몸무게와 키를 각각 변수 weight와 height에 저장하고 BMI지수를 계산해 본다. 
# 단, 키는 cm로 입력 받아서 처리한다.
# 
# BMI = 체중(kg) / (키(m) x키(m))


# 키와 몸무게 scala입력
height <- scan()
weight <- scan()
BMI <- weight / (height / 100) ** 2

# 키와 몸무게 scala입력
print("키와 몸무게 입력") 
data <- scan()
height <- data[1]
weight <- data[2]
BMI <- weight / (height / 100) ** 2
BMI

# 몸무게 수치 변환
data <- readline()
data
mode(data)



# BMI 계산
 BMI

# 키와 몸무게 vector입력
 

#문자열 입력
 

# stringr 패키지 설치
 install.packages("stringr")
 library(stringr)
# 문자열 분리
 data  <- str_split(data," ")
 data


# 자료형 확인
 mode(data)
 
# 벡터로 형변환
data <- unlist(data)
mode(data)
# 벡터 확인
is.vector(data)
 

# 숫자벡터로 변경
 data <- as.numeric(data)
 data
 
# 데이터프레임 입력
df <- data.frame() 
mode(df)
class(df)

df <- edit(df) 
df
# 데이터프레임 열명 변경
 
names(df) <- c("키", "몸무게")
df$BMI <- df$몸무게 / (df$키 / 100) ** 2
df
# 해결문제 
# 국민건강보험공단 자료를 이용하여 BMI와 비만도를 구하시오.
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1207/01/")

BMI_df <- read.table(file = "01_국민건강보험공단500.csv", sep = ",", header = T,fileEncoding = "euc-kr")
BMI_df
BMI_df$BMI <- BMI_df$체중 / (BMI_df$신장 / 100) ** 2
BMI_df
head(BMI_df)
options(max.print=1000000)

# 비만도 
BMI_df$비만도 <- ifelse(BMI_df$BMI < 20, "저체중",
                     ifelse(BMI_df$BMI < 25, "정상",
                            ifelse(BMI_df$BMI < 30, "과체중", "비만")))
BMI_df
# 저체중	20 미만
# 정상	20 - 24
# 과체중	25 - 29
# 비만	30 이상
 
# 빈도 테이블
table(BMI_df$성별)
table(BMI_df$비만도, BMI_df$성별)

# 빈도 테이블 저장
write.csv(x = table(BMI_df$비만도, BMI_df$성별),quote=FALSE,file="빈도테이블.csv")
write.csv(x = BMI_df,quote=FALSE,file="비만도.csv")


# 해결문제
# 국민건강보험 관리공단의 건강검진 자료를 이용하여 대사증후군을 판별하시오.
Metabolic_df <- read.table(file = "국민건강보험공단_건강검진정보_20211229.CSV", sep = ",", header = T,fileEncoding = "euc-kr")
Metabolic_df 

#열 명 확인
names(Metabolic_df)

#필요한 열 추출
df2 <- Metabolic_df[c("성별코드", "허리둘레", "수축기.혈압","이완기.혈압", "식전혈당.공복혈당","트리글리세라이드","HDL.콜레스테롤")]

head(df2)

#NA 값 제거
df2 <- na.omit(df2)

# 높은 혈압(130/85mmHg 이상)
df2$높은혈압
# 높은 혈당(공복 혈당 100mg/dL 이상)
# 높은 중성지방(트리글리세라이드 150mg/dL 이상)
# 낮은 HDL 콜레스테롤 수치(남성은 40mg/dL 미만, 여성은 50mg/dL 미만)
# 복부 비만(남성 90cm 이상, 여성 85cm 이상)
# 판별
# 0 : 정상, 1~2 : 주의군, 3~5 : 위험군