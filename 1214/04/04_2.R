# 기상개황 자료를 이용하여 월별 불쾌지수를 계산하고 불쾌지수가 높음이상인 월을 구하시오.
# 
# https://kosis.kr/statHtml/statHtml.do?orgId=735&tblId=DT_A1040&vw_cd=MT_ZTITLE&list_id=215_215A_735_73503_A&seqNo=&lang_mode=ko&language=kor&obj_var_id=&itm_id=&conn_path=MT_ZTITLE
# 
setwd("C:/Users/user/Desktop/수업자료/R프로그래밍/1214/04/")
df <- read.csv("./04_기상개황.csv", header = TRUE)
df
names(df)
names(df) <- c("월별","평균기온","평균최고기온","최고극값기온","평균최저기온","최저극값기온","강수량","평균상대습도"   
                ,"최소상대습도","평균해면기압", "이슬점온도","평균운량","일조시간","최심신적설","평균풍속바람","최대풍속바람" 
               ,"최대순간풍속" )
df$불쾌지수 <- 0.81 * df$평균기온 + 0.01 * df$평균상대습도 * (0.99 * df$평균상대습도 - 14.3) + 46.3

df$불쾌지수
df$불쾌지수 <- ifelse(df$불쾌지수 >= 80 ,'매우높음', ifelse(df$불쾌지수 >= 75,'높음',ifelse(df$불쾌지수>68,'보통','낮음')))
df

plot(df$평균기온)




# 불쾌지수 공식

# 
# DI = 0.81 * Ta + 0.01 * RH * (0.99 * Ta - 14.3) + 46.3
# DI: 불쾌지수
# Ta: 건구온도(평균기온)
# RH: 상대습도(평균상대습도)
# 불쾌지수 단계
# 
# 매우높음: 80이상
# 높음: 75이상 80미만
# 보통: 68이상 75미만
# 낮음: 68미만

