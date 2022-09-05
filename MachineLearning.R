setwd('C:/Users/kgt08')
getwd()

setwd('C:/Users/kgt08/Dropbox/2022_Program_Undergraduate/ML_code/Data')
install.packages('readxl')
library(readxl)
R_LIBS_SITE="C:\\Program Files\\R\\R-4.1.2\\library"

# 데이터 불러오기
df = read_excel('df_case_clean_0508.xlsx')
head(df)

# 보도일을 date로 변환
colnames(df)[1] = 'date'

df1 = df[1:200,]  # 데이터를 200개까지만 추출
D = df1$date
T = df1$total
y1 = df1[,3] # 연령대별로 데이터를 저장
y2 = df1[,4] 
y3 = df1[,5] 
y4 = df1[,6] 
y5 = df1[,7]

plot(D, T, type = 'h', lwd = 3, col = 'blue')

# 2월부터 8월까지의 연령별 확진자 수 그래프
df2 = cbind(T, y1, y2, y3, y4, y5)

plot(D, df2[,1], type = 'l', lwd = 3, col = 'orange')
lines(D, df2[,2], col = 'red', type = 'h',  lwd = 2)
lines(D, df2[,3], col = 'skyblue', type = 'h', lwd = 2)
lines(D, df2[,4], col = 'blue', type = 'h', lwd = 2)
lines(D, df2[,5], col = 'black', type = 'h', lwd = 2)
lines(D, df2[,6], col = 'green', type = 'h', lwd = 2)
legend( x = "topright", legend = c('전체', '10대', '20대', '30대',
                                   '40대', '50대'), 
        col = c('orange', 'red', 'skyblue', 'blue', 'black', 'green'),
        fill = c('orange', 'red', 'skyblue', 'blue', 'black', 'green'))

install.packages('lubridate')
library(lubridate)

# 시간을 년도, 달, 계절로 된 각각의 컬럼을 생성
df$year = year(df$date)
df$month = month(df$date)
df$season = (df$month %/% 3)%%4
head(df)

install.packages('dplyr')
library(dplyr)

colnames(df)[3:7] = c('ten', 'twenty', 'thirty', 'fourty', 'fifty')
df4 = df %>% group_by(season, year) %>% summarise(sum_total = sum(total), 
                                                  sum_1 = sum(ten), sum_2 = sum(twenty), sum_3 = sum(thirty), sum_4 = sum(fourty),
                                                  sum_5 = sum(fifty))
df4

# 계절에 따른 연도별 연령별 확진자 수

par(mfrow = c(2,2))
spring = df4[1:3, -3]
summer = df4[4:6, -3]
fall = df4[7:8, -3]
winter = df4[9:10, -3]

par(mfrow = c(1,4))
barplot(cbind(spring$sum_1,spring$sum_2,spring$sum_3,spring$sum_4,spring$sum_5) ~ spring$year, col = c(3:7), beside = TRUE, main ='봄의 연도별 확진자수')         
legend(x = 'topleft', legend = c('10대', '20대', '30대','40대', '50대'), fill = c(3:7), cex = 0.3)
barplot(cbind(summer$sum_1,summer$sum_2,summer$sum_3,summer$sum_4,summer$sum_5) ~ summer$year, col = c(3:7), beside = TRUE, main = '여름의 연도별 확진자수')         
legend(x = 'topleft', legend = c('10대', '20대', '30대','40대', '50대'), fill = c(3:7),cex = 0.3)
barplot(cbind(fall$sum_1,fall$sum_2,fall$sum_3,fall$sum_4,fall$sum_5) ~ fall$year, col = c(3:7), beside = TRUE, main ='가을의 연도별 확진자수')         
legend(x = 'topleft', legend = c('10대', '20대', '30대','40대', '50대'), fill = c(3:7),cex=0.3)
barplot(cbind(winter$sum_1,winter$sum_2,winter$sum_3,winter$sum_4,winter$sum_5) ~ winter$year, col = c(3:7), beside = TRUE, main = '겨울의 연도별 확진자수')         
legend(x = 'topleft', legend = c('10대', '20대', '30대','40대', '50대'), fill = c(3:7),cex=0.3)

install.packages('e1071')
library(e1071)

# 머신러닝 파트

#85행까지의 데이터를 새로운 객체에 저장
data = read_excel('df_case_clean_0508.xlsx')
df_85 = data[1:85,]
df_85$idx = rownames(df_85) # index를 idx열에 저장
df_85$total_sum = cumsum(df_85$total) # total의 누적합을 total_sum열에 저장
df_85 = df_85[,10:11]  # idx와 total_sum열만 남기고 제외
head(df_85)

# 정규화 함수
normalize = function(x){
  nor_df_85 = (x-mean(x))/sd(x)
}

# 훈련데이터와 테스트 데이터 분리
x = df_85[,1]
y = df_85[,2]
set.seed(2022)

idx = sample(1:nrow(df_85), size = nrow(df_85)*0.7, replace = FALSE)
x_train = x[idx,];x_train
x_test = x[-idx,];x_test
y_train = y[idx,];y_train
y_test = y[-idx,]

x_train = transform(x_train, idx = as.numeric(idx) )
x_test = transform(x_test, idx = as.numeric(idx) )

# 각 데이터셋들을 표준정규분포로 정규화
x_train = scale(x_train, center = TRUE, scale = TRUE)
x_test = scale(x_test, center = TRUE, scale = TRUE)
y_train = scale(y_train, center = TRUE, scale = TRUE)
y_test = scale(y_test, center = TRUE, scale = TRUE)  

#선형회귀모델 학습
train = cbind(x_train, y_train);train
train = as.data.frame(train)
lrmodel = lm(total_sum ~ idx, data = train)
x_test = as.data.frame(x_test)


#다항회귀모델 학습
polymodel = lm(total_sum ~ poly(idx, 4), data = train)


#서포트 벡터 머신 모델 학습
svmmodel = svm(total_sum ~ idx, type = 'eps-regression', data = train)


# 위 모델로 예측한 결과와 실제값의 그래프\
par(mfrow = c(1,1))
x = transform(x, idx = as.numeric(idx) )
x = scale(x, center = TRUE, scale = TRUE)
y = scale(y, center = TRUE, scale = TRUE)

# 정규화된 그래프로 각 모델별 예측 적용
x = data.frame(x)
lr_preds = predict(lrmodel, x)   # data에는 데이터프레임 형식만 가능
poly_preds = predict(polymodel, x)
svm_preds = predict(svmmodel, x)

x = transform(x, idx = as.numeric(idx) )  # x를 다시 numeric형태로 변환
x = scale(x, center = TRUE, scale = TRUE)
plot(x,y, col = 'orange', lwd = 2, ylim = c(-1.5,1.5))
lines(x, lr_preds,  col ='green', lwd = 2)
lines(x, poly_preds, col = 'red', lwd = 2)
lines(x, svm_preds, col = 'blue', lwd = 2)
legend(x = 'topleft', legend = c('lr', 'svr', 'poly'), fill = c('green', 'red', 'blue'))


