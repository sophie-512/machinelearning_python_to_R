library(readxl)
data = read_excel('mydata.xlsx')
View(data)

# 독립변수, 종속변수 지정
columns <- colnames(data);columns
variables <- columns[4:21];variables
targets <- columns[2];targets

data_FS_X <- data[variables]; data_FS_X # 독립변수만 있는 데이터
data_FS_Y <- data[targets]; data_FS_Y# 종속변수만 있는 데이터

# min-max 정규화 함수생성
scaler <- function(x){
    scaled_x <- (x - min(x)) / (max(x) - min(x))
    return(scaled_x)
}

# 각 데이터를 최소,최대 정규화로 변환
data_FS_X <- scaler(data_FS_X)
data_FS_X <- data.frame(data_FS_X)
colnames(data_FS_X)

# 각 열에 결측치 데이터가 있는지 확인 : colSums()
colSums(is.na(data_FS_X))
colSums(is.na(data_FS_Y))

# 훈련데이터와 테스트 데이터 분리
set.seed(2022)
idx = sample(1:nrow(data_FS_X), size = nrow(data_FS_X)*0.7, replace = FALSE)
x_train <- data_FS_X[idx,][,1:17];x_train
x_test <- data_FS_X[-idx,][,1:17]
y_train <- data_FS_Y[idx,];y_train
y_test <- data_FS_Y[-idx,]

nrow(x_train)
nrow(y_train)

train = cbind(x_train, y_train)
train = as.data.frame(train) # dataframe 형태로 변환

train <- train[,1:17];train
train <- cbind(train, y_train)
colnames(train)[18] = 'count'
View(train)


# 각 독립변수와 종속변수 사이의 단변량 F검정을 통해 p값을 구해 중요하지 않은 변수를 제거
variables <- variables[1:17];variables
result = c()
for (i in 1:length(variables)){
  x <- train[,i]
  model <- summary(aov(count ~ x, data = train))
  result <- c(result, model[[1]]$'Pr(>F)'[1])
}
result # 각 독립변수 별 p-value

result <- -log(result, base = 10);result
max(result)
result <- result/max(result)
barplot(result, names = variables, space = 1, col = 'skyblue', cex.names = 0.7, las = 2, lwd = 2)
# las : x축 라벨을 세로로 작성, cex.names : x축 라벨의 크기
# 훈련 데이터셋에 들어가는 데이터에 따라서 p값은 달라지므로 파이썬과 다를 수 있다.
p_dataframe <- data.frame(cbind(variables, result));p_dataframe

#install.packages('randomForest')
library(randomForest)
library(caret)
control <- rfeControl(functions = rfFuncs, method = 'repeatedcv', repeats = 3, number = 10)
#rfe1 <- rfe(x = x_train, y = as.matrix(y_train), size = c(1:17), rfeControl = control)
#RMSE <- rfe1$results[[2]] ;RMSE # 각 변수의 RMSE값
#cat('MSE의 평균 : ', mean(RMSE^2), 'MSE값들의 표준편차 : ', sd(RMSE^2))

data_FS_X <- data.frame(data_FS_X[,1:17])
data_FS_Y <- as.matrix(data_FS_Y)
rfe2 <- rfe(x = data_FS_X, y = data[[2]], size = c(1:17), rfeControl = control);rfe2
RMSE2 <- rfe2$results[[2]]
cat('MSE의 평균 : ', mean(RMSE2^2), 'MSE값들의 표준편차 : ', sd(RMSE2^2))
boxplot(RMSE2^2)

control <- rfeControl(functions = rfFuncs, method = 'boot', repeats = 3, number = 10)
rfe3 <- rfe(x = data_FS_X, y = data[[2]], size = c(1:17), rfeControl = control)
RMSE3 <- rfe3$results[[2]]
cat('MSE의 평균 : ', mean(RMSE3^2), 'MSE값들의 표준편차 : ', sd(RMSE3^2))
boxplot(RMSE3^2)

# 너무 오래 걸림
#control <- rfeControl(functions = rfFuncs, method = 'LOOCV', repeats = 3, number = 10)
#rfe4 <- rfe(x = data_FS_X, y = data[[2]], size = c(1:17), rfeControl = control)
#RMSE4 <- rfe4$results[[2]]
#cat('MSE의 평균 : ', mean(RMSE4^2), 'MSE값들의 표준편차 : ', sd(RMSE4^2))
#boxplot(RMSE4^2)

control <- rfeControl(functions = rfFuncs, method = 'LGOCV', repeats = 3, number = 10)
rfe5 <- rfe(x = data_FS_X, y = data[[2]], size = c(1:17), rfeControl = control)
RMSE5 <- rfe5$results[[2]]
cat('MSE의 평균 : ', mean(RMSE5^2), 'MSE값들의 표준편차 : ', sd(RMSE5^2))
boxplot(RMSE5^2)

boxplot(RMSE2^2, RMSE3^2, RMSE5^2,names = c("repeatedcv","boot","LGOCV"))

# RMSE순으로 rank정렬
library(dplyr)
cell27 <- cbind(variables, RMSE5, rank(RMSE5))
colnames(cell27) <- c('variables', 'RMSE', 'rank')
cell27 <- data.frame(cell27)
cell27$rank <- as.numeric(cell27$rank)
cell27 <- cell27 %>% arrange(rank)
cell27
# 후진제거법은 RMSE순으로 rank를 정렬한것은 아닌듯

# 너무 오래 걸림
#MSE <- RMSE2^2
#score <- c()
#for (i in 3:length(data_FS_x)){
#  result <- rfe(x = data_FS_x, y = data[[2]], size = c(3:i), rfeControl = control)
#  RMSE_result <- result$results[[2]]
#  MSE_result <- RMSE_result^2
#  score <- c(score, MSE_result)
#}

#names <- c(3:length(data_FS_x))
#df <- cbind(names, MSE_result)
#boxplot(MSE_result ~ names, data = df, border = 'blue', col = 'skyblue')
#score







