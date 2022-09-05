library(readxl)
data = read_excel('mydata.xlsx') # data 불러오기
head(data)
names(data) # column 이름 출력

data[['count', 'age65up', 'Delta_count', 'Omicron_count',
      'Delta_prop', 'Omicron_prop', 'seoul_retail_recreation',
      'seoul_grocery_pharmacy', 'seoul_parks', 'seoul_transit_stations',
      'seoul_workplaces', 'seoul_residential', 'metro_retail_recreation',
      'metro_grocery_pharmacy', 'metro_parks', 'metro_transit_stations',
      'metro_workplaces', 'metro_residential', 'death']]

library(dplyr)
# 특정 열만 추출
datacorr_cases=data %>% select('count', 'age65up', 'Delta_count', 'Omicron_count',
                               'Delta_prop', 'Omicron_prop', 'seoul_retail_recreation',
                               'seoul_grocery_pharmacy', 'seoul_parks', 'seoul_transit_stations',
                               'seoul_workplaces', 'seoul_residential', 'metro_retail_recreation',
                               'metro_grocery_pharmacy', 'metro_parks', 'metro_transit_stations',
                               'metro_workplaces', 'metro_residential', 'death')
datacorr_cases
ncol(datacorr_cases)
k=ncol(datacorr_cases)

corrmat=cor(datacorr_cases) # 상관관계 분석
corrmat
#install.packages('ggcorrplot')
library(ggcorrplot)
ggcorrplot(corrmat) # 히트맵
