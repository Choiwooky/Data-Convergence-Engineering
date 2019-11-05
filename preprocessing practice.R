
##### 신재생에너지 전처리 사전 mission #####

# read_excel, read_xls, read_xlsx 함수를 사용하기 위한 패키지 설치
if(!require(readxl)){
  install.packages("readxl")
}
library(readxl)

# JAVA가 설치되어 있는 경로를 컴퓨터에서 확인하고 R에 작성
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_211")

# rJava를 설치하지 않으면 xlsx패키지 실행이 안됨
if(!require(rJave)){
  install.packages("rJava")
}
library(rJava)

# write.xlsx 함수를 사용하기 위한 패키지 설치

if(!require(xlsx)){
  install.packages("xlsx")
}
library(xlsx)


# .xlsx파일
# .xlsx 확장자로 sample data 여러개 생성
for(i in 1:5){
  mypath0 <- paste0('C:/Users/whddnr/Desktop/rawdata_cjw/SampleData',i,'.xlsx')
  # assgin 함수와 paste0 동시 사용
  assign(paste0('sampledata',i,'_x'), read_xlsx(mypath0, sheet = 'SalesOrders'))
}

# .csv파일
# .csv 확장자로 sample data 여러개 생성
for(i in 1:5){
  mypath1 <-paste0('C:/Users/whddnr/Desktop/rawdata_cjw/SampleData',i,'_c.csv')
  assign(paste0('sampledata', i, '_c'), read.csv(mypath1))
}

# 여러개의 데이터에대해 모델을 적합하고
# 적합한 모델의 beta 계수만을 모아 따로 파일화

datasize <- 5

for(i in 1:datasize){
  # 데이터 사이즈 만큼 반복
  
  mypath1 <- paste0('C:/Users/whddnr/Desktop/rawdata_cjw/crabdata',i,'.csv')
  # rawdata_cjw폴더에 있는 crabdata1~5를 불러옴
  
  assign('crab', read.csv(mypath1))
  crab$y <- 1*(crab$satellite != 0)
  # 반응변수 y를 부수체의 유무로 설정
  crab$fcolor <- factor(crab$color)
  # 색깔변수를 color로 설정
  fit <- glm(y ~ width + fcolor, data = crab, family = "binomial")
  # glm을 통해 로짓모형 적합 
  fit_s <- summary(fit)
  # fit_s에 summary()를 사용하여 결과물 저장
  mypath2 <- paste0('C:/Users/whddnr/Desktop/output/')
  # output 폴더로 가는 path
  capture.output(fit_s, file = paste0(mypath2,'output',i,'.txt'))
  # 결과를 텍스트 파일화해서 저장
  fit_coef <- fit_s$coefficients
  # beta값을 알 수 있는 coefficients를 할당
  fit_coef <- as.data.frame(fit_coef)
  # 데이터프레임화 시켜줌
  mybeta <- matrix(ncol = length(fit_coef$`Std. Error`), nrow = datasize)
  # 각 분석 결과의 베타값만을 저장하기 위한 빈 행렬 생성
  for(j in 1:datasize){
    for(k in 1:length(fit_coef$`Std. Error`)){
      mybeta[j,k] <- fit_coef$`Std. Error`[k]
    }
  }
  # beta값만을 모은 행렬 생성 
}


cn <- c()
for(i in 1:ncol(mybeta)){
  cn[i] <- paste0('beta',i-1)
}

cn

colnames(mybeta) <- cn
# 열 이름 설정

write.csv(mybeta, file = 'C:/Users/whddnr/Desktop/beta/mybeta_c.csv')
write.xlsx(mybeta, file = 'C:/Users/whddnr/Desktop/beta/mybeta_x.xlsx')
