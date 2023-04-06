# 01. 만족도 지수 계산

# 목차 #
# 1. 기본 package 설정
# 2. 데이터 불러오기
# 3. 100점 만점으로 변환
# 4. 상관계수 추출
# 5. 종합만족도 계산



# 1. 기본 package 설정
library(tidyverse)
library(tidymodels)
library(rstatix)
library(skimr)

# 코드 분석 #
# tidyverse: 데이터 분석과 시각화를 위한 여러 R 패키지를 모아놓은 패키지
# dplyr, ggplot2, tidyr, purrr, stringr 등을 포함하고 있으며, 데이터 전처리, 조작, 시각화 등에 사용

# tidymodels: tidyverse와 마찬가지로 데이터 분석에 필요한 R 패키지를 모아놓은 패키지
# parsnip, recipes, rsample, dials, yardstick 등이 있으며, 머신러닝 모델 제작, 평가, 비교에 사용

# rstatix: 데이터 분석을 위한 통계 분석 도구를 제공하는 패키지
# 데이터 분석 결과 요약, 가설 검정, ANOVA, 회귀 분석, 상관 분석 등의 통계 분석에 사용

# skimr: 데이터셋의 요약 통계 정보를 제공하는 패키지
# 데이터셋의 변수 유형, 결측치 비율, 이상치 정보 등을 확인하고, 데이터셋의 각 변수별 분포와 기초 통계 정보를 시각화하여 제공



# 2. 데이터 불러오기
csi_tb <-read_csv("csi.csv",
                  col_names = T,
                  locale = locale("ko", encoding = "euc-kr"),
                  na = ".") %>% # 파이프라인
  mutate(학부 = factor(학부,
                       levels = c(1:5),
                       labels = c("신학", "사회복지", "아동상담",
                                  "경영", "외국어")),
         학년 = factor(학년,
                        levels = c(1:4),
                        labels = c("1학년", "2학년", "3학년", "4학년")),
         성별 = factor(성별,
                        levels = c(1:2),
                        labels = c("남자", "여자")
                       )
         )
str(csi_tb) #데이터 프레임 구조 출력
csi_tb # 데이터 프레임 확인

# 코드 분석 #
# "csi.csv" 파일을 읽어들임
# col_names 옵션을 True로 설정하여 열 이름을 사용
# locale 옵션을 "ko"와 "euc-kr"로 설정하여 한글 인코딩 방식 채택
# na 옵션을 "."으로 설정하여 결측치를 표시

# dplyr 패키지 mutate 함수를 사용하여 데이터 프레임의 열을 변형
# 학부, 학년, 성별 열은 factor 클래스로 변환
# 각각의 factor 클래스를 정의, levels과 labels를 사용하여 각각의 값을 설정

# 파이프라인이란? (%>% 기호)
# 이전 코드의 결과를 다음 함수의 첫 번째 인자로 전달하는 역할
# 왼쪽에 있는 값(데이터)을 오른쪽에 있는 함수에 입력하고, 그 결과를 다시 다음 함수의 입력으로 전달
# 파이프라인 기능은 함수를 연속적으로 사용하는 것과 유사하며, 가독성을 높이고 간단하게 코드를 작성하게 함



# 3. 100점 만점으로 변환
csi_100_tb <- csi_tb %>%
  rowwise() %>%
  transmute(학부, 학년, 성별,
            전공교육 = (mean(c(A1, A2, A3, A4, A5))-1) / 6 * 100,
            교양교육 = (mean(c(B1,B2,B3,B4,B5))-1) / 6 * 100,
            학습지원 = (mean(c(C1,C2,C3,C4,C5))-1) / 6 * 100,
            상담지원 = (mean(c(D1,D2,D3,D4,D5))-1) / 6 * 100,
            전반적만족도 = (mean(c(E1,E2,E3,E4,E5))-1) / 6 * 100,
            )
as_tibble()

csi_100_tb
str(csi_100_tb)

# 코드 분석 #
# csi_tb 데이터셋에서 각 항목에 대한 평균값을 100점 만점으로 변환해 csi_100_tb라는 새로운 데이터셋을 만드는 작업

# transmute: 데이터셋의 변수를 변환하는 함수로, 새로운 변수를 추가하거나 기존 변수를 수정
# rowwise: '행단위 연산'일때 그룹화를 위해 사용하며, 변수를 행 단위로 계산
# tibble: mutate계산시 list로 생성된 부분을 삭제하여 더욱 직관적이게 함

# 방법2
# csi_tb %>%
#  select(A1:A5) %>%
#  mutate(A1 = (A1-1)/6*100,
#         A2 = (A2-1)/6*100,
#         A3 = (A3-1)/6*100,
#         A4 = (A4-1)/6*100,
#         A5 = (A5-1)/6*100) %>%
#  rowwise() %>%
#  mutate(전공교육 = mean(c(A1,A2,A3,A4,A5)))



# 4. 상관계수 추출
cor_tb <- csi_100_tb %>%
  cor_test(var1 = 전반적만족도, # 기준점을 정해준다. 전반적 만족도가 기준이 된다.
           method = "pearson") %>%
  select(var2, cor) %>%
  mutate(wgt = cor/sum(cor)) # 상관관계로 가중치 구하기 (모두 더하고 1/n해준다)

cor_tb #wgt 가중치를 이용해서 종합만족도를 계산할 예정

# 코드 분석 #
# tibble 형태로 변환된 데이터셋 csi_100_tb에서 cor_test() 함수를 이용하여 변수들 간의 상관관계를 계산하고,
# select() 함수와 mutate() 함수를 이용하여 필요한 변수와 가중치 값을 계산

# cor_text: 변수들 간의 상관계수와 유의성을 계산
# var1 인자를 통해 기준 변수를 지정할 수 있으며, 전반적만족도를 기준 변수로 설정
# method 인자를 통해 상관계수 계산 방법을 지정할 수 있으며, pearson 방법을 사용

# select: 데이터셋에서 필요한 열(columns)을 선택하는 함수
# mutate: 데이터셋에 새로운 열(columns)을 추가하거나 기존 열을 수정하는 함수
# mutate(종합만족도 = 전공교육 * 0.237 + 교양교육 * 0.247) 가중치 부여



# 5. 종합만족도 계산
csi_total_tb <- csi_100_tb %>%
  select(학부:상담지원) %>%
  rowwise() %>%
  mutate(종합만족도 = 전공교육 * (cor_tb %>%
                                    filter(var2 == "전공교육") %>%
                                    select(wgt)) +
                      교양교육 * (cor_tb %>%
                                    filter(var2 == "교양교육") %>%
                                    select(wgt)) +
                      학습지원 * (cor_tb %>%
                                    filter(var2 == "학습지원") %>%
                                    select(wgt)) +
                      상담지원 * (cor_tb %>%
                                    filter(var2 == "상담지원") %>%
                                    select(wgt))
        )

str(csi_total_tb)

# 종합만족도 점수를 dbl로 처리
csi_total_tb <- csi_total_tb %>%
  unnest(종합만족도) %>%
  rename(종합만족도 = wgt)
str(csi_total_tb)

# 코드 분석 #
# csi_100_tb 데이터셋 변수들을 이용하여 종합만족도를 계산
# select() 함수를 이용하여 학부부터 상담지원 변수까지 선택
# rowwise() 함수를 이용하여 행별로 계산
# mutate() 함수를 이용하여 종합만족도 변수 계산
# filter(), select() 함수를 이용하여 해당 변수와 가중치를 선택한 후, * 연산자로 가중합을 계산
# 계산 결과는 unnest() 함수와 rename() 함수를 이용하여 종합만족도만 남긴 후, dbl 형식으로 처리

# 방법2: 상관계수값으로 직접 계산
# csi_100_tb %>%
#  select(전공교육:상담지원) %>%
#  mutate(종합만족도 = (전공교육 * 0.237) + 
#                      (교양교육 * 0.242) +
#                      (학습지원 * 0.292) + 
#                      (상담지원 * 0.228)
#        )