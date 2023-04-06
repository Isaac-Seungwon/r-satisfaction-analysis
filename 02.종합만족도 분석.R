# 02.종합만족도 분석

# 1.기본 package 설정
library(tidyverse)
library(tidymodels)
library(rstatix)
library(skimr)


# 2.응답자 특성

# 학부
csi_total_tb %>%
  freq_table(학부) %>%
  write_excel_csv("01.응답자_학부.xls", col_names = T)

# 학년
csi_total_tb %>%
  freq_table(학년) %>%
  write_excel_csv("01.응답자_학년.xls", col_names = T)

# 성별
csi_total_tb %>%
  freq_table(성별) %>%
  write_excel_csv("01.응답자_성별.xls", col_names = T)


# 3. 차원별 만족도 분석
csi_total_tb %>%
  get_summary_stats(종합만족도, 전공교육, 교양교육, 학습지원, 상담지원,
                    show = c("mean")) %>%
  select(variable, mean) %>%
  write_excel_csv("02.차원별 종합만족도.xls",
                  col_names = T)


# 4. 학과별 만족도

# 학과별 종합만족도
csi_total_tb %>%
  group_by(학부) %>%
  get_summary_stats(종합만족도,
                    show = c("mean")) %>%
  select(학부, mean) %>%
  write_excel_csv("03.학과별 종합만족도.xls",
                  col_names = T)

# 차원별 종합만족도
csi_total_tb %>%
  group_by(학부) %>%
  get_summary_stats(show = c("mean")) %>%
  select(학부, variable, mean) %>%
  pivot_wider(names_from = 학부,
              values_from = mean) %>%
  write_excel_csv("03.학과별 차원별 만족도.xls",
                  col_names = T)

#longer와 wider형의 차이를 알아둔다.