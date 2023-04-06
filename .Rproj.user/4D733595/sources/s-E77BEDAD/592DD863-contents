# 03.IPA 분석석

# 1.IPA용 데이터로 정리

# 만족도
ipa_1_tb <- csi_total_tb %>%
  get_summary_stats(종합만족도, 전공교육, 교양교육, 학습지원, 상담지원,
                    show = c("mean")) %>%
  select(variable, mean) %>%
  filter(!(variable == "종합만족도")) %>%
  rename(항목 = variable,
          만족도 = mean)

# 중요도
ipa_2_tb <- cor_tb %>%
  arrange(var2) %>%
  select(wgt) %>%
  rename(중요도 = wgt)

ipa_tb <-cbind(ipa_1_tb, ipa_2_tb)

ipa_tb

# 2.그래프 그리기
ipa_tb %>%
  ggplot(mapping = aes(x = 중요도,
                       y = 만족도)) +
  geom_point(shape = 18,
             size = 3,
             color = "red",
             show.legend = F) +
  coord_cartesian(xlim = c(0.1, 0.4),
                  ylim = c(52.5, 72.5)) +
  geom_text(mapping = aes(label = 항목, #전공교육, 학습지원, 교양교육, 상담지원
                          size = 1,
                          vjust = -1,
                          hjust = 0),
            show.legend = F) +
  geom_vline(xintercept = 0.25,
             size = 0.5,
             alpha = 0.5) +
  geom_hline(yintercept = 62.5,
             size = 0.5,
             alpha = 0.5)
