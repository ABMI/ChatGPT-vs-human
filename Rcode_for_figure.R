library(readxl)
library(dplyr)
library(ggplot2)

cpx_doctor <- read.csv('~/cpx_from_expert.csv')
cpx_patient <- read_excel('~/cpx_from_patient.xlsx')
writing_result <- read.csv('~/wt_ex_expert.csv')

# 난이도, 전체 문제 plot 
writing_result <- writing_result %>% mutate(score_sum = Q1 + Q2 + Q3)

writing_result <- writing_result %>%
  mutate(
    number = case_when(
      set == 1 & case_num == 1 ~ 1,
      set == 1 & case_num == 2 ~ 2,
      set == 1 & case_num == 3 ~ 3,
      set == 1 & case_num == 4 ~ 4,
      set == 1 & case_num == 5 ~ 5,
      set == 1 & case_num == 6 ~ 6,
      set == 1 & case_num == 7 ~ 7,
      set == 2 & case_num == 1 ~ 8,
      set == 2 & case_num == 2 ~ 9,
      set == 2 & case_num == 3 ~ 10,
      set == 2 & case_num == 4 ~ 11,
      set == 2 & case_num == 5 ~ 12,
      set == 2 & case_num == 6 ~ 13,
      set == 2 & case_num == 7 ~ 14,
      set == 3 & case_num == 1 ~ 15,
      set == 3 & case_num == 2 ~ 16,
      set == 3 & case_num == 3 ~ 17,
      set == 3 & case_num == 4 ~ 18,
      set == 3 & case_num == 5 ~ 19,
      set == 3 & case_num == 6 ~ 20,
      set == 3 & case_num == 7 ~ 21,
      set == 4 & case_num == 1 ~ 22,
      set == 4 & case_num == 2 ~ 23,
      set == 4 & case_num == 3 ~ 24,
      set == 4 & case_num == 4 ~ 25,
      set == 4 & case_num == 5 ~ 26,
      set == 4 & case_num == 6 ~ 27,
      set == 4 & case_num == 7 ~ 28,
      set == 5 & case_num == 8 ~ 29,
      set == 5 & case_num == 9 ~ 30,
      set == 5 & case_num == 10 ~ 31,
      set == 5 & case_num == 11 ~ 32,
      set == 5 & case_num == 1 ~ 29,
      set == 5 & case_num == 2 ~ 30,
      set == 5 & case_num == 3 ~ 31,
      set == 5 & case_num == 4 ~ 32
    )
  )

chat_writing <- writing_result %>% filter(class == 'AI')
doctor_writing <- writing_result %>% filter(class == 'doctor')

doctor_results <- doctor_writing %>%
  group_by(number) %>%
  summarise(
    average_score = mean(score_sum)
  )

doctor_results <- data.frame(doctor_results)

chat_results <- chat_writing %>%
  group_by(number) %>%
  summarise(
    average_score = mean(score_sum)
  )

chat_results <- data.frame(chat_results)

#############

results <- merge(doctor_results, chat_results, by = 'number')
colnames(results) <- c('number', 'doctor_score', 'ChatGPT_score')

scores_df <- results %>%
  mutate(difference = ChatGPT_score - doctor_score)

# ggplot(scores_df, aes(x = reorder(number, -doctor_score))) +
#   geom_bar(aes(y = doctor_score, fill = "Doctor Score"), stat = "identity", width = 0.7) +
#   geom_point(aes(y = ChatGPT_score, color = "ChatGPT Score"), size = 3) +
#   geom_point(aes(y = difference, color = "Score Difference"), size = 3) +
#   geom_line(aes(y = difference, group = 1, color = "Score Difference"), size = 1) +
#   scale_fill_manual(values = c("Doctor Score" = "lightgray")) +
#   scale_color_manual(values = c("ChatGPT Score" = "orange", "Score Difference" = "red")) +
#   labs(x = "Case Number", y = "Score", title = "Score Differences between Doctor and ChatGPT") +
#   theme_minimal() +
#   theme(legend.title = element_blank())


ggplot(scores_df, aes(x = reorder(number, -doctor_score))) +
  geom_bar(aes(y = doctor_score, fill = "Doctor Score"), stat = "identity", width = 0.7) +
  geom_point(aes(y = ChatGPT_score, color = "ChatGPT Score"), size = 3) +
  geom_point(aes(y = difference, color = "Score Difference"), size = 3) +
  geom_line(aes(y = difference, group = 1, color = "Score Difference"), size = 1) +
  geom_hline(yintercept = 9, linetype = "dashed", color = "blue") +
  geom_text(aes(y = 9, label = "Max score", x = Inf), vjust = -1, hjust = 1.1, color = "blue") + # 'max' 라벨 추가
  scale_fill_manual(values = c("Doctor Score" = "lightgray")) +
  scale_color_manual(values = c("ChatGPT Score" = "orange", "Score Difference" = "red")) +
  labs(x = "Case Number", y = "Score", title = "Score Differences between Doctor and ChatGPT") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  ylim(0, 20)  # y축 범위 조정
############

one_third <- quantile(scores_df$doctor_score, 1/3)
two_thirds <- quantile(scores_df$doctor_score, 2/3)

scores_df <- scores_df %>% mutate(difficulty = ifelse(doctor_score < one_third, 'hard', ifelse(doctor_score <= two_thirds, 'medium', 'easy')))

scores_summary <- scores_df %>%
  group_by(difficulty = factor(difficulty, levels = c("easy", "medium", "hard"))) %>%
  summarise(
    doctor_mean = mean(doctor_score),
    doctor_se = sd(doctor_score) / sqrt(n()),
    ChatGPT_mean = mean(ChatGPT_score),
    ChatGPT_se = sd(ChatGPT_score) / sqrt(n())
  ) %>%
  mutate(
    doctor_ci_upper = doctor_mean + qt(0.975, df = n() - 1) * doctor_se,
    doctor_ci_lower = doctor_mean - qt(0.975, df = n() - 1) * doctor_se,
    ChatGPT_ci_upper = ChatGPT_mean + qt(0.975, df = n() - 1) * ChatGPT_se,
    ChatGPT_ci_lower = ChatGPT_mean - qt(0.975, df = n() - 1) * ChatGPT_se
  )
  
table(scores_df$difficulty)
easy_df <- scores_df %>% filter(difficulty == "easy")
medium_df <- scores_df %>% filter(difficulty == "medium")
hard_df <- scores_df %>% filter(difficulty == "hard")
t.test(easy_df$ChatGPT_score, easy_df$doctor_score)

# 그래프 그리기
ggplot(scores_summary, aes(x = difficulty)) +
  geom_line(aes(y = doctor_mean, color = "Doctor"), size = 1) +
  geom_point(aes(y = doctor_mean, color = "Doctor"), size = 3) +
  geom_line(aes(y = ChatGPT_mean, color = "ChatGPT"), size = 1) +
  geom_point(aes(y = ChatGPT_mean, color = "ChatGPT"), size = 3) +
  geom_errorbar(aes(ymin = doctor_ci_lower, ymax = doctor_ci_upper, x = difficulty), 
                width = 0.08, color = "blue") +
  geom_errorbar(aes(ymin = ChatGPT_ci_lower, ymax = ChatGPT_ci_upper, x = difficulty), 
                width = 0.08, color = "orange") +
  scale_color_manual(values = c("Doctor" = "blue", "ChatGPT" = "orange")) +
  labs(x = "Difficulty", y = "Score", title = "Comparison of Scores by Difficulty") +
  theme_minimal() +
  theme(legend.title = element_blank(), panel.grid.major.x = element_blank())

