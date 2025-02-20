library(data.table)
library(ggplot2)
library(MetBrewer)

load("logs.Rdata")
head(logs_all)
logs_all <- as.data.table(logs_all)
logs_all <- logs_all[order(user_id, created)]


unique(logs_all$answer)
unique(logs_all$question_mark)
logs_all[question_mark == 1, unique(correct_answered)]
logs_all[question_mark == 1, correct_answered := NA]

logs_all[, uniqueN(user_id)]
logs_all[, n_count := .N, user_id]
hist(logs_all$n_count, breaks = 200)

logs_all[, uniqueN(item_id)]
logs_all[, n_items := uniqueN(item_id), user_id]
hist(logs_all$n_items, breaks = 200)

logs_all[, mean(question_mark)]
hist(logs_all[, .(avg_qm = mean(question_mark)), user_id]$avg_qm, breaks = 100)
difficulty <- logs_all[, .(avg_qm = mean(question_mark), 
                           ci_l = t.test(question_mark)$conf.int[1], 
                           ci_u = t.test(question_mark)$conf.int[2]), difficulty]
ggplot(difficulty, aes(x = difficulty, y = avg_qm)) +
  geom_bar(stat = "identity", fill = "gray80", color = "gray20") +
  geom_linerange(aes(ymin = ci_l, ymax = ci_u), linewidth = 1, color = "gray20") +
  theme_bw() +
  theme(legend.position = "none")


difficulty_user <- logs_all[, .(avg_qm = mean(question_mark)), .(user_id, difficulty)]
ggplot(difficulty_user, aes(x = factor(difficulty), y = avg_qm, group = factor(difficulty))) +
  geom_boxplot(outliers = FALSE, alpha = 0.4, fill = "gray80", color = "gray20") +
  theme_bw()
  
  


hist(logs_all[, .(avg_qm = mean(question_mark)), item_id]$avg_qm, breaks = 100)

item_ratings <- logs_all[, .(avg_rat = mean(item_rating), avg_qm = mean(question_mark), n = .N), .(difficulty, item_id)]
plot(item_ratings$avg_rat, item_ratings$avg_qm)
ggplot(item_ratings[n >= 50], aes(x = avg_rat, y = avg_qm)) +
  geom_point(aes(size = n)) +
  #geom_smooth() +
  facet_wrap(~difficulty)
cor.test(item_ratings$avg_rat, item_ratings$avg_qm)

item_ratings <- item_ratings[n >= 50]
cor.test(item_ratings[difficulty == 0]$avg_rat, item_ratings[difficulty == 0]$avg_qm)
cor.test(item_ratings[difficulty == 1]$avg_rat, item_ratings[difficulty == 1]$avg_qm)
cor.test(item_ratings[difficulty == 2]$avg_rat, item_ratings[difficulty == 2]$avg_qm)

user_ratings <- logs_all[, .(avg_rat = mean(user_domain_rating), avg_qm = mean(question_mark)), .(user_id, n_count)]
plot(user_ratings$avg_rat, user_ratings$avg_qm)
ggplot(user_ratings, aes(x = avg_rat, y = avg_qm)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  theme_bw()
cor.test(user_ratings$avg_rat, user_ratings$avg_qm)
cor.test(user_ratings[n_count >= 50]$avg_rat, user_ratings[n_count >= 50]$avg_qm)


min_sessions <- 50
logs_all[, session_count := rleid(created), user_id]
logs_all[, new_user := ifelse(1 %in% new_user_domain_modified_count, 1, 0), user_id]
sessions <- logs_all[session_count <= min_sessions, .(avg_qm = mean(question_mark), ci_l = t.test(question_mark)$conf.int[1], ci_u = t.test(question_mark)$conf.int[2]), .(session_count, new_user)]

ggplot(sessions, aes(x = session_count, y = avg_qm, group = factor(new_user))) +
  geom_line(aes(linetype = factor(new_user)), linewidth = 1) +
  geom_ribbon(aes(ymax= ci_u, ymin = ci_l), alpha = 0.1) +
  theme_bw()

sessions <- logs_all[n_count >= 50 & session_count <= min_sessions, .(avg_qm = mean(question_mark), ci_l = t.test(question_mark)$conf.int[1], ci_u = t.test(question_mark)$conf.int[2]), .(session_count, new_user)]
ggplot(sessions, aes(x = factor(session_count), y = avg_qm, group = factor(new_user))) +
  geom_line(aes(linetype = factor(new_user)), linewidth = 1) +
  geom_ribbon(aes(ymax= ci_u, ymin = ci_l), alpha = 0.1) +
  theme_bw()

logs_all[, response_type := ifelse(question_mark == 1, "QM", ifelse(correct_answered == 1, "Correct", "Error"))]
ggplot(logs_all[too_late == 0], aes(x = response_in_milliseconds, color = response_type, fill = response_type)) +
  geom_density(alpha = 0.4) +
  scale_discrete_manual(aesthetics = c("color", "fill"), values = met.brewer("Archambault", 3)) +
  theme_bw()
