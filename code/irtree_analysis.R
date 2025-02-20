library(lme4)
library(ggplot2)
library(tidyverse)
library(ggExtra)
library(cowplot)

load("mod_tree.Rdata")
load("sum_mod_tree.Rdata")

## Fixed effects ----
int <- sum$coefficients[1] # fixed intercept
1 / (1 + exp(-int)) # logistic function

## Inspect random effects ----
ranefs_mod <- ranef(mod_tree)
# save(ranefs_mod, file = "ranefs_mod.Rdata")
# load("ranefs_mod.Rdata")
hist(plogis(ranefs_mod$user_id[,1]), breaks = 50, main = "Distribution of random user effects, node 1")
hist(plogis(ranefs_mod$user_id[,2]), breaks = 50,main = "Distribution of random user effects, node 2")
plot(plogis(ranefs_mod$user_id[,1]), plogis(ranefs_mod$user_id[,2]), xlab = "Random user effects, node 1", ylab = "Random user effects, node 2")

hist(plogis(ranefs_mod$item_id[,1]), breaks = 50, main = "Distribution of random item effects, node 1")
hist(plogis(ranefs_mod$item_id[,2]), breaks = 50,main = "Distribution of random item effects, node 2")
plot(plogis(ranefs_mod$item_id[,1]), plogis(ranefs_mod$item_id[,2]), xlab = "Random item effects, node 1", ylab = "Random item effects, node 2")

## Make dataframe of random effects, for plotting ----
ranef_user_node1 <- ranefs_mod$user_id[,1]
ranef_user_node2 <- ranefs_mod$user_id[,2]

ranef_item_node1 <- ranefs_mod$item_id[,1]
ranef_item_node2 <- ranefs_mod$item_id[,2]

ranefs <- data.frame(node = factor(c(rep(c(1, 2), each = length(ranef_user_node1)), rep(c(1, 2), each = length(ranef_item_node1)))),
                     estimate = c(rep("user", times = length(ranef_user_node1)*2), rep("item", times = length(ranef_item_node1)*2)),
                     id = c(rep(rownames(ranefs_mod$user_id), 2), rep(rownames(ranefs_mod$item_id), 2)),
                     # values are converted to probabilities
                     value = c(ranef_user_node1, ranef_user_node2, ranef_item_node1, ranef_item_node2))
rm(ranef_user_node1, ranef_user_node2, ranef_item_node1, ranef_item_node2)
# save(ranefs, file = "ranefs_df.Rdata")
# load("ranefs_df.Rdata")

## Plot correlation between random effects ----
cor_plot_user <- ranefs %>%
  filter(estimate == "user") %>%
  pivot_wider(names_from = node, values_from = value) %>% 
  ggplot(aes(x = `1`, y = `2`)) + 
  geom_point(color = "gray", alpha = 0.3) +
  geom_smooth(method = "lm", color = "tan2", fill = "tan2") +
  labs(x = "Node 1", y = "Node 2") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 12))

cor_plot_item <- ranefs %>%
  filter(estimate == "item") %>% 
  pivot_wider(names_from = node, values_from = value) %>% 
  ggplot(aes(x = `1`, y = `2`)) +
  geom_point(color = "gray", alpha = 0.3) +
  geom_smooth(method = "lm", color = "tomato3", fill = "tomato3") +
  labs(x = "Node 1", y = "Node 2") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(size = 12))

# add marginal dist
cor_plot_item_marginal <- ggMarginal(cor_plot_item + labs(x = "Item Skipping Threshold", y = "Item Difficulty"), 
                                     type = "density", fill = "tomato3", alpha = 0.4, color = "tomato3")
cor_plot_user_marginal <- ggMarginal(cor_plot_user + labs(x = "User Propensity to Skip an Item", y = "User Propensity for Incorrect Response"), 
                                     type = "density", fill = "tan2", alpha = 0.4, color = "tan2")

combined_plot <- plot_grid(cor_plot_item_marginal, 
                           cor_plot_user_marginal, 
                           ncol = 2)
rm(cor_plot_item, cor_plot_item_marginal, cor_plot_user, cor_plot_user_marginal)
combined_plot

## Extract individual response patterns ----
load("ranefs_mod.Rdata")
load("logs_clean.Rdata")

ind_plots <- list()

for (i in 1:4) {
  # set boundary for which areas of correlation plot to sample from 
  # upper right, lower right, lower left, upper left
  # for each extract plot, change values from 1 to 4
  min_node1 = c(1, 1, -4, -4)[i]
  max_node1 = c(6, 6, -1, 0)[i]
  min_node2 = c(1.5, -2, -2, 1)[i]
  max_node2 = c(4, 0, 0, 4)[i]
  
  set.seed(121103961)
  users <- sample(as.numeric(rownames(ranefs_mod$user_id[which(ranefs_mod$user_id$`factor(node)1` > min_node1 &
                                                                 ranefs_mod$user_id$`factor(node)1` < max_node1 & 
                                                                 ranefs_mod$user_id$`factor(node)2` > min_node2 & 
                                                                 ranefs_mod$user_id$`factor(node)2` < max_node2),])), 2)
  
  dat_sub <- logs_clean[logs_clean$user_id%in% users,]
  dat_sub$response_new <- ifelse(dat_sub$q == 1, "Question Mark", ifelse(dat_sub$correct_answered == 0, "Error", "Correct"))
  dat_sub <- dat_sub[order(dat_sub$id),]
  dat_sub$item_number <- 1:nrow(dat_sub)
  
  custom_labels <- switch(
    i,
    setNames(paste(rev(LETTERS[1:2]), ": Low Accuracy, High Skipping"), unique(dat_sub$user_id)),
    setNames(paste(rev(LETTERS[3:4]), ": High Accuracy, High Skipping"), unique(dat_sub$user_id)),
    setNames(paste(rev(LETTERS[5:6]), ": High Accuracy, Low Skipping"), unique(dat_sub$user_id)),
    setNames(paste(rev(LETTERS[7:8]), ": Low Accuracy, Low Skipping"), unique(dat_sub$user_id))
  )
  
  p <- dat_sub %>%
    arrange(id) %>%
    mutate(count = row_number()) %>%
    filter(count < 400) %>% 
    ggplot(aes(x = factor(item_number), y = rt, color = response_new)) + 
    geom_path(group = 1, color = "black", alpha = 0.3) + 
    geom_point() + 
    labs(x = "Item count", y = "Response Time", color = "Response") +
    scale_color_manual(values = c("Correct" = "gray40", "Error" = "tomato3", "Question Mark" = "tan1")) +
    facet_wrap(~user_id, nrow = 10, scales = "free", labeller = labeller(user_id = custom_labels)) + 
    scale_x_discrete(breaks = function(x) x[seq(1, length(x), by = 20)]) +  # Show only every 5th x-axis label
    theme_bw()
  
  ind_plots[[paste0("plot_", i)]] <- p
}

plot_users <- plot_grid(ind_plots$plot_4 + theme(legend.position = "none"), ind_plots$plot_1 + theme(legend.position = "none") , ind_plots$plot_3 + theme(legend.position = "none"), ind_plots$plot_2 + theme(legend.position = "none"), ncol = 2)


