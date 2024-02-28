
library(data.table)
library(dplyr)
library(ggplot2)

mod<-fread("C:/Users/MatJa/OneDrive/Documents/MEGA/uni/Modelling/Data/Task-Switching Experiment Data.csv")

mod <- mod %>%
  mutate(switchcost = ifelse(task_transition =='switch', rt - lag(rt), NA))



# Comparing rt in congruent in incongruent condition
ggplot(mod, aes(x = rt, fill = congruency)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") + # Adjust this based on your data
  labs(title = "Reaction Times for Congruency", x = "Reaction Time", y = "Count", fill = "Congruency") +
  theme_minimal() #overlapping distribution

    #comparing means additionally

mean_rt_congruency <- mod %>%
  group_by(congruency) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE))

ggplot(mean_rt_congruency, aes(x = congruency, y = mean_rt, fill = congruency)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Reaction Times by Congruency",
       x = "Congruency",
       y = "Mean Reaction Time") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# ttest
congruent_data <- mod %>% filter(congruency == 'congruent') %>% pull(rt)
incongruent_data <- mod %>% filter(congruency == 'incongruent') %>% pull(rt)
t_test_result <- t.test(congruent_data, incongruent_data)

print(t_test_result) #t = -2.6145, df = 4796.8, p-value = 0.008963




# Comparing rt in switch or repetition condition

mod_filtered <- mod %>% 
  filter(task_transition %in% c('switch', 'repetition'))

ggplot(mod_filtered, aes(x = rt, fill = task_transition)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(title = "Reaction Times for Task Transition", x = "Reaction Time", y = "Count", fill = "Task Transition") +
  theme_minimal()

    #comparing means additionally

mean_rt_transition <- mod_filtered %>%
  group_by(task_transition) %>%
  summarize(mean_rt = mean(rt, na.rm = TRUE))

ggplot(mean_rt_transition, aes(x = task_transition, y = mean_rt, fill = task_transition)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Reaction Times by transition",
       x = "Transition",
       y = "Mean Reaction Time") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

# ttest
switch_data <- mod_filtered %>% filter(task_transition == 'switch') %>% pull(rt)
repetition_data <- mod_filtered %>% filter(task_transition == 'repetition') %>% pull(rt)
t_test_result_transition <- t.test(switch_data, repetition_data)
print(t_test_result_transition)#t = 1.1418, df = 4723.8, p-value = 0.2536


