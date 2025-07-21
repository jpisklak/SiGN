library(tidyverse)
files <- list.files(path = "dev/Cunningham_Shahan_2019/digi_data",
                    pattern = "\\.csv$", full.names = TRUE)

data <- files |>
  map_df(~ suppressMessages(read_csv(., ))) |>
  rename(cp = "y")

data$cp[data$cp < 0] <- 0
data$cp[data$cp > 1] <- 1
data$cp <- round(data$cp, 7)
1 -> data$il_dur_a -> data$il_dur_b
data$x -> data$tl_dur_a1 -> data$tl_dur_a2 -> data$tl_dur_b1 -> data$tl_dur_b2
data$tl_p_a1 <- 0.2
data$tl_p_a2 <- 0.8
data$tl_p_b1 <- 0.2
data$tl_p_b2 <- 0.8
data$tr_p_a1 <- 1
data$tr_p_a2 <- 0
0.5 -> data$tr_p_b1 -> data$tr_p_b2
"FR" -> data$il_sched_a -> data$il_sched_b
"FT" -> data$tl_sched_a1 -> data$tl_sched_a2 -> data$tl_sched_b1 -> data$tl_sched_b2

data <- data |>
  select(subject, cp, il_dur_a : tl_sched_b2)

write_csv(data, "dev/Cunningham_Shahan_2019/gen_data/Cunningham_&_Shahan_2019.csv")

