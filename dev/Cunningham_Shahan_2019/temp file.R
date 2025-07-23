library(tidyverse)
source("dev/Cunningham_Shahan_2019/fit_SiGN.R")

cs_2019 <- read.csv("C:/Users/piskl/Desktop/Projects/SiGN/dev/Cunningham_Shahan_2019/gen_data/Cunningham_&_Shahan_2019.csv")

cs_2019 <- filter(cs_2019, subject == "K30")

params <- do.call(choice_params, as.list(cs_2019[3:18]))


#######
mod <- fit_SiGN(params, cs_2019$cp, b = 1, k_r = 2, k_d = 0.05)
df <- mod$details



df$subject <- cs_2019$subject
df$tl_dur <- cs_2019$tl_dur_a1

df <- pivot_longer(df,
  cols = c(cp_obs, cp_fit),
  names_to = "cp_type",
  values_to = "cp"
)

df$cp_type <- factor(df$cp_type,
  levels = c("cp_obs", "cp_fit"),
  labels = c("Observed", "Predicted")
)


# Plot
ggplot(df, aes(x = tl_dur, y = cp)) +
  geom_line(aes(colour = cp_type, linetype = cp_type)) +
  geom_point(aes(colour = cp_type, shape = cp_type),
             size = 2
  ) +
  scale_colour_manual(values = palette.colors(n = 2, palette = "R4")) +
  #facet_wrap(~subject, nrow = 2) +
  coord_cartesian(xlim = c(0, 50)) +
  labs(
    x = "Terminal Link Duration (s)",
    y = "Suboptimal Choice Proportion",
    colour = " ", shape = " ", linetype = " "
  ) +
  theme(legend.position = "top")
