
library(tidyverse)
library(palmerpenguins)

data("penguins")
head(penguins)
str(penguins)

penguins %>% 
  select(
    species,
    sex,
    bill_length_mm, 
    bill_depth_mm, 
    flipper_length_mm, 
    body_mass_g
  ) %>% 
  pivot_longer(
    col = where(is.numeric),
    names_to = "variable", 
    values_to = "value"
  ) %>% 
  filter(complete.cases(.)) %>% 
  ggplot(mapping = aes(x = sex, y = value)) +
  geom_jitter(aes(color = sex)) +
  facet_grid(variable ~ species, scales = "free_y", switch = "y") +
  labs(y = NULL) +
  guides(color = "none") +
  theme_minimal() +
  theme(strip.placement = "outside")

adelie_bill_depth <-
  penguins %>% 
  filter(species == "Adelie") %>% 
  select(sex, bill_depth_mm) %>% 
  filter(complete.cases(.)) %>% 
  print()

adelie_bill_depth %>% 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram(
    aes(fill = sex), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

adelie_bill_depth_summary <-
  adelie_bill_depth %>% 
  group_by(sex) %>% 
  summarize(
    n = n(),
    mean = mean(bill_depth_mm),
    sd = sd(bill_depth_mm),
    sem = sd/sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

adelie_bill_depth %>% 
  ggplot(aes(x = sex, y = bill_depth_mm)) +
  geom_jitter(aes(color = sex), 
              shape = 16, size = 3, 
              alpha = 0.3, width = 0.4) +
  geom_errorbar(aes(y = mean, ymax = upper, ymin = lower), 
                data = adelie_bill_depth_summary, 
                width = .1, size = .8) +
  geom_point(aes(y = mean), 
             data = adelie_bill_depth_summary, 
             size = 3) +
  scale_color_manual(values = c("darkorange","cyan4")) +
  theme_minimal() +
  guides(color = "none")

t.test(formula = bill_depth_mm ~ sex, data = adelie_bill_depth)

penguins %>% 
  ggplot(aes(x = bill_depth_mm)) +
  geom_histogram(
    aes(fill = species), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4")) +
  theme_minimal()

bill_depth_means <-
  penguins %>% 
  filter(!is.na(bill_depth_mm)) %>%      # remove missing values
  group_by(species) %>% 
  summarize(
    mean = mean(bill_depth_mm),
    sd = sd(bill_depth_mm),
    n = n(),
    sem = sd / sqrt(n),
    upper = mean + 1.96 * sem,
    lower = mean - 1.96 * sem
  ) %>% 
  print()

ggplot(data = penguins, aes(x = species, y = bill_depth_mm)) +
  geom_jitter(aes(color = species),
              width = 0.1,
              alpha = 0.7,
              show.legend = FALSE,
              na.rm = TRUE) +
  geom_errorbar(aes(y = mean, ymin = lower, ymax = upper), 
                data = bill_depth_means,
                width = .1, position = position_nudge(.3)) +
  geom_point(aes(y = mean), data = bill_depth_means,
             position = position_nudge(.3)) +
  scale_color_manual(values = c("darkorange","darkorchid","cyan4"))

aov_bill_depth_species <-
  aov(bill_depth_mm ~ species, data = penguins)
aov_bill_depth_species

summary(aov_bill_depth_species)
