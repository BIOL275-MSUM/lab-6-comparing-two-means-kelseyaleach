
# load packages -----------------------------------------------------------

library(tidyverse)


# read data ---------------------------------------------------------------

fish <- read_csv("chap12q19ElectricFish.csv")

fish_long <- 
  pivot_longer(fish, speciesUpstream:speciesDownstream,
               names_to = "location",
               values_to = "species") %>% 
  mutate(location = str_remove(location, c("species"))) %>% 
  print()

# do stuff ----------------------------------------------------------------


#Performing a t-test
t.test(formula = species ~ location, data = fish_long)


#Creating a graph to check if the distribution is normal
fish_long %>% 
  ggplot(aes(x = species)) +
  geom_histogram(
    aes(fill = location), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity"
  ) +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()



# CRABS Read data ---------------------------------------------------------

crab <- read_csv("chap15q27FiddlerCrabFans.csv")

crab %>% 
  ggplot(aes(x = bodyTemperature)) +
  geom_histogram(
    aes(fill = crabType), 
    bins = 15, 
    alpha = 0.5, 
    position = "identity",
    na.rm = TRUE
  ) +
  scale_fill_manual(values = c("darkorange", "darkorchid", "cyan4", "deeppink")) +
  theme_minimal()


# ANOVA -------------------------------------------------------------------

aov_crab_claw <-
  aov(bodyTemperature ~ crabType, data = crab)
aov_crab_claw
summary(aov_crab_claw)

