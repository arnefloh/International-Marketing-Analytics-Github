# Load packages ----
library(tidyverse)
library(lm.beta)
library(performance)
library(janitor)
library(rstatix)

# Info about the dataset: https://www.kaggle.com/datasets/gregorut/videogamesales

# Load data ----
raw.data <- read_csv("Data/Video_Games_Sales.csv")
my.data <- raw.data |> clean_names()


# Explore data ----
glimpse(my.data)

my.data <- my.data |> mutate(
              user_score = as.numeric(user_score),
              platform = as_factor(platform),
              developer = as_factor(developer),
              publisher = as_factor(publisher)
              )

glimpse(my.data)
summary(my.data)


# Regression analysis
r1 <- my.data |> lm(formula = global_sales ~ critic_score + user_score) 
summary(r1)
lm.beta(r1)


pred_val <- predict(r1)
res_val <- residuals(r1) 
res_val_tbl <- tibble(error = res_val)

res_val_tbl |> ggplot(aes(x = error)) + geom_histogram()

# Testing assumptions
check_model(r1)
check_collinearity(r1)
check_normality(r1)




# Regression analysis with filter
my.data |> count(developer) |> arrange(desc(n))
my.data |> count(platform) |> arrange(desc(n))
my.data |> count(genre) |> arrange(desc(n))
my.data |> count(publisher) |> arrange(desc(n))


r2 <- my.data |> filter (platform == "Wii" & genre == "Action") |> 
  lm(formula = global_sales ~ critic_score + user_score)
summary(r2)

wii_action <- my.data |> filter (platform == "Wii" & genre == "Action")
r3 <- wii_action |>lm(formula = global_sales ~ critic_score + user_score)
summary(r3)

wii_action |> ggplot(aes(x = critic_score, y = user_score)) +
                       geom_point() +
                       geom_smooth(se = FALSE)
wii_action |> cor_test(critic_score, user_score)                     

r4 <- wii_action |> lm(formula = global_sales ~ user_score)                     
summary(r4)

check_model(r4)

r4_log <- wii_action |> lm(formula = log(global_sales) ~ user_score)
summary(r4_log)

check_model(r4_log)

# Normality Test
shapiro_test(my.data$global_sales)
