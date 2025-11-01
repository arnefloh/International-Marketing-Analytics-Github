# Load libraries
library(tidyverse)
library(janitor)

# Data info: https://www.kaggle.com/datasets/blastchar/telco-customer-churn?phase=FinishSSORegistration&returnUrl=/datasets/blastchar/telco-customer-churn/versions/1?resource=download&SSORegistrationToken=CfDJ8J1i-7MzxEhBg3BSP9qFZm-BP1KLeUVmLc0tceRzJ2I9jlXvK0KWvPkuzccABUFn_aQ0ShKERPltj6at9MKA5QLkpODF6-XzwoGG3CLzl_bsRhj_vFpURawFlT4Y6Arthdlbj-IWss_s2nMCzMI8P4IP3d9A9b_LBarB4UyWrx6w3snpSZmmFYKmVCV9tDJXJQeFReMca1RSorBsicdrKyWGXGLEw18ThFXVtqHhb8U2PZyE9fVf9c5qwFkTUchnqP8QYmgFUaX-DdTLEWDJV4PTfTSm42c4BXbZyH-pH3RxSLRzRfJf-uVby8AoNCwjnR4upQYoJuAYjLnXYPr4W9zJ&DisplayName=Arne%20Floh


# Load data
raw.data  <- read_csv("Data/WA_Fn-UseC_-Telco-Customer-Churn.csv") 
my.data <- raw.data |> clean_names()

# Explore data
glimpse(my.data)

my.data <- my.data |> mutate(
  churn_fct = as.factor(churn)
)

glimpse(my.data)


# Logistic Regression
r1 <- my.data |> glm(formula = churn_fct ~ tenure, family = binomial)
summary(r1)

exp(-0.038767)
#Each additional unit of tenure is associated with a 3.8% decrease in the odds of churn (1 - 0.962 = 0.038).


new <- tibble(tenure = seq(min(my.data$tenure), max(my.data$tenure), length.out = 100))
new <- new %>%
  mutate(predicted_churn = predict(r1, newdata = ., type = "response"))

ggplot(new, aes(tenure, predicted_churn)) +
  geom_line() +
  labs(title = "Predicted probability of churn vs. tenure",
       y = "P(churn)", x = "Tenure") +
  theme_minimal()
