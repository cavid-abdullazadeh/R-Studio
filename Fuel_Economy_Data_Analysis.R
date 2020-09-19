library(tidyverse) 
library(data.table)
library(skimr)
library(plotly)
library(h2o)  
library(inspectdf)


df <- ggplot2::mpg

df %>% skim()
df %>% glimpse()

target <- 'cty'
features <- df %>% select(-cty) %>% names()

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))

glm <- glm(f, data = df)

coef_na <- attributes(alias(glm)$Complete)$dimnames[[1]]
features <- features[!features %in% coef_na]

f <- as.formula(paste(target, paste(features, collapse = " + "), sep = " ~ "))
glm <- glm(f, data = df)

glm %>% summary()

#---- split data

h2o.init()

h2o_data <- df %>% as.h2o()


h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

#----- fitting model

model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))


# Significance levels of P_value:
# 0     < p_val < 0.001 ***
# 0.001 < p_val < 0.01  **
# 0.01  < p_val < 0.05  *
# 0.05  < p_val < 0.1   .

