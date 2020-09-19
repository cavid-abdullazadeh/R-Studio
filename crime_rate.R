library(tidyverse) 
library(data.table)
library(rstudioapi)

raw <- fread("crimes.csv")
raw %>% skimr::skim()
raw %>% glimpse()



#1

df <- raw

target <- "ViolentCrimesPerPop"
features <- df %>% 
  select(-ViolentCrimesPerPop) %>% 
  names()

f <- as.formula(paste
                (target, 
                  paste(features, 
                        collapse = " + "), 
                  sep = " ~ "))
glm <- glm(f, data = df)

while(glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[1] >= 1.5){
  afterVIF <- glm %>% faraway::vif() %>% sort(decreasing = T) %>% .[-1] %>% names()
  f <- as.formula(paste(target, paste(afterVIF, collapse = " + "), sep = " ~ "))
  glm <- glm(f, data = df)
}

glm %>% faraway::vif() %>% sort(decreasing = T) %>% names() -> features 

df <- df %>% select(ViolentCrimesPerPop,features)
df %>% View()


# 2

df$ViolentCrimesPerPop -> violen_crimes

df[,-1] %>% scale() %>% as.data.frame() -> df_final

df_final$ViolentCrimesPerPop <- df$ViolentCrimesPerPop

df <- df_final %>% 
  select(ViolentCrimesPerPop, everything())

# 3 

library(h2o)
h2o.init()
h2o_data <- df %>% as.h2o()

h2o_data <- h2o_data %>% 
  h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- "ViolentCrimesPerPop"
features <- df %>% 
  select(-ViolentCrimesPerPop) %>% 
  names()

# 4
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
options(scipen = 999)
# p value of PctImmigRecent is 0.756

df %>% 
  select(-PctImmigRecent) -> df


# 5 RMSE and Adj R Squared 
y_pred <- model %>% 
  h2o.predict(newdata = test) %>% 
  as.data.frame()

test_set <- test %>% as.data.frame()
residuals <-  test_set$ViolentCrimesPerPop - y_pred$predict

# rmse
RMSE <-  sqrt(mean(residuals^2))
RMSE
# r squared
y_test_mean = mean(test_set$ViolentCrimesPerPop)

tss = sum((test_set$ViolentCrimesPerPop - y_test_mean)^2) #total 
rss = sum(residuals^2) #residual 

R2 = 1 - (rss/tss); R2

n <- test_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2 = 1-(1-R2)*((n-1)/(n-k-1))

tibble(RMSE = round(RMSE,1),
       R2, Adjusted_R2)

# 6 Check overfitting 
y_pred_train <- model %>% 
  h2o.predict(newdata = train) %>% 
  as.data.frame()

train_set <- train %>% as.data.frame()
residuals = train_set$ViolentCrimesPerPop - y_pred_train$predict

RMSE_train = sqrt(mean(residuals^2))
y_train_mean = mean(train_set$ViolentCrimesPerPop)

tss = sum((train_set$ViolentCrimesPerPop - y_train_mean)^2)
rss = sum(residuals^2)

R2_train = 1 - (rss/tss); R2_train

n <- train_set %>% nrow() #sample size
k <- features %>% length() #number of independent variables
Adjusted_R2_train = 1-(1-R2_train)*((n-1)/(n-k-1))

tibble(RMSE_train = round(RMSE_train,1),
       RMSE_test = round(RMSE,1),
       
       Adjusted_R2_train,
       Adjusted_R2_test = Adjusted_R2) %>% 
  as.data.frame() %>% View()
















