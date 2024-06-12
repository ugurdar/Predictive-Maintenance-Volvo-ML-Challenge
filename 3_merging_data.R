library(zoo)
library(dplyr)
df_train <- left_join(train_gen1,variants,by="ChassisId_encoded")
df_train <- train_gen1
# df_train_variant <- train_gen1 %>%
#   select(ChassisId_encoded,risk_level,gen) %>%
#   left_join(.,
#                               variants,
#                               by="ChassisId_encoded")%>%
#   select(-ChassisId_encoded)
# df_train_variant$risk_level <- factor(df_train_variant$risk_level)
# df_train_variant$gen <- as.numeric(factor(df_train_variant$gen))

df_train$risk_level <- as.factor(df_train$risk_level)


missing_threshold <- 0.99

missing_ratios <- df_train %>% summarise_all(~ mean(is.na(.)))

high_missing_columns <- names(missing_ratios)[missing_ratios > missing_threshold]

print(high_missing_columns)

df_train_cleaned <- df_train %>% select(-all_of(high_missing_columns))
zero_variance_cols <- df_train_cleaned %>%
  select(-c(Timesteps, ChassisId_encoded, gen, risk_level)) %>%
  summarise(across(everything(), ~ var(.) == 0)) %>%
  select(where(~ . == TRUE)) %>%
  names()
df_train_cleaned <- df_train_cleaned %>%
  select(-all_of(zero_variance_cols))

df_test <- left_join(public_X_test,variants,by="ChassisId_encoded")
df_test <- public_X_test

df_test_cleaned <- df_test %>% select(-all_of(high_missing_columns)) %>%
  select(-all_of(zero_variance_cols))
  


calculate_moving_features <- function(data, window_size) {
  data %>%
    mutate(
      across(
        starts_with("af1__"),
        ~ rollapply(
          .x,
          width = window_size,
          FUN = mean,
          fill = -1,
          align = "right"
        ),
        .names = "ma_{col}"
      ),
      across(
        starts_with("af1__"),
        ~ rollapply(
          .x,
          width = window_size,
          FUN = sd,
          fill = -1,
          align = "right"
        ),
        .names = "sd_{col}"
      )
    )
}

# Hareketli ortalama ve standart sapma ekleme
window_size <- 3  # Hareketli ortalama ve standart sapma için pencere boyutu

train_gen1_moving <- df_train_cleaned %>%
  group_by(ChassisId_encoded) %>%
  arrange(Timesteps) %>%
  do(calculate_moving_features(., window_size)) %>%
  ungroup()


test_gen1_moving <- df_test_cleaned %>%
  group_by(ChassisId_encoded) %>%
  arrange(Timesteps) %>%
  do(calculate_moving_features(., window_size)) %>%
  ungroup()

# Verinin PCA için hazırlanması
# Sıfır varyansa sahip sütunları belirleyin ve çıkarın
numerical_data <- train_gen1_moving %>%
  select(-c(Timesteps, ChassisId_encoded, gen, risk_level)) %>%
  select(where(is.numeric))

# Sıfır varyansa sahip sütunları kontrol edin ve çıkarın
zero_variance_cols <- numerical_data %>%
  summarise(across(everything(), ~ var(.) == 0)) %>%
  select(where(~ . == TRUE)) %>%
  names()

numerical_data <- numerical_data %>%
  select(-all_of(zero_variance_cols))

numerical_data_test <- test_gen1_moving %>%
  select(-c(Timesteps, ChassisId_encoded, gen)) %>%
  select(where(is.numeric))


# PCA uygulaması
preProcess_params <- preProcess(numerical_data,
                                method = c("center", "scale", "pca"),
                                pcaComp = 100)
train_pca <- predict(preProcess_params, numerical_data)
test_pca <- predict(preProcess_params, numerical_data_test)

# PCA sonucu olan veriyi orijinal veri seti ile birleştirme
train_gen1_pca <- train_gen1_moving %>%
  select(Timesteps, ChassisId_encoded, gen, risk_level) %>%
  bind_cols(train_pca)

test_gen1_pca <- test_gen1_moving %>%
  select(Timesteps, ChassisId_encoded, gen) %>%
  bind_cols(test_pca)

# PCA sonrası veri setinin boyutlarını ve sütun isimlerini göster
print(dim(train_gen1_pca))
print(colnames(train_gen1_pca))



test_gen1_moving_0_var <- test_gen1_moving %>%
  select(-all_of(zero_variance_cols))


# train_gen1_cleaned <- train_gen1_moving %>%
#   filter(complete.cases(.))
