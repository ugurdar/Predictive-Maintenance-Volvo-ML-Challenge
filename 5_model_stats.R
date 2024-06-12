library(dplyr)
library(caret)
library(MLmetrics)
library(randomForest)
library(beepr)
# Sayısal sütunları seçin (Timesteps ve ChassisId_encoded hariç)
numerical_columns <- df_train_cleaned |>
  select(where(is.numeric)) |>
  select(-c(Timesteps, ChassisId_encoded)) |>
  colnames()

numerical_columns_test <- df_test_cleaned |>
  select(where(is.numeric)) |>
  select(-c(Timesteps, ChassisId_encoded)) |>
  colnames()

# Özet istatistikleri hesaplama fonksiyonu
calculate_summary_stats <- function(data) {
  data |>
    mutate(across(everything(), list(
      min = ~ min(.),
      max = ~ max(.),
      median = ~ median(.),
      range = ~ max(.) - min(.),
      mean = ~ mean(.)
    )))
}

# Özet istatistikler
# train
summary_stats <- df_train_cleaned |>
  group_by(ChassisId_encoded) |>
  select(all_of(numerical_columns)) |>
  calculate_summary_stats()|>
  ungroup() |>
  select(-ChassisId_encoded) 
# test
summary_stats_test <- df_test_cleaned |>
  group_by(ChassisId_encoded) |>
  select(all_of(numerical_columns)) |>
  calculate_summary_stats() |>
  ungroup() |>
  select(-ChassisId_encoded) 

# Özet istatistikleri orijinal veri setine ekleyin

# Orjinal columlarla birlikte
# df_train_summary <- df_train_cleaned |>
#   select(-all_of(numerical_columns)) |>
#   bind_cols(summary_stats)
# df_test_summary <- df_test_cleaned |>
#   select(-all_of(numerical_columns)) |>
#   bind_cols(summary_stats_test)

# sadece istatistikler
df_train_summary <- df_train_cleaned |>
  select(-all_of(numerical_columns)) |>
  bind_cols(summary_stats) |>
  select(-all_of(numerical_columns)) 


df_test_summary <- df_test_cleaned |>
  select(-all_of(numerical_columns)) |>
  bind_cols(summary_stats_test) |>
  select(-all_of(numerical_columns)) 

# 0 varyans olanların çıkartılması
zero_variance_cols_stats <- summary_stats %>%
  summarise(across(everything(), ~ var(.) == 0)) %>%
  select(where(~ . == TRUE)) %>%
  names()

df_train_summary <- df_train_summary %>%
  select(-all_of(zero_variance_cols_stats))

df_test_summary <- df_test_summary %>%
  select(-all_of(zero_variance_cols_stats))

# Tabloyu gösterme
print(df_train_summary)


## Low'dan başka bir değere geçiş oluyor mu çok fazla buna bak.
## Verinin ne kadarında böyle bir değişim oluyor yoksa genelde
## risk düzeyi ne ise orada mı kalıyor?


train_gen1_pca_stats <- df_train_summary
train_gen1_pca_stats$Timesteps <- NULL
train_gen1_pca_stats$`ChassisId_encoded` <- NULL
train_gen1_pca_stats$gen <- NULL

numerical_data_pca_stats <- train_gen1_pca_stats |>
  select(-c(risk_level)) |>
  select(where(is.numeric))


numerical_data_pca_stats_test <- df_test_summary |>
  select(where(is.numeric)) |>
  select(-Timesteps)


# PCA uygulaması
preProcess_params <- preProcess(numerical_data_pca_stats,
                                method = c("center", "scale", "pca"),
                                pcaComp = 30)
train_pca_stats <- predict(preProcess_params, numerical_data_pca_stats)
test_pca_stats <- predict(preProcess_params, numerical_data_pca_stats_test)

# PCA sonucu olan veriyi orijinal veri seti ile birleştirme
train_gen1_pca_stats_ <- df_train_summary |>
  select(Timesteps, ChassisId_encoded, gen, risk_level) |>
  bind_cols(train_pca_stats) |>
  select(-any_of(numerical_columns))

train_gen1_pca_stats_merged <- left_join(train_gen1_pca_stats_,
                                         variants,
                                         by= "ChassisId_encoded")

test_gen1_pca_stats_<- df_test_summary |>
  select(Timesteps, -ChassisId_encoded, gen) |>
  bind_cols(test_pca_stats)|>
  select(-any_of(numerical_columns))

test_gen1_pca_stats_merged <- left_join(test_gen1_pca_stats_,
                                         variants,
                                         by= "ChassisId_encoded")

variant_df <- train_gen1_pca_stats_ %>% select(ChassisId_encoded,
                                               risk_level) %>% 
  left_join(.,
            variants,
            by= "ChassisId_encoded") |>
  select(-ChassisId_encoded)

model_data <- train_gen1_pca_stats_merged |>
  select(-Timesteps,-ChassisId_encoded,-gen)
# Eğitim ve test indekslerini oluşturma
train_index <- createDataPartition(model_data$risk_level, p = 0.8, list = FALSE)

# Eğitim ve test veri setlerini oluşturma
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

rf_model <- randomForest(risk_level ~ ., data = train_data)
beep(sound = "coin")

varImpPlot(rf_model)


predictions <- predict(rf_model, test_data)

# Gerçek ve tahmin edilen değerleri al
true_values <- test_data$risk_level

# Confusion Matrix (Karışıklık Matrisi) oluşturma
confusion <- confusionMatrix(predictions, true_values)

# F1 Skorunu hesaplama
f1_score <- F1_Score(y_pred = predictions, y_true = true_values, positive = NULL)

# Sonuçları yazdırma
print(confusion)
print(paste("F1 Score: ", f1_score))


sub_data <- test_gen1_pca_stats_merged |>
  select(-Timesteps,-ChassisId_encoded,-gen)
sub_data$Timesteps <- NULL
sub_data$ChassisId_encoded <- NULL
sub_data$gen <- NULL
predictions_sub <- predict(rf_model, sub_data)
df_sub <- data.frame(pred = as.character(predictions_sub))
write.csv(df_sub,"predictions/prediction.csv",row.names=FALSE)
