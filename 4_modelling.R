# Gerekli paketleri yükleyin
library(dplyr)
library(caret)

train_gen1_pca <- df_train_cleaned
train_gen1_pca$Timesteps <- NULL
train_gen1_pca$ChassisId_encoded <- NULL
train_gen1_pca$gen <- NULL



# Eğitim ve test indekslerini oluşturma
train_index <- createDataPartition(train_gen1_pca$risk_level, p = 0.8, list = FALSE)

# Eğitim ve test veri setlerini oluşturma
train_data <- train_gen1_pca[train_index, ]
test_data <- train_gen1_pca[-train_index, ]

rf_model <- randomForest(risk_level ~ ., data = train_data)
  varImpPlot(rf_model)
library(MLmetrics)

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


sub_data <- df_test_cleaned
sub_data$Timesteps <- NULL
sub_data$ChassisId_encoded <- NULL
sub_data$gen <- NULL
predictions_sub <- predict(rf_model, sub_data)

df_sub <- data.frame(pred = as.character(predictions_sub))
pred4 <- read_csv("predictions/4prediction.csv")
write.csv(df_sub,"predictions/prediction.csv",row.names=FALSE)
