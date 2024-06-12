source("1_data_loading.R")
library(dplyr)
# 
# dim(public_X_test)
# dim(variants)
# dim(train_gen1)
# 
# 
# 
# common_columns_public_variants <- intersect(colnames(public_X_test), colnames(variants))
# common_columns_variants_train <- intersect(colnames(variants), colnames(train_gen1))
# common_columns_all <- Reduce(intersect, list(colnames(public_X_test), colnames(variants), colnames(train_gen1)))
# 
# # Ortak sütun isimlerini yazdır
# print(common_columns_public_variants)
# print(common_columns_variants_train)
# print(common_columns_all)
# 
# 
# 
# common_chassis_public_variants <- intersect(public_X_test$`ChassisId_encoded`, variants$`ChassisId_encoded`)
# common_chassis_public_train <- intersect(public_X_test$`ChassisId_encoded`, train_gen1$`ChassisId_encoded`)
# 
# common_chassis_variants_train <- intersect(variants$`ChassisId_encoded`, train_gen1$`ChassisId_encoded`)
# common_chassis_all <- Reduce(intersect, list(public_X_test$`ChassisId_encoded`, variants$`ChassisId_encoded`, train_gen1$`ChassisId_encoded`))
# 
# # Ortak şasi ID'lerine sahip satır sayısını yazdır
# print(length(common_chassis_public_variants))
# print(length(common_chassis_variants_train))
# print(length(common_chassis_all))
# print(length(common_chassis_public_train))
# 
# risk_level_counts <- train_gen1 %>%
#   group_by(ChassisId_encoded) %>%
#   summarize(unique_risk_levels = n_distinct(Timesteps)) %>%
#   filter(unique_risk_levels > 10)
# 
# # Benzersiz risk_level sayısı 1'den fazla olan ChassisId_encoded değerlerini göster
# print(risk_level_counts)
# 
# 
# time_stamp_counts <- train_gen1 %>%
#   group_by(ChassisId_encoded) %>%
#   summarize(unique_risk_levels = n_distinct(Timesteps))
# print(time_stamp_counts)
# 
# 
# 
# train_gen1 %>%
#   filter(ChassisId_encoded == 49837)
# 
# 
# train_gen1 %>%
#   filter(Timesteps == 33) %>% tail()
# 
# 
# 
# public_X_test %>% filter(Timesteps == 33)
# train_gen1 %>% filter(ChassisId_encoded == 4955)
# 
# 
# train_gen1 %>% 
#   group_by(ChassisId_encoded) %>%
#   summarize(dd = n_distinct(Timesteps)) %>%
#   filter(dd >= 10)
# 
# train_gen1 %>%
#   filter(risk_level == "High")
