set.seed(17)

# 패키지 로드
library(pROC)
library(caret)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(rms)
# 데이터 읽기
data <- read.csv("C:\\Users\\user\\Desktop\\두드\\데이터\\데이터프레임\\7차데이터모델.csv", fileEncoding = "EUC-KR")
head(data)
names(data)
attach(data)
# 데이터 전처리
# 당뇨병유발 변수를 "No", "Yes"로 변환
data$당뇨병유발 <- ifelse(data$당뇨병유발 == 0, "No", 
                     ifelse(data$당뇨병유발 == 1, "Yes", NA))

data$만나이구간 <- ifelse(data$만나이구간 == 0, "40세미만", 
                     ifelse(data$만나이구간 == 1, "40세이상64세이하", 
                     ifelse(data$만나이구간 == 2, "65세이상", NA)))

data$BMI구간 <- ifelse(data$BMI구간 == 0, "18.5미만", 
                     ifelse(data$BMI구간 == 1, "18.5이상22.9이하", 
                     ifelse(data$BMI구간 == 2, "23.0이상24.9이하", 
                     ifelse(data$BMI구간 == 3, "25.0이상29.9이하", 
                     ifelse(data$BMI구간 == 4, "30이상", NA)))))

data$성별 <- ifelse(data$성별 == 1, "Male", 
                     ifelse(data$성별 == 2, "Female", NA))

data$고혈압여부 <- ifelse(data$고혈압여부 == 0, "No", 
                     ifelse(data$고혈압여부 == 1, "Yes", NA))

data$근력운동 <- ifelse(data$근력운동 == 0, "No", 
                     ifelse(data$근력운동 == 1, "Yes", NA))

data$현재흡연율 <- ifelse(data$현재흡연율 == 0, "No", 
                     ifelse(data$현재흡연율 == 1, "Yes", NA))

data$당뇨병유전 <- ifelse(data$당뇨병유전 == 0, "No", 
                     ifelse(data$당뇨병유전 == 1, "only_Father",
                     ifelse(data$당뇨병유전 == 2, "only_Mother", 
                     ifelse(data$당뇨병유전 == 3, "both", NA))))

# 다시 factor로 변환, "Yes"가 positive class
data$당뇨병유발 <- factor(data$당뇨병유발, levels = c("No", "Yes"))
data$만나이구간  <- factor(data$만나이구간, levels = c("40세미만","40세이상64세이하","65세이상"))
data$BMI구간 <- factor(data$BMI구간, levels=c("18.5미만","18.5이상22.9이하","23.0이상24.9이하","25.0이상29.9이하","30이상"))
data$성별 <- factor(data$성별,levels=c("Male","Female"))
data$고혈압여부 <- factor(data$고혈압여부,levels=c("No","Yes"))
data$근력운동 <- factor(data$근력운동,levels=c("No","Yes"))
data$현재흡연율 <- factor(data$현재흡연율,levels=c("No","Yes"))
data$당뇨병유전 <- factor(data$당뇨병유전,levels=c("No","only_Father","only_Mother","both"))
class(data)
class(data$당뇨병유발)
class(data$만나이구간)
class(data$BMI구간)
class(data$성별)
class(data$고혈압여부)
class(data$근력운동)
class(data$현재흡연율)
class(data$당뇨병유전)
colSums(is.na(data))

# 가중치 튜닝 범위
weights_range <- seq(1, 20, by = 0.1)

# 결과 저장 벡터 (리스트로 변경)
result_list <- list()

# 모델과 혼동 행렬을 저장할 리스트
model_list <- list()
cm_list <- list()

# 최적 모델 초기화
best_auc <- 0
best_model_auc <- NULL
best_weight_auc <- NULL
best_cm_auc <- NULL

# 최적 모델 초기화
best_sens <- 0
best_model_sens <- NULL
best_weight_sens <- NULL
best_cm_sens <- NULL

# 최적 모델 초기화
best_spec <- 0
best_model_spec <- NULL
best_weight_spec <- NULL
best_cm_spec <- NULL

# 최적 모델 초기화
best_f1 <- 0
best_model_f1 <- NULL
best_weight_f1 <- NULL
best_cm_f1 <- NULL

# my모델(제약조건 : 민감도>=0.6 특이도>=0.7)
best_auc_my <-0
best_model_my <- NULL
best_weight_my <- NULL
best_cm_my <- NULL

# my2모델(제약조건 : 민감도>=0.7 특이도>=0.6)
best_auc_my2 <-0
best_model_my2 <- NULL
best_weight_my2 <- NULL
best_cm_my2 <- NULL


# 가중치 튜닝 루프
for (weight_value in weights_range) {
  # 가중치 설정
  data$weights <- ifelse(data$당뇨병유발 == "Yes", weight_value, 1)
  
  # 5-fold 교차검증
  folds <- createFolds(data$당뇨병유발, k = 5, list = TRUE, returnTrain = FALSE)
  
  fold_results <- data.frame(
    AUC = numeric(),
    Sensitivity = numeric(),
    Specificity = numeric(),
    F1_Score = numeric()
  )
  
  for (fold_index in seq_along(folds)) {
    test_idx <- folds[[fold_index]]
    train_data <- data[-test_idx, ]
    test_data <- data[test_idx, ]
    
    # 모델 학습
    model <- train(
      당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
      data = train_data,
      method = "glm",
      family = "binomial",
      metric = "ROC",
      weights = train_data$weights,
      trControl = trainControl(method = "cv", number = 5, classProbs = TRUE)  
    )
    
    # 테스트 데이터 예측
    pred_probs <- predict(model, test_data, type = "prob")
    pred_classes <- factor(ifelse(pred_probs$Yes > 0.5, "Yes", "No"), levels = c("No", "Yes"))
    cm <- confusionMatrix(pred_classes, test_data$당뇨병유발, positive = "Yes")
    
    # AUC 계산
    roc_curve <- roc(test_data$당뇨병유발, pred_probs$Yes, levels = c("No", "Yes"))
    auc_value <- auc(roc_curve)
    
    # F1-Score 계산
    specificity_value <- cm$byClass["Specificity"]
    precision_value <- cm$byClass["Precision"]
    recall_value <- cm$byClass["Sensitivity"]
    f1_value <- 2 * (precision_value * recall_value) / (precision_value + recall_value)
    
    # 결과 저장
    fold_results <- rbind(fold_results, data.frame(
      AUC = auc_value,
      Sensitivity = cm$byClass["Sensitivity"],
      Specificity = cm$byClass["Specificity"],
      F1_Score = f1_value
    ))
  }
  
  # 각 폴드의 평균 결과 계산
  mean_results <- colMeans(fold_results)
  
  # 결과 저장 (result_list로 변경)
  result_list[[as.character(weight_value)]] <- data.frame(
    weight = weight_value,
    AUC = mean_results["AUC"],
    Sensitivity = mean_results["Sensitivity"],
    Specificity = mean_results["Specificity"],
    F1_Score = mean_results["F1_Score"]
  )
  
  # 모델과 혼동 행렬을 리스트에 저장
  model_list[[as.character(weight_value)]] <- model
  cm_list[[as.character(weight_value)]] <- cm
  
  # 최적 모델 갱신
  if (mean_results["AUC"] > best_auc) {
    best_auc <- mean_results["AUC"]
    best_model_auc <- model
    best_weight_auc <- weight_value
    best_cm_auc <- cm
  }
  if (mean_results["Sensitivity"] > best_sens) {
    best_sens <- mean_results["Sensitivity"]
    best_model_sens <- model
    best_weight_sens <- weight_value
    best_cm_sens <- cm
  }
  if (mean_results["Specificity"] > best_spec) {
    best_spec <- mean_results["Specificity"]
    best_model_spec <- model
    best_weight_spec <- weight_value
    best_cm_spec <- cm
  }
  if (mean_results["F1_Score"] > best_f1) {
    best_f1 <- mean_results["F1_Score"]
    best_model_f1 <- model
    best_weight_f1 <- weight_value
    best_cm_f1 <- cm
  }
  if (mean_results["Sensitivity"] >=0.6 && mean_results["Specificity"]>=0.7 && mean_results["AUC"]>best_auc_my) {
    best_auc_my <- mean_results["AUC"]
    best_model_my <- model
    best_weight_my <- weight_value
    best_cm_my <- cm
  }
  if (mean_results["Sensitivity"] >=0.7 && mean_results["Specificity"]>=0.6 && mean_results["AUC"]>best_auc_my2) {
    best_auc_my2 <- mean_results["AUC"]
    best_model_my2 <- model
    best_weight_my2 <- weight_value
    best_cm_my2 <- cm
  }
  
  
  cat(sprintf("Weight: %.1f | AUC: %.4f | Sens: %.4f | Spec: %.4f | F1: %.4f\n",
              weight_value, mean_results["AUC"], mean_results["Sensitivity"],
              mean_results["Specificity"], mean_results["F1_Score"]))
}



# 결과 리스트 확인
print(result_list)

# 모델과 혼동 행렬 리스트 출력 (원하는 대로 확인 가능)
print(model_list)
print(cm_list)

cat("best_weight_auc : ",best_weight_auc,"\n")
cat("best_auc : ",best_auc,"\n")
print("best_cm_auc")
print(best_cm_auc)

cat("best_weight_sens : ",best_weight_sens,"\n")
cat("best_sens : ",best_sens,"\n")
print("best_cm_sens")
print(best_cm_sens)

cat("best_weight_spec : ",best_weight_spec,"\n")
cat("best_spec : ",best_spec,"\n")
print("best_cm_spec")
print(best_cm_spec)

cat("best_weight_f1 : ",best_weight_f1,"\n")
cat("best_f1 : ",best_f1,"\n")
print("best_cm_f1")
print(best_cm_f1)

cat("best_weight_my : ",best_weight_my,"\n")
cat("best_auc_my : ",best_auc_my,"\n")
print("best_cm_my")
print(best_cm_my)

cat("best_weight_my2 : ",best_weight_my,"\n")
cat("best_auc_my2 : ",best_auc_my,"\n")
print("best_cm_my2")
print(best_cm_my2)

# 결과 데이터를 결합
results_df <- bind_rows(lapply(result_list, function(x) x), .id = "weight")

# 데이터를 numeric 타입으로 변환
results_df$weight <- as.numeric(results_df$weight)

# 각 값의 최대값 인덱스 찾기
peak_auc <- which.max(results_df$AUC)
peak_sens <- which.max(results_df$Sensitivity)
peak_spec <- which.max(results_df$Specificity)
peak_f1 <- which.max(results_df$F1_Score)

# 그래프 그리기
p1 <- ggplot(results_df, aes(x = weight, y = AUC)) + 
  geom_line(color = "blue") + 
  geom_point(aes(x = weight[peak_auc], y = AUC[peak_auc]), color = "red", size = 3) + 
  annotate("text", x = results_df$weight[peak_auc], y = results_df$AUC[peak_auc], 
           label = paste("Max AUC:", round(results_df$AUC[peak_auc], 4), "\nWeight:", results_df$weight[peak_auc]), 
           vjust = -1, color = "blue") +
  labs(title = "AUC vs Weight") + 
  theme_minimal()

p2 <- ggplot(results_df, aes(x = weight, y = Sensitivity)) + 
  geom_line(color = "green") + 
  geom_point(aes(x = weight[peak_sens], y = Sensitivity[peak_sens]), color = "red", size = 3) + 
  annotate("text", x = results_df$weight[peak_sens], y = results_df$Sensitivity[peak_sens], 
           label = paste("Max Sens:", round(results_df$Sensitivity[peak_sens], 4), "\nWeight:", results_df$weight[peak_sens]), 
           vjust = -1, color = "green") +
  labs(title = "Sensitivity vs Weight") + 
  theme_minimal()

p3 <- ggplot(results_df, aes(x = weight, y = Specificity)) + 
  geom_line(color = "purple") + 
  geom_point(aes(x = weight[peak_spec], y = Specificity[peak_spec]), color = "red", size = 3) + 
  annotate("text", x = results_df$weight[peak_spec], y = results_df$Specificity[peak_spec], 
           label = paste("Max Spec:", round(results_df$Specificity[peak_spec], 4), "\nWeight:", results_df$weight[peak_spec]), 
           vjust = -1, color = "purple") +
  labs(title = "Specificity vs Weight") + 
  theme_minimal()

p4 <- ggplot(results_df, aes(x = weight, y = F1_Score)) + 
  geom_line(color = "orange") + 
  geom_point(aes(x = weight[peak_f1], y = F1_Score[peak_f1]), color = "red", size = 3) + 
  annotate("text", x = results_df$weight[peak_f1], y = results_df$F1_Score[peak_f1], 
           label = paste("Max F1:", round(results_df$F1_Score[peak_f1], 4), "\nWeight:", results_df$weight[peak_f1]), 
           vjust = -1, color = "orange") +
  labs(title = "F1 Score vs Weight") + 
  theme_minimal()

# 그래프 출력
grid.arrange(p1, p2, p3, p4, ncol = 2)


# 1. result_list를 데이터 프레임으로 변환
data_list <- lapply(result_list, function(x) data.frame(x))
result_df <- do.call(rbind, data_list)

# weight 열 추가 (각 row에 weight 부여)
result_df$weight <- rep(sapply(result_list, function(x) x$weight), each = nrow(result_list[[1]]))

# 데이터 구조 확인
#print(result_df)

# 2. Tidy 형태로 변환
# 'AUC', 'Sensitivity', 'Specificity', 'F1_Score'를 모아 Long 형식으로 변환
result_long <- result_df %>%
  pivot_longer(cols = c(Sensitivity, Specificity, F1_Score),
               names_to = "Metric",
               values_to = "Value")

# 3. ggplot을 사용하여 AUC 값에 따른 Metric 값 시각화
ggplot(result_long, aes(x = AUC, y = Value, color = Metric)) +
  geom_line(size = 1.2) +             # 선 그래프
  geom_point(size = 3) +             # 각 포인트 추가
  scale_color_manual(values = c("green", "purple", "red")) +  # 색상 지정
  labs(title = "AUC vs Sensitivity, Specificity, F1 Score",
       x = "AUC",
       y = "Metric Value",
       color = "Metric") +           # 범례 이름 지정
  theme_minimal(base_size = 15) +    # 깔끔한 테마
  theme(legend.position = "bottom")  # 범례 아래로 배치


# Plot ROC Curve 함수 정의
plot_roc_auc <- function(best_model, data) {
  # 모델 변수 이름 자동 추출
  model_name <- deparse(substitute(best_model))
  
  # 예측 확률을 얻기
  pred_probs <- predict(best_model, data, type = "prob")  # 클래스 확률 예측
  
  # ROC 곡선 계산 (positive class는 "Yes")
  roc_curve <- roc(data$당뇨병유발, pred_probs$Yes, levels = c("No", "Yes"))
  
  # AUC 값 계산
  auc_value <- auc(roc_curve)
  
  # ROC 데이터 프레임 만들기
  roc_data <- data.frame(
    FPR = 1 - roc_curve$specificities,  # False Positive Rate (1 - Specificity)
    Sensitivity = roc_curve$sensitivities  # Sensitivity (True Positive Rate)
  )
  
  # ROC 곡선 플로팅 (x축: FPR, y축: Sensitivity)
  p <- ggplot(roc_data, aes(x = FPR, y = Sensitivity)) +
    geom_line(color = "blue", size = 1) +  # ROC 곡선
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +  # y = x 대각선
    labs(
      title = paste("ROC Curve -", model_name, "(AUC =", round(auc_value, 4), ")"),
      x = "False Positive Rate (1 - Specificity)",
      y = "Sensitivity (True Positive Rate)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5), 
      axis.title = element_text(size = 12)
    )
  cat("AUC for", model_name, ": ", auc_value, "\n")
  # 플롯 반환
  return(p)
}

{
  # 각각의 플롯 생성
  auc_plot1 <- plot_roc_auc(best_model_auc, data)
  auc_plot2 <- plot_roc_auc(best_model_sens, data)
  auc_plot3 <- plot_roc_auc(best_model_spec, data)
  auc_plot4 <- plot_roc_auc(best_model_f1, data)
  
  # 여러 플롯을 하나의 화면에 배치
  grid.arrange(auc_plot1, auc_plot2, auc_plot3, auc_plot4, nrow = 2, ncol = 2)
}

{
  # 각각의 플롯 생성
  auc_plot5 <- plot_roc_auc(best_model_my, data)
  auc_plot6 <- plot_roc_auc(best_model_my2, data)
  
  # 여러 플롯을 하나의 화면에 배치
  grid.arrange(auc_plot5, auc_plot6, nrow = 2, ncol = 2)
}




# AUC 모델: lrm 모델 학습
ddist <- datadist(data)
options(datadist = "ddist")
best_model_auc_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_auc_lrm)
best_model_auc_lrm$coefficients <- coef(best_model_auc$finalModel)
coef(best_model_auc_lrm)

nom_auc <- nomogram(best_model_auc_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_auc)
title(main = "Nomogram for best_model_auc")

# Sens 모델: lrm 모델 학습 
ddist <- datadist(data)
options(datadist = "ddist")
best_model_sens_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_sens_lrm)
best_model_sens_lrm$coefficients <- coef(best_model_sens$finalModel)
coef(best_model_sens_lrm)

nom_sens <- nomogram(best_model_sens_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_sens)
title(main = "Nomogram for best_model_sens")

# Spec 모델 : lrm 모델 학습
ddist <- datadist(data)
options(datadist = "ddist")
best_model_spec_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_spec_lrm)
best_model_spec_lrm$coefficients <- coef(best_model_spec$finalModel)
coef(best_model_spec_lrm)

nom_spec <- nomogram(best_model_spec_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_spec)
title(main = "Nomogram for best_model_spec")


# F1 모델 : lrm 모델 학습
ddist <- datadist(data)
options(datadist = "ddist")
best_model_f1_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_f1_lrm)
best_model_f1_lrm$coefficients <- coef(best_model_f1$finalModel)
coef(best_model_f1_lrm)

nom_f1 <- nomogram(best_model_f1_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_f1)
title(main = "Nomogram for best_model_f1")


# my 모델 : lrm 모델 학습
ddist <- datadist(data)
options(datadist = "ddist")
best_model_my_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_my_lrm)
best_model_my_lrm$coefficients <- coef(best_model_my$finalModel)
coef(best_model_my_lrm)

nom_my <- nomogram(best_model_my_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_my)
title(main = "Nomogram for best_model_my")

# my2 모델 : lrm 모델 학습
ddist <- datadist(data)
options(datadist = "ddist")
best_model_my2_lrm <- lrm(당뇨병유발 ~ 만나이구간 + BMI구간 + 성별 + 고혈압여부 + 근력운동 + 현재흡연율 + 당뇨병유전,
                            data = data, weights = data$weights)
coef(best_model_my2_lrm)
best_model_my2_lrm$coefficients <- coef(best_model_my2$finalModel)
coef(best_model_my2_lrm)

nom_my2 <- nomogram(best_model_my2_lrm, fun = function(x) 1 / (1 + exp(-x)), funlabel = "Predicted Probability (당뇨병)")

plot(nom_my2)
title(main = "Nomogram for best_model_my2")

########노모그램 total 점수에 대한 시각화################

nom_f1
nom_f1['total.points']
nom_f1$x
str(nom_f1['Predicted Probability (당뇨병)'])
print(nom_f1$`Predicted Probability (당뇨병)`$x)
print(nom_f1$`Predicted Probability (당뇨병)`$x.real)



# 그래프를 그리는 함수 정의
plot_nomogram <- function(nom_object, plot_title = "Total Points vs Predicted Probability") {
  # 데이터 준비: TotalPoints와 PredictedProbability를 data.frame으로 만들기
  data_for_plot <- data.frame(
    TotalPoints = nom_object$`Predicted Probability (당뇨병)`$x,
    PredictedProbability = nom_object$`Predicted Probability (당뇨병)`$x.real
  )
  
  # (0, 0) 값을 데이터에 추가
  data_for_plot <- rbind(data.frame(TotalPoints = 0, PredictedProbability = 0), data_for_plot)
  
  # y = 0.5일 때 TotalPoints 값 찾기
  total_points_at_0_5 <- data_for_plot$TotalPoints[which.min(abs(data_for_plot$PredictedProbability - 0.5))]
  
  # ggplot으로 시각화
  ggplot(data_for_plot, aes(x = TotalPoints, y = PredictedProbability)) +
    geom_line(color = "blue", size = 1.2) +  # 선 그래프
    geom_point(color = "red", size = 3) +    # 데이터 점을 표시
    labs(title = plot_title,
         x = "Total Points",
         y = "Predicted Probability (당뇨병)") +
    theme_minimal() +                       # 깔끔한 테마 적용
    theme(
      plot.title = element_text(hjust = 0.5),  # 제목 중앙 정렬
      axis.title = element_text(size = 12),    # 축 제목 크기 조정
      axis.text = element_text(size = 10)      # 축 글자 크기 조정
    ) +
    geom_text(aes(x = total_points_at_0_5, y = 0.5, label = paste("Total Points at y=0.5:", round(total_points_at_0_5, 2))),
              vjust = -1, color = "black", size = 4)  # y=0.5에서 Total Points 값 표시
}

# 각 nomogram 객체에 대해 함수 호출
plot_nomogram(nom_auc, "Total Points vs Predicted Probability (AUC)")
plot_nomogram(nom_sens, "Total Points vs Predicted Probability (Sensitivity)")
plot_nomogram(nom_spec, "Total Points vs Predicted Probability (Specificity)")
plot_nomogram(nom_f1, "Total Points vs Predicted Probability (F1 Score)")
plot_nomogram(nom_my, "Total Points vs Predicted Probability (my Score)")
plot_nomogram(nom_my2, "Total Points vs Predicted Probability (my2 Score)")
summary(nom_f1)
nom_auc
nom_sens
nom_spec
nom_f1
nom_my
nom_my2
summary(best_model_my)
summary(best_model_my2)
best_model_my2_lrm
########################################################################3



# 사용자로부터 값 하나 입력받기 API부분
user_age <- readline(prompt = "만나이를 입력하세요(예시 40세미만:0, 40세이상64세이하:1, 65세이상:2 " ) 
cat("age 입력된 값은:", user_age, "입니다.\n")

user_bmi <- readline(prompt = "bmi를 입력하세요(예시 18.5미만:0, 18.5이상22.9이하:1, 23.0이상 24.9이하:3, 30이상:4) : ")
cat("bmi 입력된 값은:", user_bmi, "입니다.\n")

user_sex <- readline(prompt = "성별을 입력하세요(예시 남성:1, 여성:2) : ")
cat("sex 입력된 값은:", user_sex, "입니다.\n") 

user_hypertension <- readline(prompt = "고혈압 여부를 입력하세요(예시 고혈압x:0, 고혈압o:1) : ")
cat("hypertension 입력된 값은:", user_hypertension, "입니다.\n") 

user_exercise <- readline(prompt = "근력운동 여부를 입력하세요(예시 근력운동x:0, 근력운동o:1) : ")
cat("exercise 입력된 값은:", user_exercise, "입니다.\n")

user_smoke <- readline(prompt = "흡연 여부를 입력하세요(예시 흡연x:0, 흡연o:1) : ")
cat("smoke 입력된 값은:", user_smoke, "입니다.\n")

user_heredity <- readline(prompt = "당뇨병 유전 여부를 입력하세요(예시 no:0, only_father:1, only_mother:2, both:3) : ")
cat("heredity 입력된 값은:", user_heredity, "입니다.\n")


user_age <- ifelse(user_age == 0, "40세미만", 
                     ifelse(user_age == 1, "40세이상64세이하", 
                     ifelse(user_age == 2, "65세이상", NA)))

user_bmi <- ifelse(user_bmi == 0, "18.5미만", 
                     ifelse(user_bmi == 1, "18.5이상22.9이하", 
                     ifelse(user_bmi == 2, "23.0이상24.9이하", 
                     ifelse(user_bmi == 3, "25.0이상29.9이하", 
                     ifelse(user_bmi == 4, "30이상", NA)))))

user_sex <- ifelse(user_sex == 1, "Male", 
                     ifelse(user_sex == 2, "Female", NA))

user_hypertension <- ifelse(user_hypertension == 0, "No", 
                     ifelse(user_hypertension == 1, "Yes", NA))

user_exercise <- ifelse(user_exercise == 0, "No", 
                     ifelse(user_exercise == 1, "Yes", NA))

user_smoke <- ifelse(user_smoke == 0, "No", 
                     ifelse(user_smoke == 1, "Yes", NA))

user_heredity <- ifelse(user_heredity == 0, "No", 
                     ifelse(user_heredity == 1, "only_Father",
                     ifelse(user_heredity == 2, "only_Mother", 
                     ifelse(user_heredity == 3, "both", NA))))

levels_age <- levels(best_model_my$trainingData$만나이구간)
levels_bmi <- levels(best_model_my$trainingData$BMI구간)
levels_sex <- levels(best_model_my$trainingData$성별)
levels_hypertension <- levels(best_model_my$trainingData$고혈압여부)
levels_exercise <- levels(best_model_my$trainingData$근력운동)
levels_smoke <- levels(best_model_my$trainingData$현재흡연율)
levels_heredity <- levels(best_model_my$trainingData$당뇨병유전)

new_user <- data.frame(
  만나이구간 = factor(user_age, levels = levels_age),
  BMI구간 = factor(user_bmi, levels = levels_bmi),
  성별 = factor(user_sex, levels = levels_sex),
  고혈압여부 = factor(user_hypertension, levels = levels_hypertension),
  근력운동 = factor(user_exercise, levels = levels_exercise),
  현재흡연율 = factor(user_smoke, levels = levels_smoke),
  당뇨병유전 = factor(user_heredity, levels = levels_heredity)
)
new_user

##당뇨병 나이 노모그램 점수##
user_age
{
age_points <- nom_my$만나이구간$points
age_40_points <-age_points[nom_my$만나이구간$만나이구간 == "40세미만"]
age_40_64_points <-age_points[nom_my$만나이구간$만나이구간 == "40세이상64세이하"]
age_65_points <-age_points[nom_my$만나이구간$만나이구간 == "65세이상"]
if(user_age=="40세미만") {age_nomo_points <- age_40_points}
if(user_age=="40세이상64세이하") {age_nomo_points <- age_40_64_points}
if(user_age=="65세이상") {age_nomo_points <- age_65_points}
}
age_nomo_points


##당뇨병 BMI 노모그램 점수##
user_bmi
{
bmi_points <- nom_my$BMI구간$points
bmi_18.5_points <-bmi_points[nom_my$BMI구간$BMI구간 == "18.5미만"]
bmi_18.5_22.9_points <-bmi_points[nom_my$BMI구간$BMI구간 == "18.5이상22.9이하"]
bmi_23.0_24.9_points <-bmi_points[nom_my$BMI구간$BMI구간 == "23.0이상24.9이하"]
bmi_25.0_29.9_points <-bmi_points[nom_my$BMI구간$BMI구간 == "25.0이상29.9이하"]
bmi_30_points <-bmi_points[nom_my$BMI구간$BMI구간 == "30이상"]
if(user_bmi=="18.5미만") {bmi_nomo_points <- bmi_18.5_points}
if(user_bmi=="18.5이상22.9이하") {bmi_nomo_points <- bmi_18.5_22.9_points}
if(user_bmi=="23.0이상24.9이하") {bmi_nomo_points <- bmi_23.0_24.9_points}
if(user_bmi=="25.0이상29.9이하") {bmi_nomo_points <- bmi_25.0_29.9_points }
if(user_bmi=="30이상") {bmi_nomo_points <- bmi_30_points }
}
bmi_nomo_points

##당뇨병 성별 노모그램 점수##
user_sex
{
sex_points <- nom_my$성별$points
sex_male_points <-sex_points[nom_my$성별$성별 == "Male"]
sex_female_points <-sex_points[nom_my$성별$성별 == "Female"]
if(user_sex=="Male") {sex_nomo_points <- sex_male_points}
if(user_sex=="Female") {sex_nomo_points <- sex_female_points}
}
sex_nomo_points

##당뇨병 고혈압 노모그램 점수##
user_hypertension
{
hypertesnsion_points <- nom_my$고혈압여부$points
hypertension_no_points <-hypertesnsion_points[nom_my$고혈압여부$고혈압여부 == "No"]
hypertension_yes_points <-hypertesnsion_points[nom_my$고혈압여부$고혈압여부 == "Yes"]
if(user_hypertension=="No") {hypertension_nomo_points <- hypertension_no_points}
if(user_hypertension=="Yes") {hypertension_nomo_points <- hypertension_yes_points}
}
hypertension_nomo_points


##당뇨병 근력운동 노모그램 점수##
user_exercise
{
exercise_points <- nom_my$근력운동$points
exercise_no_points <-exercise_points[nom_my$근력운동$근력운동 == "No"]
exercise_yes_points <-exercise_points[nom_my$근력운동$근력운동 == "Yes"]
if(user_exercise=="No") {exercise_nomo_points <- exercise_no_points}
if(user_exercise=="Yes") {exercise_nomo_points <- exercise_yes_points}
}
exercise_nomo_points

##당뇨병 흡연 노모그램 점수##
user_smoke
{
smoke_points <- nom_my$현재흡연율$points
smoke_no_points <-smoke_points[nom_my$현재흡연율$현재흡연율 == "No"]
smoke_yes_points <-smoke_points[nom_my$현재흡연율$현재흡연율 == "Yes"]
if(user_smoke=="No") {smoke_nomo_points <- smoke_no_points}
if(user_smoke=="Yes") {smoke_nomo_points <- smoke_yes_points}
}
smoke_nomo_points



##당뇨병 유전 노모그램 점수##
user_heredity
{
diabetes_inheritance_points <- nom_my$당뇨병유전$points
inheretance_none_points <- diabetes_inheritance_points[nom_my$당뇨병유전$당뇨병유전 == "No"]
inheretance_only_Father_points <- diabetes_inheritance_points[nom_my$당뇨병유전$당뇨병유전 == "only_Father"]
inheretance_only_Mother_points <- diabetes_inheritance_points[nom_my$당뇨병유전$당뇨병유전 == "only_Mother"]
inheretance_both_points <- diabetes_inheritance_points[nom_my$당뇨병유전$당뇨병유전 == "both"]
if(user_heredity=="No") inheritance_nomo_points <- inheretance_none_points
if(user_heredity=="only_Father") inheritance_nomo_points <- inheretance_only_Father_points
if(user_heredity=="only_Mother") inheritance_nomo_points <- inheretance_only_Mother_points
if(user_heredity=="both") inheritance_nomo_points <- inheretance_both_points
}
inheritance_nomo_points

total_nomo_points = age_nomo_points + bmi_nomo_points + sex_nomo_points + hypertension_nomo_points + exercise_nomo_points +smoke_nomo_points +inheritance_nomo_points
#total_nomo_points


user_pred_probs <- predict(best_model_my, newdata = new_user, type = "prob")
user_pred_classes <- factor(ifelse(user_pred_probs$Yes > 0.5, "Yes", "No"), levels = c("No", "Yes"))


# 결과 출력

cat("당뇨병일 확률 : ", user_pred_probs$Yes, "\n")
cat("당뇨병이 아닐확률 : ", user_pred_probs$No, "\n")
cat("예측된 클래스 : ", as.character(user_pred_classes[1]), "\n")
cat("당뇨병 점수 : ",total_nomo_points,"\n")
