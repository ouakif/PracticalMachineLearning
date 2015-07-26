library(caret)
training = read.csv("pml-training.csv")
testing = read.csv("pml-testing.csv")
# levels(training$classe) <- 1:5
# levels(training$user_name) <- 1:6
# levels(training$new_window) <- 0:1
training$classe = as.numeric(training$classe)
training$user_name = as.numeric(training$user_name)
training$new_window = as.numeric(training$new_window)
training_data_subset <- subset(training, select=c("new_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z","pitch_forearm","roll_forearm","yaw_forearm","total_accel_forearm","gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y","accel_forearm_z","magnet_forearm_x","magnet_forearm_y","magnet_forearm_z","classe"))
preProc <- preProcess(training_data_subset,method="pca",thresh=0.8)
ctrl <- trainControl(preProcOptions = list(thresh = 0.8))
modelFit_pp <- train(training_data_subset$classe ~ . , preProcess="pca", method="glm", data = training_data_subset,trControl = ctrl)
# make predictions
testing_sub_set <- subset(training, select=c("new_window","roll_belt","pitch_belt","yaw_belt","total_accel_belt","gyros_belt_x","gyros_belt_y","gyros_belt_z","accel_belt_x","accel_belt_y","accel_belt_z","magnet_belt_x","magnet_belt_y","magnet_belt_z","roll_arm","pitch_arm","yaw_arm","total_accel_arm","gyros_arm_x","gyros_arm_y","gyros_arm_z","accel_arm_x","accel_arm_y","accel_arm_z","magnet_arm_x","magnet_arm_y","magnet_arm_z","roll_dumbbell","pitch_dumbbell","yaw_dumbbell","total_accel_dumbbell","gyros_dumbbell_x","gyros_dumbbell_y","gyros_dumbbell_z","accel_dumbbell_x","accel_dumbbell_y","accel_dumbbell_z","magnet_dumbbell_x","magnet_dumbbell_y","magnet_dumbbell_z","pitch_forearm","roll_forearm","yaw_forearm","total_accel_forearm","gyros_forearm_x","gyros_forearm_y","gyros_forearm_z","accel_forearm_x","accel_forearm_y","accel_forearm_z","magnet_forearm_x","magnet_forearm_y","magnet_forearm_z","classe"))
predictions <- predict(modelFit_pp, training_sub_set)
confusionMatrix(predictions, testing_sub_set$classe)
modelFit_np <- train(training_data_subset$classe ~ . , method="glm", data = training_data_subset)
predictions_np <- predict(modelFit_np, testing_sub_set)
confusionMatrix(predictions_np, testing_sub_set$classe)



