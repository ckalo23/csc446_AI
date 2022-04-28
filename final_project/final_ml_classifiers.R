library(ggplot2)
library(cowplot)

#read csv and make certain binary/labels into factors
heart_final = read.csv("/Users/ckalo7/Documents/CSC 446/final_data/heart_final.csv")

heart_final$sex = as.factor(heart_final$sex)
heart_final$chest_pain = as.factor(heart_final$chest_pain)
heart_final$fastingBS = as.factor(heart_final$fastingBS)
heart_final$restingECG = as.factor(heart_final$restingECG)
heart_final$exercise_angina = as.factor(heart_final$exercise_angina)
heart_final$slope_st = as.factor(heart_final$slope_st)
heart_final$diagnosis = as.factor(heart_final$diagnosis)

str(heart_final) 

#Logistic Regression (diagnosis ~ rest of feat.)
logistic = glm(diagnosis ~ ., data=heart_final, family="binomial")
summary(logistic)

predict.hd = data.frame(prob.of.hd=logistic$fitted.values,
  hd=heart_final$diagnosis)

predict.hd = predict.hd[order(predict.hd$prob.of.hd, decreasing=FALSE), ]

predict.hd$rank = 1:nrow(predict.hd)

#confusion matrix
steps = step(logistic, diagnosis~., data=heart_final, family="binomial")
class_report = data.frame(response = heart_final$diagnosis, predicted = round(fitted(steps),0))

class_report_matrix = xtabs(~ predicted + response, data = class_report)
class_report_matrix

actual = c(0,0,1,1)
predicted = c(0,1,0,1)
val = c(class_report_matrix[1:4])

class_report_matrix2 = data.frame(actual, predicted, val)
my_labels = c(1,0)

#acc = (tp+tn)/(tp+tn+fp+fn) = 0.8047
#prec = tp/(tp+fp) = 0.8069
#rec = tp/(tp+fn) = 0.8462
#f1 = 2*(prec*rec)/(prec+rec) = 0.8261

png("/Users/ckalo7/Documents/CSC 446/Log-Regression-confMat")
ggplot(class_report_matrix2, aes(x=actual, y=predicted, fill=val)) + geom_tile() +
  theme(axis.ticks = element_blank()) + labs(fill = "frequency") +
  geom_text(aes(label = val)) + scale_fill_gradient(low = "#ffe5e5", high = "#ff0000") +
  scale_x_continuous(breaks=c(0.0,1.0)) + scale_y_continuous(breaks=c(0.0,1.0)) +
  ggtitle("Logistic Regression Confusion Matrix")
dev.off()

#log reg graph
png("/Users/ckalo7/Documents/CSC 446/Log-Regression-graph")
ggplot(data=predict.hd, aes(x=rank, y=prob.of.hd)) +
  geom_point(aes(color=hd), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted prob. of Heart Disease") +
  scale_color_discrete("Heart Disease") +
  ggtitle("Logistic Regression Graph")
dev.off()