library(Boruta)
library(mlbench)
library(caret)
library(randomForest)

#Read data
#df = read.csv("/Users/ckalo7/Documents/CSC 446/final_data/heart_final.csv")

#Feature Selection
set.seed(111)
boruta <- Boruta(diagnosis ~ ., data = heart_final, doTrace = 2, maxRuns = 200)
print(boruta)

png("/Users/ckalo7/Documents/CSC 446/Attribute-Importance-BW")
plot(boruta, las = 2, cex.axis = 0.7)
dev.off()

png("/Users/ckalo7/Documents/CSC 446/Attribute-Importance-Line")
plotImpHistory(boruta)
dev.off()

#Tentative fix for restingBP (Tentative -> Confirmed Important)
bor <- TentativeRoughFix(boruta)
print(bor)

#Extract attribute stats ()
attStats(boruta)
