heart = read.csv("/Users/ckalo7/Documents/CSC 446/final_data/heart.csv")

#change sex values in df to char from num
heart$sex = as.character(heart$sex)

#change sex values from 0,1 to F,M
heart$sex = replace(heart$sex, heart$sex == "0", "F")
heart$sex = replace(heart$sex, heart$sex == "1", "M")

#change chest pain values in df to char from num
heart$chest_pain = as.character(heart$chest_pain)

#change chest pain values from 1,2,3,4 to TYA,ATYA,NOA,ASY
heart$chest_pain = replace(heart$chest_pain, heart$chest_pain == "1", "TYA")
heart$chest_pain = replace(heart$chest_pain, heart$chest_pain == "2", "ATYA")
heart$chest_pain = replace(heart$chest_pain, heart$chest_pain == "3", "NOA")
heart$chest_pain = replace(heart$chest_pain, heart$chest_pain == "4", "ASY")

#change bp, chol, and blood sug values from char to num
heart$restingBP = as.numeric(heart$restingBP)
heart$cholesterol = as.numeric(heart$cholesterol)
heart$fastingBS = as.numeric(heart$fastingBS)

#change resting ECG values in df to char from num
heart$restingECG = as.character(heart$restingECG)

#change resting ECG values from 0,1,2,? to Normal,ST,LVH,NA
heart$restingECG = replace(heart$restingECG, heart$restingECG == "0", "Normal")
heart$restingECG = replace(heart$restingECG, heart$restingECG == "1", "ST")
heart$restingECG = replace(heart$restingECG, heart$restingECG == "2", "LVH")
heart$restingECG = replace(heart$restingECG, heart$restingECG == "?", NA)

#change max HR values to num from char
heart$maxHR = as.numeric(heart$maxHR)

#change Exercise Angina values from 0,1,? to N,Y,NA
heart$exercise_angina = replace(heart$exercise_angina, heart$exercise_angina == "0", "N")
heart$exercise_angina = replace(heart$exercise_angina, heart$exercise_angina == "1", "Y")
heart$exercise_angina = replace(heart$exercise_angina, heart$exercise_angina == "?", NA)

#change oldpeak values in df to num from char
heart$oldpeak = as.numeric(heart$oldpeak)

#change Slope ST values from 1,2,3,? to Up,Flat,Down,NA
heart$slope_st = replace(heart$slope_st, heart$slope_st == "1", "Up")
heart$slope_st = replace(heart$slope_st, heart$slope_st == "2", "Flat")
heart$slope_st = replace(heart$slope_st, heart$slope_st == "3", "Down")
heart$slope_st = replace(heart$slope_st, heart$slope_st == "?", NA)

#change number of major vessels  values from char to num
heart$num_vessels = as.numeric(heart$num_vessels)

#change heart defect values from char to num
heart$defect = as.numeric()

#change heart defect values from 3,6,7,? to N,Y,NA
heart$defect = replace(heart$defect, heart$defect == "3", "N")
heart$defect = replace(heart$defect, heart$defect == "6", "Y")
heart$defect = replace(heart$defect, heart$defect == "7", "Y")
heart$defect = replace(heart$defect, heart$defect == "?", NA)

#change class of df diagnosis from 0,1,2,3,4 to 0,1 
heart$diagnosis = replace(heart$diagnosis, heart$diagnosis == 2, 1)
heart$diagnosis = replace(heart$diagnosis, heart$diagnosis == 3, 1)
heart$diagnosis = replace(heart$diagnosis, heart$diagnosis == 4, 1)

#test lines
#------------------------------------
#unique & length of entry column vals
length(which(heart$defect=="?"))

#See NA values in each column
colSums(is.na(heart))

#slope_st as it relates to diagnosis
length(which(heart$slope_st == "Up" & heart$diagnosis == 0))
length(which(heart$slope_st == "Flat" & heart$diagnosis == 0))
length(which(heart$slope_st == "Down" & heart$diagnosis == 0))

length(which(heart$slope_st == "Up" & heart$diagnosis == 1))
length(which(heart$slope_st == "Flat" & heart$diagnosis == 1))
length(which(heart$slope_st == "Down" & heart$diagnosis == 1))

#num_vessels as it relates to diagnosis (conditional prob.)
length(which(heart$num_vessels == 0 & heart$diagnosis == 1)) / length(which(heart$num_vessels == 0))
length(which(heart$num_vessels == 1 & heart$diagnosis == 1)) / length(which(heart$num_vessels == 1))
length(which(heart$num_vessels == 2 & heart$diagnosis == 1)) / length(which(heart$num_vessels == 2))
length(which(heart$num_vessels == 3 & heart$diagnosis == 1)) / length(which(heart$num_vessels == 3))
length(which(is.na(heart$num_vessels) & heart$diagnosis == 1)) / length(which(is.na(heart$num_vessels)))

length(which(heart$num_vessels == 0))
length(which(heart$num_vessels == 1))
length(which(heart$num_vessels == 2))
length(which(heart$num_vessels == 3))
#------------------------------------

#output the heart df to csv
write.csv(heart, "/Users/ckalo7/Documents/CSC 446/final_data/heart_processed.csv")

heart_processed = read.csv("/Users/ckalo7/Documents/CSC 446/final_data/heart_processed.csv")
heart_trimmed = read.csv("/Users/ckalo7/Documents/CSC 446/final_data/heart_processed.csv")

heart_processed <- subset(heart_processed, select = -c(X))
heart_trimmed <- subset(heart_trimmed, select = -c(X))

#trim dataset to take 'defect' and 'num_vessels' out (majority NA vals)
heart_trimmed = heart_trimmed[,1:11]
heart_trimmed[12] = heart_processed[,14]

#heart_trimmed = na.omit(heart_trimmed)
colnames(heart_trimmed)[12] <- "diagnosis"

#remove entries w/ no data on cholesterol and resting BP (10-15 of them)
heart_trimmed <- subset(heart_trimmed, !(restingBP == 0 & cholesterol == 0))

#replace NA and 0 values of resting BP with median (1 zero val and 59 NAs)
heart_trimmed$restingBP[heart_trimmed$restingBP == 0] = NA
heart_trimmed$restingBP[which(is.na(heart_trimmed$restingBP))] = median(heart_trimmed$restingBP, na.rm = TRUE)

#replace NA and 0 values of cholesterol with median (172 0s and 30 NAs)
heart_trimmed$cholesterol[heart_trimmed$cholesterol == 0] = NA
heart_trimmed$cholesterol[which(is.na(heart_trimmed$cholesterol))] = median(heart_trimmed$cholesterol, na.rm = TRUE)

#replace NA values in binary num attribute 'fastingBS'
#find ratio of 0,1 vals for 'fastingBS' out of non-NA values (to sample/impute the values)
length(which(heart_trimmed$fastingBS == 0)) / length(which(!is.na(heart_trimmed$fastingBS)))
length(which(heart_trimmed$fastingBS == 1)) / length(which(!is.na(heart_trimmed$fastingBS)))
# result:
# 0.8366093 for fastingBS = 0
# 0.1633907 for fastingBS = 1

#shuffle and reset index before replacing NAs w/ binary num
heart_trimmed = heart_trimmed[sample(1:nrow(heart_trimmed)), ]
row.names(heart_trimmed) = NULL

#find number of NA values will be replaced with 0 and 1 to match rest of column's value density
fast0 = round((length(which(is.na(heart_trimmed$fastingBS))) *.8366), 0)
fast1 = round((length(which(is.na(heart_trimmed$fastingBS))) *.1634), 0)

NA_fastBS = which(is.na(heart_trimmed$fastingBS))

#assign 0 to first ~83.66% of NA values and 1 to the last ~16.34% (after shuffling for randomness)
heart_trimmed$fastingBS[NA_fastBS[1:fast0]] = 0
heart_trimmed$fastingBS[NA_fastBS[(length(NA_fastBS)-fast1+1):length(NA_fastBS)]] = 1

#replace NA values of max HR with median (42 NAs)
heart_trimmed$maxHR[which(is.na(heart_trimmed$maxHR))] = median(heart_trimmed$maxHR, na.rm = TRUE)

#replace NA values of oldpeak with median (48 NAs)
heart_trimmed$oldpeak[which(is.na(heart_trimmed$oldpeak))] = median(heart_trimmed$oldpeak, na.rm = TRUE)

#replace NA values in categorical attributes 'exercise angina' and 'slope st'
#----------------------------------------------------------------------------
#find ratio of Y,N vals for 'exercise angina' out of non-NA values (to sample/impute the values)
length(which(heart_trimmed$exercise_angina == 'N')) / length(which(!is.na(heart_trimmed$exercise_angina)))
length(which(heart_trimmed$exercise_angina == 'Y')) / length(which(!is.na(heart_trimmed$exercise_angina)))
# result:
# 0.6109175 for exercise angina = N
# 0.3890825 for exercise angina = Y

#find ratio of Flat, Up, Down vals for 'slope st' out of non-NA values (to sample/impute the values)
length(which(heart_trimmed$slope_st == 'Flat')) / length(which(!is.na(heart_trimmed$slope_st)))
length(which(heart_trimmed$slope_st == 'Up')) / length(which(!is.na(heart_trimmed$slope_st)))
length(which(heart_trimmed$slope_st == 'Down')) / length(which(!is.na(heart_trimmed$slope_st)))
# result:
# 0.5641447 for slope st = Flat
# 0.3338816 for slope st = Up
# 0.1019737 for slope st = Down

#find number of NA values will be replaced with above values to match rest of column's value density
exangN = round((length(which(is.na(heart_trimmed$exercise_angina))) * .6109175), 0)
exangY = round((length(which(is.na(heart_trimmed$exercise_angina))) * .3890825), 0)
slopeF = round((length(which(is.na(heart_trimmed$slope_st))) * .5641447), 0)
slopeU = round((length(which(is.na(heart_trimmed$slope_st))) * .3338816), 0)
slopeD = round((length(which(is.na(heart_trimmed$slope_st))) * .1019737), 0)

NA_exang = which(is.na(heart_trimmed$exercise_angina))
NA_slope = which(is.na(heart_trimmed$slope_st))

#assign N to first ~61.09% of NA values and Y to the last ~38.91% (after shuffling for randomness)
heart_trimmed$exercise_angina[NA_exang[1:exangN]] = 'N'
heart_trimmed$exercise_angina[NA_exang[(length(NA_exang)-exangY+1):length(NA_exang)]] = 'Y'

#assign Flat to first ~56.41%, Up to the next ~33.39%, Down to the next ~10.20% (after shuffling for randomness)
heart_trimmed$slope_st[NA_slope[1:slopeF]] = 'Flat'
heart_trimmed$slope_st[NA_slope[(slopeF+1):(slopeF+1 + slopeU)]] = 'Up'
heart_trimmed$slope_st[NA_slope[(length(NA_slope)-slopeD+1):length(NA_slope)]] = 'Down'

#shuffle and reset index one last time after updating all categorical/binary NAs
heart_trimmed = heart_trimmed[sample(1:nrow(heart_trimmed)), ]
row.names(heart_trimmed) = NULL

#export final processed, imputed, cleaned data to new csv for training/testing later
heart_final = na.omit(heart_trimmed)
write.csv(heart_final, "/Users/ckalo7/Documents/CSC 446/final_data/heart_final.csv", row.names = FALSE)
