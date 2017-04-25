source("KNN.r")

for(f in c("ua", "ub")){
  ratingsMatrix <- makeRatingsMatrix(f, 943, 1682)
  simUsersMatrix <- makeSimUsersMatrix(ratingsMatrix, f)
  simItemsMatrix <- makeSimItemsMatrix(ratingsMatrix, f)
  
  predictions <- performUserBasedKNN(ratingsMatrix, 20)
  write.table(predictions, file = paste(f, "predictions.csv", sep = ""), row.names = F, col.names = F, sep = ",")
}

