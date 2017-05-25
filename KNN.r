makeRatingsMatrix <- function(filename, trainData, numberOfUsers, numberOfItems) {
  ratingsMatrix <- matrix(nrow = numberOfUsers, ncol = numberOfItems)
  for(row in 1:nrow(trainData)){
    ratingsMatrix[as.numeric(trainData[row, 1]), as.numeric(trainData[row, 2])] <- as.numeric(trainData[row, 3])
  }
  return(ratingsMatrix)
}

userCenterMatrix <- function(matrix) {
  return(center_rowmeans(matrix))
}

itemCenterMatrix <- function(matrix) {
  return(center_colmeans(matrix))
}

center_rowmeans <- function(x) {
  xcenter = rowMeans(x, na.rm = TRUE)
  x - rep(xcenter, times = ncol(x))
}

center_colmeans <- function(x) {
  xcenter = colMeans(x, na.rm = TRUE)
  x - rep(xcenter, times = rep.int(nrow(x), ncol(x)))
}

pearsonUsers <- function(user1, user2){
  coRatedItems <- which(!is.na(ratingsMatrix[user1, ]) & !is.na(ratingsMatrix[user2, ]))
  if(length(coRatedItems) < 5)
    return(0)
  
  return(cor(ratingsMatrix[user1, ],
          ratingsMatrix[user2, ], 
          use = "pairwise.complete.obs", 
          method = "pearson"))
}

pearsonItems <- function(item1, item2){
  coUsers <- which(!is.na(ratingsMatrix[, item1]) & !is.na(ratingsMatrix[, item2]))
  if(length(coUsers) < 5)
    return(0)
  
  return(cor(ratingsMatrix[, item1],
             ratingsMatrix[, item2], 
             use = "pairwise.complete.obs", 
             method = "pearson"))
}

makeSimUsersMatrix <- function(rMatrix, filename) {
  if(file.exists(paste(getwd(), "/", filename, "SimUsers.csv", sep = ""))){
    userSimilarityMatrix <- read_csv(paste(getwd(), "/", paste(tolower(filename), "SimUsers.csv", sep = ""), sep = ""), 
                                                       col_names = FALSE, cols(.default = col_double()))
    return(as.matrix(userSimilarityMatrix))
  }
  
  userSimilarityMatrix <- matrix(nrow = nrow(rMatrix), ncol = nrow(rMatrix))
  
  for(user1 in 1:nrow(userSimilarityMatrix)){
    for(user2 in 1:nrow(userSimilarityMatrix)){
      if(user1 == user2)
        userSimilarityMatrix[user1, user2] <- 1
      else if (user2 < user1)
        userSimilarityMatrix[user1, user2] <- userSimilarityMatrix[user2, user1]
      else
        userSimilarityMatrix[user1, user2] <- pearsonUsers(user1, user2)
    }
  }
  
  return(userSimilarityMatrix)
}

makeSimItemsMatrix <- function(rMatrix, filename) {
  if(file.exists(paste(tolower(filename), "SimItems.csv", sep = ""))){
    simItemsMatrix <- read_csv(paste(getwd(), "/", paste(tolower(filename), "SimItems.csv", sep = ""), sep = ""), 
                               col_names = FALSE, cols(.default = col_double()))
    return(as.matrix(simItemsMatrix))
  }
  
  itemSimilarityMatrix <- matrix(nrow = ncol(rMatrix), ncol = ncol(rMatrix))
  
  for(item1 in 1:nrow(itemSimilarityMatrix)){
    for(item2 in 1:nrow(itemSimilarityMatrix)){
      if(item1 == item2)
        itemSimilarityMatrix[item1, item2] <- 1
      else if (item2 < item1)
        itemSimilarityMatrix[item1, item2] <- itemSimilarityMatrix[item2, item1]
      else
        itemSimilarityMatrix[item1, item2] <- pearsonItems(item1, item2)
    }
  }
  
  return(itemSimilarityMatrix)
}

getKNN <- function(k, similarityVector) {
  return(head(order(similarityVector, decreasing = TRUE), k))
}

ratingsFromKNN <- function(user, k, item) {
  users <- order(simUsersMatrix[user, ], decreasing = TRUE)
  
  top <- 0
  bottom <- 0
  consideredUsers <- 0
  
  for(v in users){
    rating <- ratingsMatrix[v, item]
    similarity <- simUsersMatrix[user, v]
    if(!is.na(rating) & v != user & 
       !is.na(similarity) & similarity != 0) {
      top <- top + (simUsersMatrix[user, v] * (ratingsMatrix[v, item] - mean(ratingsMatrix[v, ], na.rm = TRUE)))
      bottom <- bottom + (abs(simUsersMatrix[user, v]))
      consideredUsers <- consideredUsers + 1
    }
    if(consideredUsers >= k)
      break
  }
  
  if(consideredUsers == 0)
    return(0)
  
  return(top / bottom)
}

performUserBasedKNN <- function(ratingsMatrix, k) {
  predictions <- matrix(nrow = nrow(ratingsMatrix), ncol = ncol(ratingsMatrix))
  
  for(u in 1:nrow(ratingsMatrix)){
    print(u)
    userMean <- mean(ratingsMatrix[u, ], na.rm = TRUE)
    
    for(i in 1:ncol(ratingsMatrix)){
      predictions[u, i] <- userMean + ratingsFromKNN(u, k, i)
    }
  }
  
  return(predictions)
}

makeTraindata <- function(filename){
  trainData <- read_delim(paste(getwd(), "/ml-100k/", filename, ".base", sep = ""),
                          "\t", escape_double = FALSE, trim_ws = TRUE, 
                          col_names = c("userId", "movieId", "rating", "timestamp"),
                          col_types = cols(
                            userId = col_integer(),
                            movieId = col_integer(),
                            rating = col_integer(),
                            timestamp = col_integer()
                          )
  );
  return(as.matrix(trainData))
}

makeTestdata <- function(filename){
  testdata <- read_delim(paste(getwd(), "/ml-100k/", filename, ".test", sep = ""),
                          "\t", escape_double = FALSE, trim_ws = TRUE, 
                          col_names = c("userId", "movieId", "rating", "timestamp"),
                          col_types = cols(
                            userId = col_integer(),
                            movieId = col_integer(),
                            rating = col_integer(),
                            timestamp = col_integer()
                          )
  );
  return(as.matrix(testdata))
}

RMSE <- function(predictions, testData){
  squaredError <- 0
  
  for(row in 1:nrow(testData)){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    
    squaredError <- squaredError + (as.numeric(testData[row, 3]) - prediction)^2
  }
  
  return(sqrt(1/nrow(testData) * squaredError))
}

MAE <- function(predictions, testData){
  absError <- 0
  
  for(row in 1:nrow(testData)){
    prediction <- predictions[as.numeric(testData[row, 1]), as.numeric(testData[row, 2])]
    
    absError <- absError + abs(as.numeric(testData[row, 3]) - prediction)
  }
  
  return(1/nrow(testData) * absError)
}

precision <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  tp <- 0
  fp <- 0
  
  for(u in 1:numberOfUsers){
    known <- testData[which(testData[, 1] == u), ]
    
    for(k in 1:10){
      prediction <- predictions[u, known[k, 2]]
      actualRating <- known[k, 3]
      
      if(prediction >= threshhold & actualRating >= threshhold) #Recommended & Used
        tp <- tp + 1
      if(prediction >= threshhold & actualRating < threshhold) #Recommended & Unused
        fp <- fp + 1
    }
  }
  return(tp/(tp + fp))
}

recall <- function(predictions, testData, threshhold){
  numberOfUsers <- nrow(predictions)
  
  tp <- 0
  fn <- 0
  
  for(u in 1:numberOfUsers){
    known <- testData[which(testData[, 1] == u), ]
    
    for(k in 1:10){
      prediction <- predictions[u, known[k, 2]]
      actualRating <- known[k, 3]
      
      if(prediction >= threshhold & actualRating >= threshhold) #Recommended & Used
        tp <- tp + 1
      if(prediction < threshhold & actualRating >= threshhold) #Not recommended & Used
        fn <- fn + 1
    }
  }
  return(tp/(tp + fn))
}

fmeasure <- function(precision, recall){
  return((2*precision*recall)/(precision + recall))
}

