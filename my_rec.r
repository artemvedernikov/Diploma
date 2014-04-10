library('matrix')

insta
r <- as(MovieLense, "matrix") # rating matrix 

# similarity functions for users
# here userA, userB - vectors of the same size, with i - element - 
# rating given by user to the i th item or NA if user hadn't rate item yet

pearson_similarity <- function(userA, userB) {
  
  #average ratings given by users A and B
  r_a <- mean(userA, na.rm=TRUE)
  r_b <- mean(userB, na.rm=TRUE)
  
  co_rated <- (!is.na(userA) & !is.na(userB)) # note: & and && are different operators!!
  co_rated_indexes <- c(1:length(userA))[co_rated]
  
  a <- 0
  b <- 0
  c <- 0
  for (i in co_rated_indexes) {
    a <- a + (userA[i] - r_a) * (userB[i] - r_b)
    b <- b + (userA[i] - r_a) ^ 2
    c <- c + (userB[i] - r_b) ^ 2
  }
  if (b == 0) {
    b <- 1
  }
  if (c == 0) {
    c <- 1
  }
  return (a / (sqrt(b) * sqrt(c))) 
}

cosine_similarity <- function(userA, userB) {
  
  co_rated <- (!is.na(userA) & !is.na(userB)) # note: & and && are different operators!!
  
  scalar_product <- sum(userA[co_rated] * userB[co_rated])
  norm_a <- sqrt(sum(userA[co_rated]^2))
  norm_b <- sqrt(sum(userB[co_rated]^2))
  return(scalar_product / (norm_a * norm_b))
}

strange_similarity <- function(userA, userB, diff=0) {
  co_rated <- (!is.na(userA) & !is.na(userB))
  co_rated <- abs(userA[co_rated] - userB[co_rated]) <= diff
  euclid <- sqrt(sum((userA[co_rated] - userB[co_rated])^2))
  
  if (co_rated == 0) {
    return (0)
  } else {
    return(sum(co_rated) / euclid)
  }
}

# creating distance matricix between users
# TODO: Optimize it somehow
create_distance_matrix <- function(rating_matrix, method="pearson", ...) {
  res <- matrix(0, nrow(rating_matrix), nrow(rating_matrix))
  if (method == "pearson") {

    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:nrow(rating_matrix)) {
        res[i, j] <- pearson_similarity(rating_matrix[i], rating_matrix[j])      
      }
    }
    
    
  } else if (method == "cosine") {
    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:nrow(rating_matrix)) {
        res[i, j] <- cosine_similarity(rating_matrix[i], rating_matrix[j])      
      }
    }
  } else if (method == "strange") {
    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:nrow(rating_matrix)) {
        res[i, j] <- strange_similarity(rating_matrix[i], rating_matrix[j], diff=diff)      
      }
    }
  }
  return(res)
}

sparse_matrix <- function(matrix, sparsity) {
   
}
