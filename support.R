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
    return (0)
  }
  if (c == 0) {
    return(0)
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

clust

# creating similarity matricix between users
# TODO: Optimize it somehow
create_similarity_matrix <- function(rating_matrix, method="pearson", ...) {
  res <- matrix(0, nrow(rating_matrix), nrow(rating_matrix))
  if (method == "pearson") {
    
    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:i) {
        res[i, j] <- pearson_similarity(rating_matrix[i,], rating_matrix[j,])      
      }
    }
    
    
  } else if (method == "cosine") {
    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:nrow(rating_matrix)) {
        res[i, j] <- cosine_similarity(rating_matrix[i,], rating_matrix[j,])      
      }
    }
  } 
  for (i in 1:nrow(res)) {
    for (j in i: nrow(res))
      res[i,j] <- res[j,i]
  }
  res[is.na(res)] <- 0
  return(res)
}

best_clust <- function(array, degree=0.8) { 
  degrees_and_class <- array
  degrees_and_class <- rbind(degrees_and_class,  c(1:length(array)))
  ord <- order(degrees_and_class[1,], decreasing = TRUE)
  degrees_and_class <- degrees_and_class[,ord]
  sum <- 0
  res <- 1
  repeat {
    sum <- sum + degrees_and_class[1,res]
    if (sum >= degree) {
      break
    } else {
      res <- res + 1
    }
  }
  return(degrees_and_class[2,1:res])
}


# creates new matrix leaving only rows who belong to the clusters, (TEST 10)

# data - rating matrix
# clustering - vector of positive integers,  users clusters
# clusters - vector of positive integers, clusters of user we want to estimate
cut_data_matrix <- function(data, clustering, clusters) { 
  result <- data[1,] # the line we want to predict is the first line
  
  for (i in 2:nrow(data)) {
    if (clustering[i] %in% clusters) { #!!!!!!!
      result <- rbind(result, data[i,])
    }
  }
  return (result)
}

# matrix - rating matrix
# row_number - positive integer
# similarity - double vector
# mean_user_ratings - double vector
# users_who_rated - boolean matrix
# ratings_to_estimate - boolean vector
# check - additional stuff for debugging
# co_rated_bound - integer
# return_na - bool

estimate_user <- function(matrix, row_number, similarity, mean_user_ratings, users_who_rated, ratings_to_estimate, co_rated_bound, return_na, check = "0") { # TODO: BAD SIMILARITY MATRIX!!!!
  print("estimation started")
  print(Sys.time())
  if (nrow(matrix) == 1) { # matrix = vector 
    result <- matrix
    print("oops")
    print(check)
    mean_user_rating <- mean(matrix[1,], na.rm = TRUE)
    for (i in 1: ncol(matrix)) {
      if (is.na(matrix[1,i])) {
        result[1,i] <- mean_user_rating
      }
    }
    return(result)
  } 
  
  estimated_matrix <- matrix
  mean_rating <- mean_user_ratings[row_number]
  estimated_user <- c()

  for (l in 1: ncol(estimated_matrix)) {  # WHY IS THIS LOOP SOOOOO SLOOOOOW?!

    if (ratings_to_estimate[l]) { # estimate missing rating r_{1l} - only for current user
      
      users_who_rated_item_l <- users_who_rated[,l] ##!!!!
      if (sum(users_who_rated_item_l) <= co_rated_bound) {
        print("no users")
        print(l)
        if (return_na) { 
          estimated_user <- c(estimated_user, NA)
        } else {
          estimated_user <- c(estimated_user, mean_rating )
        }
      } else {

        mean_user_ratings_who_rated_item_l <- mean_user_ratings[users_who_rated_item_l] 

        item_l_ratings <- estimated_matrix[users_who_rated_item_l, l]  
 
        diff_ratings <- item_l_ratings - mean_user_ratings_who_rated_item_l
 
        sim_users_who_rated_item_l <- similarity[users_who_rated_item_l]
        sum_dist <- sum(abs(sim_users_who_rated_item_l)) # denominator
        
        if (sum_dist == 0) {
          estimated_rating <- 0
        } else {
          estimated_rating <- sum(sim_users_who_rated_item_l  * diff_ratings) / sum_dist
        }
        
        print(Sys.time())
        estimated_user <- c(estimated_user, mean_rating + estimated_rating)
      }
    } else {
      estimated_user <- c(estimated_user, estimated_matrix[row_number, l])
    } 
  } 
  return(estimated_user)
}