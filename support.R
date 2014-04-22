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