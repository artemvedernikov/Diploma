install.packages('e1071') # fuzzy clustering
library('e1071')

install.packages('cluster')
library('cluster')

install.packages('sets')
library('sets') # set difference


library('matrix') # library for work with sparse matrices
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
  } else if (method == "strange") {
    for (i in 1:nrow(rating_matrix)) {
      for (j in 1:nrow(rating_matrix)) {
        res[i, j] <- strange_similarity(rating_matrix[i,], rating_matrix[j,], diff=diff)      
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

#make rating matrix more sparse(use sokind of random values)
sparse_matrix <- function(matrix, sparsity) {
   
}

matrix_apply <- function(m, f) {
  m2 <- m
  for (r in seq(nrow(m2)))
    for (c in seq(ncol(m2)))
      m2[[r, c]] <- f(r, c)
  return(m2)
}

r <- as(MovieLense, "matrix") # extracting movielense 100k as a matrix
distance_r <- as.dist(create_distance_matrix(r)) # creating distance matrix for r (sooooo sloooow)
num_clusters <- 30

distance_r[is.na(distance_r)] <- 0 # replace on NA (TODO: where the fuck they a from??)


num_clusters <- 10

FANNY <- fanny(distance_r, num_clusters, maxit = 200)

clustering <- FANNY['clustering']

membership <- FANNY['membership'][[1]]

best_clust <- function(array, degree=0.8) {
  new_array <- sort(array, decreasing = TRUE)
  res <- 1
  sum <- 0
  repeat {
    sum <- sum + new_array[res]
    if (sum >= degree) {
      break
    } else {
      res <- res + 1
    }
  }
  return(new_array[1:res])
}

cluster_by_membership_degree <- function(memberships, degree = 0.3) {
  
  memberships > degree
  
}

# creates new matrix leaving only rows who belong to the clusters, (TEST 10)
cut_data_matrix <- function(data, clustering, clusters, rownum) {
  result <- data[rownum,]
  for (i in 1:nrow(data)) {
    if (i != rownum && clustering[i] %in% clusters) {
      result <- rbind(result, data[i,])
    }
  }
  return (result)
}

estimate_user <- function(matrix, row_number, distance_matrix) {
  
  if (nrow(matrix) == 1) { # matrix = vector (shit happens)
    result <- matrix
    print("fuck")
    mean_user_rating <- mean(matrix[1,], na.rm = TRUE)
    print(mean_user_rating)
    for (i in 1: ncol(matrix)) {
      if (is.na(matrix[1,i])) {
        result[1,i] <- mean_user_rating
      }
    }
    print(result)
    return(result)
  } 
  
  estimated_matrix <- matrix
  print(estimated_matrix)
  mean_user_ratings <- apply(estimated_matrix, 1, mean, na.rm = TRUE)
  mean_rating <- mean_user_ratings[row_number]
  
  for (l in 1: ncol(estimated_matrix)) {
    
    
    if (l %in% unrated_items) { # estimate missing rating r_{1l} - only for current user
      
      users_who_rated_item_l <- apply(estimated_matrix, 1, function(x) { return (!is.na(x[l]))})
      
      mean_user_ratings_who_rated_item_l <- mean_user_ratings[users_who_rated_item_l] 
      item_l_ratings <- estimated_matrix[users_who_rated_item_l, l]  
      
      diff_ratings <- item_l_ratings - mean_user_ratings_who_rated_item_l
      dist_users_who_rated_item_l <- distance_matrix[1, users_who_rated_item_l]
      sum_dist <- sum(abs(dist_users_who_rated_item_l)) # denominator
      
      estimated_rating <- (dist_users_who_rated_item_l  * diff_ratings) / sum_dist
      
      estimated_matrix[row_number, l] <- mean_rating + estimated_rating
    } 
  } 
  return(estimated_matrix[row_number])
}

# predicts ratings(TEST 20)
# data - rating matrix, users = rows, items = columns
predict <- function(data, clust_num = 2, total_clust_num = 10, clust_weight = 0.7, max_iterations = 5, cluster_shift = -1) {
  
  result_matrix <- c()
  
  #evaluating default clustering
  default_distance_matrix <- create_distance_matrix(data)
  default_fanny <- fanny(as.dist(default_distance_matrix), total_clust_num, maxit = 20000)
  default_clustering <- default_fanny['clustering']
  default_membership <- default_fanny['membership'][[1]]
  default_clusters <- apply(default_membership, 1, best_clust)
  
  
  for (i in 1:nrow(data)) {
    number_of_clusters <- length(default_clusters[[i]]) # double [[]] because it is list(fuck R typing)
    estimated_matrix <- data
    clustering <- default_clustering
    membership <- default_membership
    clusters <- default_clusters
    row_number <- i # stupid parameter to look after the current user, first it is now nu,ber i but after reconstructing matrix
    # we move him(her) to the first row
    iteration <- 0
    
    unrated_items <- c(1:ncol(data))[is.na(data[i,])]
    
    max_clust_num <- total_clust_num
    
    repeat { # estimate ratings 
      
      estimated_matrix <- cut_data_matrix(estimated_matrix, clustering, default_clusters[[i]], row_number)
      row_number <- 1
      good_users <- default_clustering %in% clusters # we don't compute dist matrix again but get it from existing one by leaving only those users who are in good clusters 
      distance_matrix <- default_distance_matrix[good_users, good_users]
      
      if (is.null(dim(estimated_matrix))) {
        estimated_matrix <- t(as.matrix(estimated_matrix))
      }
      new_estimated_matrix <- estimated_matrix

      new_estimated_matrix[1,] <- estimate_user(estimated_matrix, 1, distance_matrix)
        
      if (number_of_clusters <= max_clust_num || iteration == max_iterations) { # reached good clusterisation accuracy
        break
      } else { #and recompute clustering and get new number_of_user_clusters
        max_clust_num <- number_of_clusters + cluster_shift
        for (k in 2: nrow(estimated_matrix)) {
          new_estimated_matrix[k,] <- estimate_user(estimated_matrix, k, distance_matrix)
        }
        
        distance_matrix <- create_distance_matrix(new_estimated_matrix)
        fanny <- fanny(as.dist(distance_matrix), total_clust_num, maxit = 20000)
        clustering <- fanny['clustering']
        membership <- fanny['membership'][[1]]
        clusters <- apply(membership, 1, best_clust)
        number_of_clusters <- length(membership, 1, best_clust)[[1]]
        iteration <- iteration + 1
        estimated_matrix <- new_estimated_matrix
      } 
    }
    result_matrix <- rbind(result_matrix, new_estimated_matrix[1,])
    print(result_matrix)  
  }
  return(result_matrix)  
}
#==========================TEST 30
# root mean squared error
RMSE <- function(original, estimated, test) {
  
  test_size <- sum(is.na(original))
  result_summ <- 0
  for (i in 1: nrow(original)) {
    for (j in 1: ncol(original)) {
      if (is.na(original[i,j])) {
        result_summ <- result_summ + (estimated[i, j] - test[i, j]) ^ 2
      }  
    }
  }
  
  return(sqrt((1/test_size) * result_summ))
  
}

#==========================TEST 40
#mean absolute error
MAE <- function(original, estimated, test) {
  
  test_size <- sum(is.na(original))
  result_summ <- 0 
  for (i in 1: nrow(original)) {
    for (j in 1: ncol(original)) {
      if (is.na(original[i,j])) {
        result_summ <- result_summ + abs(estimated[i, j] - test[i, j])
      }  
    }
  }
  return(sqrt((1/test_size) * result_summ))
}

#==========================TEST 50
#make matrix more sparse setting random not NA elemants to NA
#NOTE it ususes random rumber to choose if to drop number so you know...
sparse_matrix <- function(matrix, sparsity) {
  if (sparsity <= 0 || sparsity >= 1) {
    print("sparsity in (0,1) you stupid")
    return (matrix)
  }
  values <- c(TRUE, FALSE)
  prob <- c(sparsity, 1 - sparsity)
  
  return(apply(matrix, 1:2, function(x) { if (sample(c(values, prob))[1]) { return (x) }  else {return(NA)}}))

}
