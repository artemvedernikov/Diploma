

# predicts ratings(TEST 20)
# data - rating matrix, users = rows, items = columns
predict <- function(data, test = NULL, clust_num = 2, total_clust_num = 10, clust_weight = 0.7, max_iterations = 5, cluster_shift = -1, min_clust_size =  10,method = "pearson") {
  print(Sys.time())
  print("started")
  result_matrix <- c()
  
  if (!is.null(test)) {
    print("testing set found")
    print("estimating only users in test set")
    users_to_estimate = nrow(test)
  } else {
    print("testing set not found")
    print("estimateing all users")
    users_to_estimate = nrow(data)
  }
  
  data <- rbind(test, data)
  
  default_similarity_matrix <- (create_similarity_matrix(data, method = method)) 
  default_distance_matrix <- 1 / (default_similarity_matrix + 1.001)
  default_fanny <- fanny(as.dist(default_distance_matrix), total_clust_num, maxit = 20000, memb.exp = 1.5)
  default_clustering <- default_fanny['clustering'][[1]]
  default_membership <- default_fanny['membership'][[1]]
  default_clusters <- lapply(split(default_membership, seq(nrow(default_membership))), best_clust)#  tut huinia!!!!!
  
  print(Sys.time())
  print("preprocessing finished")
  
  for (i in 1:users_to_estimate) {
    number_of_clusters <- length(default_clusters[[i]]) # double [[]] because it is list(fuck R typing)
    
    ## moving estimated user to the first row to the sake of convenience
    estimated_matrix <- rbind(data[i,], data[-i,])
    clustering <- c(default_clustering[i],default_clustering[-i])
    membership <- rbind(default_membership[i,], default_membership[-i,])
    clusters <- default_clusters[[i]]
    similarity <- c(default_similarity_matrix[i,i], default_similarity_matrix[i,-i])
    
    
    
    tmp <- default_similarity_matrix[-i,-i]
    
    similarity_matrix <- rbind(similarity[-1], tmp)
    similarity_matrix <- cbind(similarity, similarity_matrix)
    
    
    iteration <- 0
    
    max_clust_num <- clust_num
    
    ratings_to_estimate <- is.na(estimated_matrix[1,])
    
    repeat { # estimate ratings 

      copy <- estimated_matrix
      estimated_matrix <- cut_data_matrix(estimated_matrix, clustering, clusters)
      good_users <- clustering %in% clusters # we don't compute dist matrix again but get it from existing one by leaving only those users who are in good clusters 
      similarity <- similarity[good_users]
      
      
      
      similarity_matrix <- similarity_matrix[good_users, good_users] 

      if (is.null(dim(estimated_matrix))) {
        estimated_matrix <- t(as.matrix(estimated_matrix))
      }
      new_estimated_matrix <- estimated_matrix

      mean_user_ratings <- apply(estimated_matrix, 1, mean, na.rm = TRUE)
      mean_user_ratings[is.na(mean_user_ratings)] <- 0 ## hack for users with no ratings at all!!!
      
      users_who_rated <- !apply(estimated_matrix, 1:2, is.na)
      
      new_estimated_matrix[1,] <- estimate_user(estimated_matrix, 1, similarity_matrix[1,], mean_user_ratings, users_who_rated, ratings_to_estimate, 0, FALSE, similarity_matrix)  #CHECK IF similarity_matrix is correct(or estimated_matrix)))
      if (number_of_clusters <= max_clust_num || iteration == max_iterations || nrow(estimated_matrix) <= min_clust_size) { # reached good clusterisation accuracy or fuck stop or it would last forever
        print(i)
        print(iteration)
        print("=============")
        break
      } else { #and recompute clustering and get new number_of_user_clusters
        max_clust_num <- number_of_clusters + cluster_shift
        for (k in 2: nrow(estimated_matrix)) {

          new_estimated_matrix[k,] <- estimate_user(estimated_matrix, k, similarity_matrix[k,], mean_user_ratings, users_who_rated, is.na(estimated_matrix[k,]), 2, TRUE)
        }
        
        similarity_matrix <- create_similarity_matrix(new_estimated_matrix, method = method)
        distance_matrix <- 1 / (similarity_matrix + 1.001)
        fanny <- fanny(as.dist(distance_matrix), total_clust_num, maxit = 20000, memb.exp = 1.5)
        clustering <- fanny['clustering'][[1]]
        membership <- fanny['membership'][[1]]
        clusters_all <- lapply(split(membership, seq(nrow(membership))), best_clust)
        clusters <- clusters_all[[1]]
        number_of_clusters <- length(clusters[[1]])
        iteration <- iteration + 1
        estimated_matrix <- new_estimated_matrix
        similarity <- similarity_matrix[1,]
        
      } 
    }
    result_matrix <- rbind(result_matrix, new_estimated_matrix[1,])
  }
  return(result_matrix)  
}