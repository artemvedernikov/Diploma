# tests for functions

r <- as(MovieLense, "matrix") # extracting movielense 100k as a matrix
distance_r <- as.dist(create_distance_matrix(r)) # creating distance matrix for r (sooooo sloooow)
num_clusters <- 30

distance_r[is.na(distance_r)] <- 0 # replace on NA (TODO: where the fuck they a from??)




FANNY <- fanny(distance_r, 10, maxit = 200)

clustering <- FANNY['clustering']

membership <- FANNY['membership'][[1]]


parameter <- 0.9


num <- apply(membership, 1, num_of_clust)
#========(10)=====
"test 10"
"cut_data_matrix_test"

data <- c(1,2,3)
data <- rbind(data, c(2,3,4))
data <- rbind(data, c(3,4,5))

clustering <- c(1,2,3)
clusters <- c(1,2)
rownum = 1


result <- cut_data_matrix(data, clustering, clusters, rownum)
result[1,] == c(1,2,3)
result[2,] == c(2,3,4)
#=========
#========TEST(2)====
#predict
data <- c()
data <- rbind(data, c(NA,1,2,NA,NA,NA,5,5,5))
data <- rbind(data, c(5,5,NA,3,3,4,NA,1,1))
data <- rbind(data, c(NA,5,NA,2,4,NA,1,3,2))
data <- rbind(data, c(1,NA,NA,1,2,3,4,NA,1))
data <- rbind(data, c(5,NA,5,5,NA,4,3,2,2))
data <- rbind(data, c(5,NA,5,5,NA,4,NA,2,NA))
data <- rbind(data, c(3,4,NA,2,NA,3,5,5,NA))

predict(data, total_clust_num = 2)

test_result_100k <- predict(r, total_clust_num = 15)
#========
#========TEST(30)====
#RMSE
result_data <- data
result_data[is.na(result_data)] <- 5

test_data <- result_data
test_data[1,1] <- 1

RMSE(data, result_data, test_data)

#=========
#========TEST(40)====
MAE(data, result_data, test_data)

#========
#=======TEST(50)=====
sparse_matrix(test_data, 0.1)

