# tests for functions

r <- as(MovieLense, "matrix") # extracting movielense 100k as a matrix
distance_r <- as.dist(create_distance_matrix(r)) # creating distance matrix for r (sooooo sloooow)
num_clusters <- 30

distance_r[is.na(distance_r)] <- 0 # replace on NA (TODO: where the fuck they a from??)

#====
#strabge_sim
strange_similarity(c(1,2,3), c(1,1,NA))


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
#mae
result_data <- data
result_data[is.na(result_data)] <- 5

test_data <- result_data
test_data[1,1] <- 1

mae(data, result_data, test_data)

#=========
#========TEST(40)====
MAE(data, result_data, test_data)

#========
#=======TEST(50)=====
sparse_matrix(test_data, 0.1)


#=== first test results(all standart)
testA <- sparse_matrix(r,0.1)
testB <- sparse_matrix(r,0.2)
testC <- sparse_matrix(r,0.3)
testD <- sparse_matrix(r,0.4)
testE <- sparse_matrix(r,0.5)
testF <- sparse_matrix(r,0.6)

pedA <- predict(testA, total_clust_num = 15)
pedB <- predict(testB, total_clust_num = 15)
pedC <- predict(testC, total_clust_num = 15)
pedD <- predict(testD, total_clust_num = 15)
pedE <- predict(testE, total_clust_num = 15)
pedF <- predict(testF, total_clust_num = 15)
#=== seconds test results
"2 clust cosine"
Sys.time()
pedAcos <- predict(testA, total_clust_num = 15, method="cosine")
Sys.time()
pedBcos <- predict(testB, total_clust_num = 15, method="cosine")
Sys.time()
pedCcos <- predict(testC, total_clust_num = 15, method="cosine")
Sys.time()
pedDcos <- predict(testD, total_clust_num = 15, method="cosine")
Sys.time()
pedEcos <- predict(testE, total_clust_num = 15, method="cosine")
Sys.time()
pedFcos <- predict(testF, total_clust_num = 15, method="cosine")

"2 clust strange"
Sys.time()
pedAstr <- predict(testA, total_clust_num = 15, method="strange")
Sys.time()
pedBstr <- predict(testB, total_clust_num = 15, method="strange")
Sys.time()
pedCstr <- predict(testC, total_clust_num = 15, method="strange")
Sys.time()
pedDstr <- predict(testD, total_clust_num = 15, method="strange")
Sys.time()
pedEstr <- predict(testE, total_clust_num = 15, method="strange")
Sys.time()
pedFstr <- predict(testF, total_clust_num = 15, method="strange")
"3 clust pearson"
#=====
Sys.time()
pedAthree <- predict(testA, clust_num = 3)
Sys.time()
pedBthree <- predict(testB, clust_num = 3)
Sys.time()
pedCthree <- predict(testC, clust_num = 3)
Sys.time()
pedDthree <- predict(testD, clust_num = 3)
Sys.time()
pedEthree <- predict(testE, clust_num = 3)
Sys.time()
pedFthree <- predict(testF, clust_num = 3)
"3 clust cosine"
Sys.time()
pedAcosthree <- predict(testA, total_clust_num = 15, method="cosine")
Sys.time()
pedBcosthree <- predict(testB, total_clust_num = 15, method="cosine")
Sys.time()
pedCcosthree <- predict(testC, total_clust_num = 15, method="cosine")
Sys.time()
pedDcosthree <- predict(testD, total_clust_num = 15, method="cosine")
Sys.time()
pedEcosthree <- predict(testE, total_clust_num = 15, method="cosine")
Sys.time()
pedFcosthree <- predict(testF, total_clust_num = 15, method="cosine")

"3 clust strange"
Sys.time()
pedAstrthree <- predict(testA, clust_num = 3, method="strange")
Sys.time()
pedBstrthree <- predict(testB, clust_num = 3, method="strange")
Sys.time()
pedCstrthree <- predict(testC, clust_num = 3, method="strange")
Sys.time()
pedDstrthree <- predict(testD, clust_num = 3, method="strange")
Sys.time()
pedEstrthree <- predict(testE, clust_num = 3, method="strange")
Sys.time()
pedFstrthree <- predict(testF,clust_num = 3, method="strange")


rmseA <- RMSE(testA, pedA, r)
rmseB <- RMSE(testB, pedB, r)
rmseC <- RMSE(testC, pedC, r)
rmseD <- RMSE(testD, pedD, r)
rmseE <- RMSE(testE, pedE, r)
rmseF <- RMSE(testF, pedF, r)

maeA <- MAE(testA, pedA, r)
maeB <- MAE(testB, pedB, r)
maeC <- MAE(testC, pedC, r)
maeD <- MAE(testD, pedD, r)
maeE <- MAE(testE, pedE, r)
maeF <- MAE(testF, pedF, r)


rmseAcos <- RMSE(testA, pedAcos, r)
rmseBcos <- RMSE(testB, pedBcos, r)
rmseCcos <- RMSE(testC, pedCcos, r)
rmseDcos <- RMSE(testD, pedDcos, r)
rmseEcos <- RMSE(testE, pedEcos, r)
rmseFcos <- RMSE(testF, pedFcos, r)

maeAcos <- MAE(testA, pedAcos, r)
maeBcos <- MAE(testB, pedBcos, r)
maeCcos <- MAE(testC, pedCcos, r)
maeDcos <- MAE(testD, pedDcos, r)
maeEcos <- MAE(testE, pedEcos, r)
maeFcos <- MAE(testF, pedFcos, r)


testData <- sparse_matrix(data, 0.1)
