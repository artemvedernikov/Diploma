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
testA <- sparse_matrix(r,0.8)
testB <- sparse_matrix(r,0.2)
testC <- sparse_matrix(r,0.3)
testD <- sparse_matrix(r,0.4)
testE <- sparse_matrix(r,0.5)
testF <- sparse_matrix(r,0.6)

testA <- testA[ncol(testA) - sum(is.na(testA)) > 3,]
testB <- testB[ncol(testB) - sum(is.na(testB)) > 3,]
testC <- testC[ncol(testC) - sum(is.na(testC)) > 3,]
testD <- testD[ncol(testD) - sum(is.na(testD)) > 3,]
testE <- testE[ncol(testE) - sum(is.na(testE)) > 3,]
testF <- testF[ncol(testF) - sum(is.na(testF)) > 3,]

Sys.time()
pedA <- predict(testA, total_clust_num = 15)
Sys.time()
pedB <- predict(testB, total_clust_num = 15)
Sys.time()
pedC <- predict(testC, total_clust_num = 15)
Sys.time()
pedD <- predict(testD, total_clust_num = 15)
Sys.time()
pedE <- predict(testE, total_clust_num = 15)
Sys.time()
pedF <- predict(testF, total_clust_num = 15)
#=== seconds test results
"2 clust cosine"
Sys.time()
pedAcos <- predict(testA[1:100,], total_clust_num = 6, method="cosine")
Sys.time()
pedBcos <- predict(testB[1:100,], total_clust_num = 6, method="cosine")
Sys.time()
pedCcos <- predict(testC[1:100,], total_clust_num = 6, method="cosine")
Sys.time()
pedDcos <- predict(testD[1:100,], total_clust_num = 6, method="cosine")
Sys.time()
pedEcos <- predict(testE[1:100,], total_clust_num = 6, method="cosine")
Sys.time()
pedFcos <- predict(testF[1:100,], total_clust_num = 6, method="cosine")
Sys.time()

"3 clust pearson"
#=====
pedAthree <- predict(testA[1:100,], clust_num = 3, total_clust_num = 6)
Sys.time()
pedBthree <- predict(testB[1:100,], clust_num = 3, total_clust_num = 6)
Sys.time()
pedCthree <- predict(testC[1:100,], clust_num = 3, total_clust_num = 6)
Sys.time()
pedDthree <- predict(testD[1:100,], clust_num = 3, total_clust_num = 6)
Sys.time()
pedEthree <- predict(testE[1:100,], clust_num = 3, total_clust_num = 6)
Sys.time()
pedFthree <- predict(testF[1:100,], clust_num = 3, total_clust_num = 6)
"3 clust cosine"


Sys.time()
pedAcosthree <- predict(testA[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)
Sys.time()
pedBcosthree <- predict(testB[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)
Sys.time()
pedCcosthree <- predict(testC[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)
Sys.time()
pedDcosthree <- predict(testD[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)
Sys.time()
pedEcosthree <- predict(testE[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)
Sys.time()
pedFcosthree <- predict(testF[1:100,], clust_num = 3, method="cosine", total_clust_num = 6)


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
# test predict
predict(data, total_clust_num = 2)


Sys.time()
pedAl <- predict(testA[1:100,], total_clust_num = 6)
Sys.time()
pedBl <- predict(testB[1:100,], total_clust_num = 6)
Sys.time()
pedCl <- predict(testC[1:100,], total_clust_num = 6)
Sys.time()
pedDl <- predict(testD[1:100,], total_clust_num = 6)
Sys.time()
pedEl <- predict(testE[1:100,], total_clust_num = 6)
Sys.time()
pedFl <- predict(testF[1:100,], total_clust_num = 6)


rmseAl <- RMSE(testA[1:100,], pedAl, r[1:100,])
rmseBl <- RMSE(testB[1:100,], pedBl, r[1:100,])
rmseCl <- RMSE(testC[1:100,], pedCl, r[1:100,])
rmseDl <- RMSE(testD[1:100,], pedDl, r[1:100,])
rmseEl <- RMSE(testE[1:100,], pedEl, r[1:100,])
rmseFl <- RMSE(testF[1:100,], pedFl, r[1:100,])

maeAl <- MAE(testA[1:100,], pedAl, r[1:100,])
maeBl <- MAE(testB[1:100,], pedBl, r[1:100,])
maeCl <- MAE(testC[1:100,], pedCl, r[1:100,])
maeDl <- MAE(testD[1:100,], pedDl, r[1:100,])
maeEl <- MAE(testE[1:100,], pedEl, r[1:100,])
maeFl <- MAE(testF[1:100,], pedFl, r[1:100,])


rmseAlcosthree <- RMSE(testA[1:100,], pedAcosthree, r[1:100,])
rmseBlcosthree <- RMSE(testB[1:100,], pedBcosthree, r[1:100,])
rmseClcosthree <- RMSE(testC[1:100,], pedCcosthree, r[1:100,])

results_rmse <- c()
results_rmse <-rbind(results_rmse, c(rmseAl, NA, rmseCl,rmseDl, rmseEl, rmseFl))
results_rmse <-rbind(results_rmse, c(rmseAlcos, rmseBlcos, rmseClcos,rmseDlcos, rmseElcos, rmseFlcos))
results_rmse <-rbind(results_rmse, c(rmseAlthree, rmseBlthree, rmseClthree,rmseDlthree, rmseElthree, rmseFlthree)
results_rmse <- rbind(results_rmse, c(rmseAlcosthree, rmseBlcosthree, rmseClcosthree, NA,NA,NA))







big_ped_A = predict(testA[1:800,], test = testA[801:943,], total_clust_num = 15)
big_ped_B = predict(testB[1:800,], test = testB[801:943,], total_clust_num = 15)
big_ped_C = predict(testC[1:800,], test = testC[801:943,], total_clust_num = 15)
big_ped_D = predict(testD[1:800,], test = testD[801:943,], total_clust_num = 15)
big_ped_E = predict(testE[1:800,], test = testE[801:943,], total_clust_num = 15)
big_ped_F = predict(testF[1:800,], test = testF[801:943,], total_clust_num = 15)