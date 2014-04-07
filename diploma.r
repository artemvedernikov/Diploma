# My wonderful diploma project recomends you not to deal with science

install.packages("recommenderlab")
library(recommenderlab)

data(MovieLense) # MovieLens 100k dataset

# Some stupid stuff for presentation

## visualize part of the matrix
image(MovieLense[1:100,1:100])
## number of ratings per user
hist(rowCounts(MovieLense), main=NULL, xlab='number of users', ylab='number of ratings')
## number of ratings per movie
hist(colCounts(MovieLense), main=NULL, xlab='number of movies', ylab='number of ratings')
## mean rating (averaged over over users)
mean(rowMeans(MovieLense))
## data sparsity
r <- as(MovieLense, "matrix")
sum(is.na(r) == FALSE) / (dim(r)[1] * dim(r)[2]) # TODO: is there a better way to find number of elements in matrix? 

## My recommender 

ITERATIVE_CLASSIFICATION <- function(data, parameter = NULL) {
  
  p <- .get_parameters(list(
    normalize="center",
    aggregation=colSums
    ), parameter)
  
  ## normalize data
  if(!is.null(p$normalize)) data <- normalize(data, method=p$normalize)
  
  
  
}





## Test recommender systems

movies <- normalize(MovieLense)
 
# roc curves etc

scheme<- evaluationScheme(MovieLense, method="cross-validation", k=20, given = 3, goodRating = 3)

algorithms <- list("popular items" = list(name="POPULAR", param=NULL),
                   "user-based-CF" = list(name="UBCF", param=list(method="Cosine", nn=50, minRating=3)),
                   "item-based-CF" = list(name="IBCF", param=list(method="Cosine", minRating=3)),
                   "SVD" = list(name="SVD", param=list(method="Cosine")))

results <- evaluate(scheme, algorithms, n=c(1,3,5,10,15))

# rmse

r1 <- Recommender(getData(scheme, "train", 1), "POPULAR")
r2 <- Recommender(getData(scheme, "train", 1), "UBCF")
r3 <- Recommender(getData(scheme, "train", 1), "IBCF")
r4 <- Recommender(getData(scheme, "train", 1), "SVD")

p1 <- predict(r1, getData(scheme, "known", 1), type="ratings")
p2 <- predict(r2, getData(scheme, "known", 1), type="ratings")
p3 <- predict(r3, getData(scheme, "known", 1), type="ratings")
p4 <- predict(r4, getData(scheme, "known", 1), type="ratings")

error <- rbind(
  calcPredictionError(p1, getData(scheme, "unknown", 1)),
  calcPredictionError(p2, getData(scheme, "unknown", 1)),
  calcPredictionError(p3, getData(scheme, "unknown", 1)),
  calcPredictionError(p4, getData(scheme, "unknown", 1))
)
error





