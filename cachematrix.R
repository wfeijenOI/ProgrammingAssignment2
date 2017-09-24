## Put comments here that give an overall description of what your
## functions do

## Example code
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

v1 <- makeVector(1:15)
v1$getmean()
v1$get()
v1$set(30:50)
cachemean(v1)
v1$getmean()

## Write a short comment describing this function
# This function makes an underlying set of functions
# The underlying functions are to set and get variables which are cached in the scope of the function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




## Write a short comment describing this function
# This function is what gives makeCacheMatrix its meaning
# It invoces the expensive inverse operation when the variable x$m (the cache for the inverse result) 
# is empty. x$m is not visible to the outside world so it is returned using a get function
# If m is not empty it symply returns the value of m
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #Computing the inverse of a square matrix can be done with the `solve`
  #function in R. For example, if `X` is a square invertible matrix, then
  #`solve(X)` returns its inverse.
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

m1<- makeCacheMatrix(matrix( c(2, 4, 3, 1),  nrow=2,   ncol=2) )
m1$getInverse()
m1$set(matrix( c(0, 1, 3, 1),  nrow=2,   ncol=2))
cacheSolve(m1)
m1$getInverse()

