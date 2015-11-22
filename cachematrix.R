## Create a cache object which holds matrix and its inversed value 

makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  get <- function() x
  setcache <- function(calculated) cache <<- calculated
  getcache <- function() cache
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


## Get inversed matrix either from cache if it's available or by calculating it

cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getcache()
  if(!is.null(inv)) {
    #print("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setcache(inv)
  inv
}

## you may check my code by removing comments and debugging the code below:

## make cache object
#cm <- makeCacheMatrix(m)
#n = 1000
## create random inversible matrix
#m = diag(runif(n))
## cache the matrix
#cm$set(m)
#inv1 <- cacheSolve(cm)
## this call goes into a cache
#inv2 <- cacheSolve(cm)

