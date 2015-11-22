## Write a short comment describing this function

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


## Write a short comment describing this function

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
## put matrix into a cache
#cm$set(m)
#inv1 <- cacheSolve(cm)
## this call goes into a cache
#inv2 <- cacheSolve(cm)

