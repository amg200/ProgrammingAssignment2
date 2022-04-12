## Use cache to make matrix operations more efficient

## Create the "cache matrix" structure

makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(Y) {
    X <<- Y
    M <<- NULL
  }
  get <- function() X
  setInv <- function(solve) X <<- solve
  getInv <- function() M
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Return the inverse of x: if the result has been calculated before return
## the cached matrix otherwise calculate and store the inverse before returning

cacheSolve <- function(X, ...) {
  ## Return a matrix that is the inverse of 'x'
  M <- X$getInv()
  if(!is.null(M)) {
    message("getting cached data")
    return(M)
  }
  data <- X$get()
  M <- solve(data, ...)
  X$setInv(M)
  M
}
