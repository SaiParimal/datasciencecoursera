## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set_ip <- function(p) {
    x <<- p
    i <<- NULL
  }
  get_ip <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set_ip = set_ip,
       get_ip = get_ip,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if (!is.null(i)) {
    message("fetching cached data")
    return(i)
  }
  data <- x$get_ip()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
