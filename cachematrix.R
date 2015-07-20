## Put comments here that give an overall description of what your
## functions do

## This function creates some kind "matrix" that contains cache
## inverse for given matrix if it was already sent to cacheSolve
## or write it to cache if it wasn't

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  set <- function(x) {
    mt_inv <<- x
    m_inv <<- NULL
  }
  get <- function() mt_inv
  setinv <- function(invert) m_inv <<- invert
  getinv <- function() m_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## function calculates the inverse of the "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse of  the data and sets the inverse matrix

cacheSolve <- function(x, ...) {
  m_inv <- x$getinv()
  if(!is.null(m_inv)) {
    message("getting cached data")
    return(m_inv)
  }
  data <- x$get()
  m_inv <- solve(data, ...)
  x$setinv(m_inv)
  m_inv
  ## Return a matrix that is the inverse of 'x'
}
