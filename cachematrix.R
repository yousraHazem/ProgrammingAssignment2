## Put comments here that give an overall description of what your
## functions do

## This method serves as a constructor for the cache matrix. Cache matrix has
## an attribute inv that stores the inverse of matrix x once it's set. whenever
## x is modified via the set method inv is nulified.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  get_inv <- function() inv
  set_inv <- function(new_inv) inv <<- new_inv
  list(set = set, get = get, get_inv = get_inv, set_inv = set_inv)
}


## cacheSolve function solves for the cahed matrix x. If x had its inverse 
## calculated before and x hasn't been modified the cached value is returned. 
## Otherwise the inverse is computed and cached then returned. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inv()
  if(!is.null(inv)){
    message('getting cached inverse')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$set_inv(inv)
  inv
}
