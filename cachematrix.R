## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix: This function creates a square invertible matrix and returns a list of
## functions that will be used as input to the cachesolve function

makeCacheMatrix <- function(x = matrix()) {
  
  ## @x: a square invertible matrix
  ## return: a list containing functions to
  ##  1. set the matrix
  ##  2. get the matrix
  ##  3. set the inverse
  ##  4. get the inverse
  ## this list is used as the input to cacheSolve()
  
  inv = NULL
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if the inverse
##has already been calculated. If so, it gets the inverse from the cache 
##and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.
cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv = x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
