## Put comments here that give an overall description of what your
## functions do

## Below are two functions that are used to create a special object that 
## stores a numeric matrix and cache's its Inverse.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function calculates the Inverse of a special matrix created with the above 
# function. However, it first checks to see if the Inverse has already been calculated. If so, 
## it gets the Inverse from the cache and skips the computation. Otherwise, it calculates the 
# INverse of the matrix and sets the value of the Inverse in the cache via the setInv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    newMatrix <- x$get()
    m <- solve(newMatrix)
    x$setInv(m)
    m
}  
  