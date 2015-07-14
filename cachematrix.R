## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## self-contained inverse varialble matrix object
## empty matrix is a default initialization.
## return a list of self-contain set and get functions
makeCacheMatrix <- function(x = matrix()) {
   
   inv <- NULL # inverse variable - to be computed 
   
   # value assignment function
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   # value retrieve function
   get <- function() {
      x
   }
   
   # inverse value assignment function
   setInverse <- function(val) {
      inv <<- val
   }
   
   # retrieve inverse value function
   getInverse <- function() {
      inv
   }
   
   # return a list of set, get, setInverse and getInverse functions
   list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## Write a short comment describing this function
## function to compute inverse matrix
## input is self-contained inverse matrix value
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   
   inv <- x$getInverse() # retrieve computed inverse value form matrix, if any.
   
   if(!is.null(inv)) { # if inv has already been computed, then return
      message("getting cached data")
      return(inv)
   }
   
   data <- x$get()
   inv <- solve(data, ...) # solve - R inverse matrix function
   x$setInverse(inv) # set the computed inverse value back to the matrix
   inv
}
