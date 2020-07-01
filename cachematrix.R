## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# The makeCacheMatrix function generates a list which contains the functions...
# required to
# set and get the original matrix and get and set the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(inputMatrix){
    x <<- inputMatrix
    i <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

#Given an inputed matrix if the inverse has not been computed before it computes 
#the inverse and caches it, else it retrieves the inverse that has already been 
#computed. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
