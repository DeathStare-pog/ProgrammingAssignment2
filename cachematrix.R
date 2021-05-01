## Put comments here that give an overall description of what your
## functions do

## creates matrix 

makeCacheMatrix <- function(x = matrix()) 
  {
  inverse <- NULL
  set <- function(y)
    {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(smat) inverse <<- smat
  getInverse <- function() inverse 
  list(set = set, get = get,setInverse = setInverse,getInverse = getInverse)

}


## function gives the inverse of matrix returned by above function

cacheSolve <- function(x, ...) 
  {
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  inverse
        ## Return a matrix that is the inverse of 'x'
}
