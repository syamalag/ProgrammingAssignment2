## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function can be used to create a secial matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) { ## change the vector stored in the main function
            x <<- y
            Inv <<- NULL
      }
      get <- function() x ## return the vector stored in the main function
      setinverse <- function(solve) Inv <<- solve
      getinverse <- function() Inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function
## This function is used to create inverse of the special matrix returned by makeCacheMatrix function

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'
        ## Check if the Inverse is existing in cache
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
        }
	
	## Create Inverse if it is not existing in chache
	data <- x$get()
	Inv <- solve(data, ...)
	x$setinverse(Inv)
	Inv

}
