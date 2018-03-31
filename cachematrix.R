## This function creates a special "matrix" object that can cache its inverse.B <- matrix(c(1,2,3,4),2,2)

makeCacheMatrix <- function(x = matrix()) {
    ## create initial object
    i <- NULL
    
    ## create set function which sets the values returns it to the initial environment
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## create get function which retrieves the matrix from the initial environment
    get <- function() x
    
    ## create Inverse function which inverses the matrix created by the set function
    ## and assigns it to the i object from the initial environment
    setInverse <- function(inverse) i <<- inverse
    
    ## create get functions which retrieves the inverted matrix from the initial environment
    ## it is created by the setInverse function
    getInverse <- function() i
    
    ## create a list from which we can call specific functions in the cacheSolve function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## receive the inverted matrix created by the makeCacheMatrix function and assign it to i
  i <- x$getInverse()
  
  ## if i != NULL (inverted matrix is created) then write message "getting cached data from makeCacheMatrix"
  ## and return the inverted matrix
  if(!is.null(i)) {
    message("getting cached data from makeCacheMatrix")
    return(i)
    
  ## else get the matrix and assign it to the new data object, invert the matrix and set it to i
  } else
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}