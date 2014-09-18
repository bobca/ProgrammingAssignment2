## makeCacheMatrix takes a square, 2-D matrix and caches it. cacheSolve gets this matrix and calculates the inverse (no 
## test for valid inverse is made). If a cached inverse exists, it is retrieved, not recaclulated.


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL  	## resets inverse to NULL
  
  set <- function(y) {		## set is fn that puts new vector into x & sets inverse to NULL
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x		## get is a fn that retrieves the matrix
  setinverse <- function(inverse) inv <<- inverse		## setinverse stuffs its var (inverse) into inv cache
  getinverse <- function() inv		## getinverse is fn that retrieves the inverse
    
  list(set = set, get = get,		## this is the output vector, a group of functions
       setinverse = setinverse,
       getinverse = getinverse)


}


## cacheSolve first tests if a caclulation is needed, returning the cached value. Otherwise inverse is calc's an set.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
inv <- x$getinverse()  	## gets inverse cache val and stuffs to inv
  
  if(!is.null(inv)) {		## tests for NULL and returns cahced inverse if no NULL
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()		## no cached inverse, so calc it; this step gets the matrix
  inv <- solve(data)
  x$setinverse(inv)		## sets inverse to cache
  inv	 
}
