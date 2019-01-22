##  this is the R file for the commitment of programming assignment 2 week3
##  the R file contains 2 functions the makeCacheMatrix and the cacheSolve function

## makeCacheMatrix function set the value of the initial matrix, set the inverse matrix to NULL  
## It also  define set and get function for the original matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  ##  S variable to cach inversed matrix
    s <- NULL
    ##  Set function 
    set <- function(y) {
      x <<- y
      s <<- NULL
    }
    ## get function 
    get <- function() x
    ##  set inverse function 
    setinverse <- function(inverse) s <<- inverse
    ##  get inverse function 
    getinverse <- function() s
    ##  list of associated function 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Cashsolve function  look for a cached value of inverse matrix otherwise calculate it with solve() function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## calculate the inverse matrix
  data <- x$get()
  s <- solve(data)
  
  x$setinverse(s)
  s

}
