## This function creates a special "matrix" object that can cache its inverse


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL

  ## set the value of the matrix
  set <- function(y) {
    if (is.matrix(y) & length(dim(y)) ==2 & length(y[,1])==length(y[1,]) ) {
      x <<- y
      xinv <<- NULL
      } else{
        print("x is not a square matrix")
        print(y)
        return
      }
  }
  
  ## get the value of the matrix
  get <- function()x 
  
  ## set the value of the inverse matrix
  setinv <- function(solve) xinv <<- solve
  
  ## get the value of the inverse matrix
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. Otherwise, 
## it calculates the inverse of the data and sets the inverse matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  
  ## if the inverse has already been calculated, returning the cached data
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  
  ## calculates the inverse of the data and sets the inverse matrix in the cache via the setinv function
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}

