## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
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
  get <- function()x 
  setinv <- function(solve) xinv <<- solve
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data)
  x$setinv(xinv)
  xinv
}

