## Put comments here that give an overall description of what your
## functions do
## You can test this function using the following matrix
## A<-matrix(c(7,2,1,0,3,-1,-3,4,-2), nrow = 3,ncol = 3, byrow = TRUE)
## B<-makeCacheMatrix(A)
## C<-cacheSolve(B)
## C and B$getinverse() are expected to be the same.

## This function "wraps" the original matrix (parameter x)
## It returns a list with 4 functions:
## get/set operate are getting or setting the matrix
## getinverse/setinverse are getting or setting the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) s<<-inverse
  getinverse <- function() s 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function operates on "wrapped" matrix (list that was created by the function makeCacheMatrix)
## It first checks if inverse was already calculated.
## If so, it returns the result.
## Otherwise, it calculates the inverse and stores it to the "wrapped" matrix for future use.
cacheSolve <- function(x) {
  ## Return a matrix that is the inverse of 'x'
		
	s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setinverse(s)
  s
}
