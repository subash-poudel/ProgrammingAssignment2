## Function to calculate Inverse of a matrix 
## with caching of intermidiate values

## Calculates the inverse of the matrix
## Argument x is matrix to inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize initialize
  inverseMatrix <- NULL
  
  set <- function(y){
    x <<- y
    inverseMatrix <<- NULL
  }
  
  ## function responsible for getting or setting 
  ## values of the inverse matrix
  
  get <- function() x
  setInverseMatrix <- function(solve) inverseMatrix <<- solve
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)

}


## This function is for calculating the inverse of the matrix

cacheSolve <- function(x, ...) {
    
  ## Check if the inverse is already calculated
    ## If already calculated return cached value
    ## If not calculate and set the inverse of the matrix
  m <- x$getInverseMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  m
  
}
