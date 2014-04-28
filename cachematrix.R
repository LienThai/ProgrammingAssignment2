# rProgramming Peer Assessment

# The first function, makeCacheMatrix creates a  special "matrix" object that can cache its inverse"; 
# It consists of 2 vvariables and give a list of four functions: set the value of the matrix, get the 
# value of the matrix, set the value of the inverse matrix, get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  get <- function() x
  set.inv_matrix <- function(inv.matrix) inv_matrix <<- inv.matrix
  get.inv_matrix <- function() inv_matrix
  list(set = set, get = get,
       set.inv_matrix = set.inv_matrix,
       get.inv_matrix = get.inv_matrix)
}


# The second function, cacheSolve computes the inverse matrix returned by makeCacheMatrix above. 
# If the inverse matrix has not been computed i.e. invmatrix is null, then calculate the invmatrix using the solve function,
# alternatively if the inverse matrix has already been computed (and that the matrix has not been changed) then it will get
# the inverse matrix from the cache and skip the computation. 

cacheSolve <- function(x, ...) {
  invmatrix <- x$get.inv_matrix()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    # Return a matrix that is the inverse of 'x'
    return(invmatrix)
  }
  matrix.data <- x$get()
  invmatrix <- solve(matrix.data, ...)
  x$set.inv_matrix(invmatrix)
  # Return a matrix that is the inverse of 'x'
  return(invmatrix)
}
