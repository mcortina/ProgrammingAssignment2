## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix stores a matrix to use in combination with cacheSolve function.
## It provides methods to set and get matrix data.
## The matrix is stored in its parent enviroment to serve as cache storage and
## avoid a calculation already done.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_inverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
  get_inverse_matrix <- function() m
  list(set = set, get = get,
       set_inverse_matrix = set_inverse_matrix,
       get_inverse_matrix = get_inverse_matrix)
}


## cacheSolve uses a makeCacheMatrix object as argument to calculate its inverse.
## Before to proceed with that calculation it checks if it has cached,
## that is, if the calculation has already been done previously.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse_matrix(m)
  m
}


# Test
# ----------------
# This matrix:
#    1   2   3
#    0   1   4
#    5   6   0
# has as inverse:
#  -24  20  -5
#   18 -15   4
#    5  -4   1
#
# Let's see if it works.
#
# 1.- Create matrix variable:
m <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3)
# 2.- Pass matrix variable to makeCacheMatrix function:
x1 <- makeCacheMatrix(m)
# 3. Check the matrix is in stored
x1$get()
# 4. Solve and get inverse
cacheSolve(x1)
# 5. Solve again inverse.
# cacheSolve should return cached value since it has been previously calculated.
cacheSolve(x1)
