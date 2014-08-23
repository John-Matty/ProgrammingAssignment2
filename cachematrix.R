## This pair of functions enable you to define a matrix and calculate the inverse of that matrix.
## The inverse is then stored (cached) so that if the inverse is required later, the cached solution
## may be returned, thus saving computer time - as long as the matrix has not changed.
## John Matty 20140821

## makeCacheMatrix returns a list of functions to access the matrix argument and store its inverse

makeCacheMatrix <- function(SaveM = matrix()) {
  matt <- as.matrix(0)                                          # init matrix placeholder
  get <- function() SaveM                                       # function to return new matrix
  setSolve <- function(solve) matt <<- solve                    # function to cache solved matrix
  getInverse <- function() matt                                 # function to return cached matrix
  list(get = get, setSolve = setSolve, getInverse = getInverse)
}

## cacheSolve calls functions returned by makeCacheMatrix to see if the matrix
## in its argument has been solved (i.e. cached). If not cached, it solves it 
## and caches the result by calling another makeCacheMatrix function.

cacheSolve <- function(Imatrix, ...) {
  Mat <- Imatrix$getInverse()             # call getInverse() function to retrieve matrix
  if(!sum(Mat) == 0) {                    # if matrix is non-zero, it is the solved matrix
    return(Mat)                           # return matrix as solution.
  }
  data <- Imatrix$get()                   # *else* get unsolved matrix
  Mat <- solve(data, ...)                 # solve it 
  Imatrix$setSolve(Mat)                   # and cache it using setSolve() function
  Mat                                     # return solution
}
