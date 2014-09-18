# makeCacheMatrix and cacheSolve are two functions used to compute the inverse
# of a matrix. These functions uses the advantage of lexical scoping of R. 
# makeCacheMatrix passes a list of 4 functions to cacheSolve fuction.
# The 4 functions passes the matrix, real data, get the inverse and stores.
# cacheSolve function computes the inverse and passes the inverse to 
# makeCacheMatrix function. The cacheSolve function also retrieves the cached
# inverse matrix.

# makeCacheMatrix function creates the special matrix to cache its inverse. 
# The function returns a list of 4 functions. These 4 functions are used in 
# cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # m is the placeholder to store the inverse of the matrix.
  
# set matrix function is used to receive the new matrix, i.e., when the values of
# the matrix changes. When the values are changed, the function also resets the 
# placeholder m.
  setmatrix <- function(y) {
    x <<- y
    m <<- NULL
  }

# getmatrix function is used to pass the matrix to cacheSolve function to compute
# the inverse for the first time and also when the values changes.
  getmatrix <- function() x

# setinverse function stores the inverse of the matrix in the placeholder m for 
# the first time and also when the values changes.
  setinverse <- function(inv) m <<- inv

# getinverse function passes the value of the placeholder to cacheSolve matrix.
  getinverse <- function() m

# The 4 functions described above are returned as list of functions.
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function computes the inverse of the matrix and returns/retrieves
# the inverse based on the changes in the values in the matrix.
cacheSolve <- function(x, ...) {
  
# getinverse function is called to get the values of the placeholder. In the 
# first execution it is always NULL and the inverse is computed after the 
# if loop. When the cacheSolve function is called second time without changes in
# value of the matrix, the function retreives the cached value.  
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Retreiving already computed (cached) inverse matrix")
    return(m)
  }

# getmatrix function is used to get value of the matrix for the first time and
# also when the matrix values are changed.
  matrixdata <- x$getmatrix()

# Solve function computes the inverse of the matrix.      
  inv <- solve(matrixdata)    
    
# setinverse function passes the inverse of the matrix to the placeholder m.
  x$setinverse(inv)
    
  inv
}