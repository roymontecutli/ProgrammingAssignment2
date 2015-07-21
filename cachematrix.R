## Assignment 2 - R Programming
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the  inverse of a matrix rather than computing it repeatedly.
## Assumption <- it is assumed that all the matrixes provided are invertible.

## The code and the structure is a clone from 
## https://github.com/rdpeng/ProgrammingAssignment2

## More information regarding matrix inversion 
## http://mathworld.wolfram.com/MatrixInverse.html

## Definition of the matrix object that saves the inverted matrix on a memory cache.
makeCacheMatrix <- function(x = matrix()) {

      # localInverse will hold the inverted matrix in memory
      localInverse <- NULL
      
      # Override of the set and get functions of the matrix
      set <- function(newMatrix) {
            x <<- newMatrix
            # as this is a new copy of the matrix, the previous copy of the inverted 
            # matrix is erased.
            localInverse <<- NULL
      }
      
      get <- function() {
            x
      }
      
      # Overload of the Set and get of the inverted matrix
      setInv <- function(invertedMatrix) {
            localInverse <<- invertedMatrix
      }
      
      getInv <- function() {
            localInverse
      }
      
      list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Computes the inverted cache on the first time it is executed, the following calls 
## will return the values saved on memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      cacheObj <- x$getInv()
      
      if (!is.null(cacheObj)) {
            message("cache")
            return(cacheObj)
      }
      message("non - cache")
      mat <- x$get()
      # from solve{base} Lapack
      cacheObj <- solve(mat, ...)
      x$setInv(cacheObj)
      cacheObj
}

## Unit test of the function
unitTest <- function() {
      # To verify the calculations, as explained on 
      # https://www.mathsisfun.com/algebra/matrix-inverse.html
      # the original matrix is printed
      test1 <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
      print(test1)
      
      # CacheMatrix object is created
      testCache1 <- makeCacheMatrix(test1)
      
      # the first time the function is invoked the calculcation will be executed 
      # ("non -cache" label will be displayed)
      testInverted1 <- cacheSolve(testCache1)
      print(testInverted1)
      
      # All the following times that the inverted matrix is requested, it will 
      # be from the cache ("cache" label)
      testInverted2 <- cacheSolve(testCache1)
      print(testInverted2)
      
      testInverted3 <- cacheSolve(testCache1)
      print(testInverted3)
}
