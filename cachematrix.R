##                          R Programming Assignment 2

## Caching the inverse of a matrix
## In many cases, xatrix inversion is a costly computation. Therefore, some benefits
## would come from cashing the inverse of a matrix. Cashing the inverse of a matrix
## will reduce the time of computing the inverse repeatedly. 

## Below is a pair of functions for R Programming Assignment 2 will store a 
## matrix and cashe its inverse. 

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y 
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mymtrx <- x$get()
  m <- solve(mymtrx, ...)
  x$setinverse(m)
  m
}

## Testing the function, I have pre-selected two invertiable matrices
## one is 2 by 2 and the other one is 3 by 3

## Creating Matrix A.
Matrix_A <- matrix(c(-5,2,-3,1), nrow=2, ncol=2)

## The matrix function is created with internal cache
Mat_test_A_for_Assignment2 <- makeCacheMatrix()

## Internal value of "Mat_test_A_for_Assignment2" is set
Mat_test_A_for_Assignment2$set(Matrix_A)

## 1. This call will find the inverse 
cacheSolve(Mat_test_A_for_Assignment2)

## Below is the inverse I got for running the previous code (line 58)
##      [,1] [,2]
## [1,]    1    3
## [2,]   -2   -5


## 2. This call will use the cashed value
cacheSolve(Mat_test_A_for_Assignment2)

## Below is the cashed value I got for running the previous code (line 67),
## the message "getting cached data" defined in the cacheSolve() function is shown
## getting cached data (This line will be in red on R console)
##      [,1] [,2]
## [1,]    1    3
## [2,]   -2   -5



## Creating Matrix B.
Matrix_B <- matrix(c(1,0,5,2,1,6,3,4,0), nrow=3, ncol=3)

## The matrix function is created with internal cache
Mat_test_B_for_Assignment2 <- makeCacheMatrix()

## Internal value of "Mat_test_A_for_Assignment2" is set
Mat_test_B_for_Assignment2$set(Matrix_B)

## 1. This call will find the inverse 
cacheSolve(Mat_test_B_for_Assignment2)

## Below is the inverse I got for running the previous code (line 88)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1

## 2. This call will use the cashed value
cacheSolve(Mat_test_B_for_Assignment2)

## Below is the cashed value I got for running the previous code (line 97),
## the message "getting cached data" defined in the cacheSolve() function is shown
## getting cached data (This line will be in red on R console)
##      [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1

## Thank you for reviewing it. 


