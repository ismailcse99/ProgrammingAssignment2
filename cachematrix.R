
## Caching the inverse of a Matrix
## The following two functions are used to perform this task.

## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## Assume that, the given input matrix is always invertible

## The makeCacheMatrix creates a special "matrix" object that can cache its inverse 
## this function creates a list containing a function to
## a. set the value of the matrix
## b. get the value of the matrix
## c. set the inverse value of the matrix
## d. get the inverse value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y){
          x <<- y
          minv <<- NULL
  }
  get <- function() x
  m_setinverse <- function(solve) minv <<- solve
  m_getinverse <- function() minv
  list(set = set, get = get, m_setinverse = m_setinverse, m_getinverse = m_getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        minv <- x$m_getinverse()
        if(!is.null(minv)){
                message("Getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$m_setinverse(minv)
        minv
}

## Sample test and verification

## x <- rbind(c(1, 2), c(2,1))
## m <- makeCacheMatrix(x)
## m$get()
##      [,1] [,2]
##[1,]    1    2
##[2,]    2    1

## cacheSolve(m)
##         [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333

## Getting the cached data

## cacheSolve(m)

## Getting cached data
##         [,1]       [,2]
## [1,] -0.3333333  0.6666667
## [2,]  0.6666667 -0.3333333
 