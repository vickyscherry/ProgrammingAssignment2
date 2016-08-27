## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse. It's a list that:

## 1.Sets the value of the matrix
## 2.Gets the value of the matrix
## 3.Sets the value of the inverse of the matrix
## 4.Gets the value of the inverse of the matrix

## (The <<- operator which can be used to assign a value to an object in an environment that is different from the current environment)

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
              x <<- y
              m <<- NULL
              }
              get <- function() x
              setinverse <- function(solve) m <<- solve
              getinverse <- function() m
              list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
  
}

## The function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
## Computing the inverse of a square matrix can be done with the solve function in R, solve(X) returns its inverse.
## This function assumes that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
            if(!is.null(m)) {
            message("getting cached data")
            return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setinverse(m)
  m
}

## Sample Run
##> x<- matrix(1:4,2,2)
##> m = makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4

##First Run (no cache)
##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##Second Run 
##> cacheSolve(m)
##getting cached data
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
