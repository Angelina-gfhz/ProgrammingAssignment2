## Put comments here that give an overall description of what your
## functions do

## A: Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.


## Write a short comment describing this function

## A: The first function, `makeCacheMatrix` creates a special "matrix", which is
## really a list containing a function to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

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


## Write a short comment describing this function

## A: The following function calculates the inverse of the special "matrix"
## created with the above function. 
## It first checks to see if the inverse has already been calculated. 
## If so, it `get`s the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ....)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
