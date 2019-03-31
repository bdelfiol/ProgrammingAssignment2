## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCaheMatrix: This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        w <- NULL
        set <- function(y) {
                x <<- y
                w <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) w <<- inverse
        getinverse <- function() w
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        }

B <- matrix(c(1,2,3,4,5,6), 2, 2) #test the function
B1<-makeCacheMatrix(B) #inverse returned from cache

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

B1<-makeCacheMatrix(B)

cachesolve <- function(x, ...) {
        
        w <- x$getinverse()
        if(!is.null(w)) {
                message("getting cached data")
                return(w)
        }
        data <- x$get()
        w <- solve(data, ...)
        x$setinverse(w)
        w
}

cachesolve(B1) #inverse returned after computation
