## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## X=makeCacheMatrix() creates a reassignable matrix that is meant to hold its inverse matrix
## X$set(x) allows us to reset the stored matrix
## X$get() returns the stored matrix
## getinverse and setinverse shouldn't be used directly, use cacheSolve(X) instead

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
            x <<- y
            inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Write a short comment describing this function

## Computes and caches the inverse of a matrix created via makeCacheMatrix
## Usage: X = makeCacheMatrix()
##        cacheSolve(X)     ## Computes, caches and returns the inverse of X
##        cacheSolve(X)     ## uses the cached value
##        X.set(y)          ## Resets the matrix stored in X, and clears the cache
##        cacheSolve(X)     ## Computes, caches and returns the inverse of the new matrix in X
##        cacheSolve(X)     ## uses the new cached value

## See after the code for this function if interested in an alternative way to do the caching

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        X <- x$get()
        inv <- solve(X, ...)
        x$setinverse(inv)
        inv
}

### In my opinion, a more elegant solution exists, where cacheSolve is incorporated
### into makeCacheMatrix's getinverse. Sample code follows, commented so it does not execute.
### The idea is that getinverse simply calls an internal function called f. That function defaults to
### a function called getinverseDefault, which when executed computes the inverse of the matrix,
### and then replaces the function f with a version that just accesses this computed inverse.
### Subsequent calls to getinverse will just use this overwritten f.
### A call to X$set resets f back to this default.
###
### Only one function needed with this technique

# alternateCacheMatrix <- function(x = matrix()) {
#     f <- getinverseDefault <- function() {
#         inv <- solve(x)
#         f <<- function() { message("cached"); inv }
#         inv
#     }
#     set <- function(y) {
#             x <<- y
#             f <<- getinverseDefault
#     }
#     get <- function() x
#     getinverse <- function() { f() }
#     list(set = set, get = get,
#          getinverse = getinverse)
# }
# 
# y = alternateCacheMatrix(matrix(rnorm(16), 4))
# y$get()
# y$getinverse()
# y$getinverse()
# y$set(matrix(rnorm(16), 4))
# y$get()
# y$getinverse()
# y$getinverse()
