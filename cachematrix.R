## Caching the Inverse of a matrix
## This is accoplished by using two functions defined below.
## If the inverse of matrix already exists in the cache, it is returned
## without recalculating the inverse, this, saves computing time.
## Note: makeCacheMatrix(x) should be called prior to cacheSolve(x)

## makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
## First, it initializes a variable "inv" which will be used to save
## inverse matrix later on, i.e., a cached data.
## It provides a function get() to obtain raw matrix for which inverse 
## has to be found.
## It provides a function setinv() to assign computed inverse matrix (of x) to inv.
## It also provides a function getinv() to obtain the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
## It first checks if the inverse matrix can be found in the cache; if yes, then it returns the result and quits. 
## If not, then inverse of x is calculated, saved to cached, and returned.
## NOTE: argument x for this function must be cached, i.e. a list returned by calling makeCacheMatrix(x).


cacheSolve <- function(x, ...) { 
        inv<- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        else {
                message("Calculating Inverse Matrix.")
                data <- x$get()         # obtains matrix from object x
                inv <- solve(data, ...) # computes inverse of matrix
                x$setinv(inv)           # assigns resulting inverse matrix to object x
                return(inv)
        }
        
}

# Sample Running
# source("cachematrix.R")
# x<-matrix(1:4,2,2)
# inv=makeCacheMatrix(x)
# inv$get()
#[,1] [,2]
#[1,]    1    3
#[2,]    2    4

# no cache in the first run
# cacheSolve(inv)
#Calculating Inverse Matrix.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# Retrieving from the cache in the second run
# cacheSolve(inv)
#getting cached data.
#[,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
