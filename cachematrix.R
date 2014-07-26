## Put comments here that give an overall description of what your
## functions do
## The first function, makeCacheMatrix creates a special matrix object with a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}
