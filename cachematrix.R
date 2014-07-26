## Put comments here that give an overall description of what your
## functions do
## The first function, makeCacheMatrix creates a special matrix object that can cache its own inverse using methods - 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of matrix
## 4. get the value of inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
	## Inititalize the inverse variable
	m <- NULL
	##Method to se the value of the matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
	## Method to get the value of the matrix
    get <- function() x

	## Method to set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse

	## Method to return cached inverse of the matrix
    getinverse <- function() m

	## Returns list of supported methods
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Calculates the inverse of the matrix returned by "makeCacheMatrix" 
## If the inverse has already been computed, function will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
            m <- x$getinverse()
        
	## Check if the inverse is already cached
   if(!is.null(m)) {
        message("getting cached data.")
	## Return the inverse from cache
        return(m)
    }
	## Get the matrix data
    data <- x$get()

	## Solve for matrix inverse
    m <- solve(data)

	##Cache the inverse for future use
    x$setinverse(m)

	## Return the matrix inverse
    m
}
