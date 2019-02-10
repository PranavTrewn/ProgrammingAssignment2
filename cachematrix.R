## The two functions below together allow for caching the inverse of a matrix.

## makeCacheMatrix is a function that will create a unqiue matrix object able to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
		set <- function(y){
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(solvematrix) inv <<- solvematrix
		getinv <- function() inv
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is a function that computes the inverse of the unique matrix returned by makeCacheMatrix. cacheSolve will retrive the inverse for a matric from the cache if it's already been calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
     if(!is.null(inv)) {
     	message("getting cached data")
     	return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}