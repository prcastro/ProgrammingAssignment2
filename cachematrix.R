## This functions are solutions for Programming Assignment 2, of Coursera's
## R Programming course. They cache the inverse of a matrix

## This function returns a matrix capable of caching it's inverse
## using the <<- operator 
makeCacheMatrix <- function(x = matrix()) {
	## Initializing inverse
	i      <- NULL

	## Declaring the functions
	set    <- function(y) {
		x <<- y
		i <<- NULL
	}
	get    <- function() x
	setinv <- function(inv) i <<- inv
	getinv <- function() i

	## Returning the function
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Check if the inverse of cacheMatrix is in it's cache. If yes, return the inverse,
## otherwise, caclculates the inverse, cache it, and return the inverse

cacheSolve <- function(x, ...) {
		i <- x$getinv()
		
		if (!is.null(i)) {
			message("Getting the cache data")
			## Return the cached inverse
			return(i)
		}

		data <- x$get()
		i <- solve(data, ...)
		x$setinv(i)

        ## Return a matrix that is the inverse of 'x'
        i
}
