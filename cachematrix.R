## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#converts an atomic matrix to a cacheable matrix whose inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	input <- x
	set <- function(m){
		input <<- m
		i <<- NULL
	}
	get <- function() input
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#returns the inverse of a cacheable matrix; fetches the value from cache if present, else is freshly computed
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if(!is.null(i)){
		message('getting cached inverse')
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
