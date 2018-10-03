##The first function creates a special "matrix" to set or get matrix and
##set or get the inverse of the matrix while the second function gives
##the value of the inverse of the matrix, it calculates it if it has not
##been calculated otherwise it will recover it from cache



##makeCacheMatrix creates the special "matrix" which is a list
##containing a function to set the matrix, get the matrix,
##set the inverse of the matrix and get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get,
	     setinv = setinv,
	     getinv = getinv)
}


##cacheSolve computes the inverse returned by the above function
##if it has already been calculated it will return it from cache

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setinv(inv)
	inv
}
