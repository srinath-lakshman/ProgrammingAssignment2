## makeCacheMatrix takes in a invertible matrix 'x' and returns a list object.
## The list object has the attributes set, get, set_inv, get_inv.
## The get() and get_inv() functions, returns the stored matrix 'x' and its inverse (initially NULL).
## The set(x_xet) and set_inv(inv) functions, returns the newly stored matrix 'x_set' and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mem <- NULL
        set <- function(x_set) {
		x <<- x_set
		mem <<- NULL
	}
	get <- function() x
	set_inv <- function(inv) mem <<- inv
	get_inv <- function() mem
	list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## cacheSolve returns the inverse of the matrix.
## It uses the makeCacheMatrix function to store matrix and its inverse.
## The makeCacheMatrix can also be used to change to a new matrix and to store its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mem <- x$get_inv()
        if(!is.null(mem)) {
                message("getting cached data")
                return(mem)
	}
	data <- x$get()
        mem <- solve(data, ...) 
	x$set_inv(mem)
	mem
}
