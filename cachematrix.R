## This function stores the original square matrix in "x"
## and its inverse form in "inv". The inverse form is obtained
## from cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #Clear "inv" of previous data to initialize
        set <- function(y) {
                x <<- y
                inv <<- NULL #For initialization
        }
        get <- function() x #Store the original matrix
        setinv <- function(solve) inv<<-solve #Store output from cacheSolve()
        getinv <- function() inv #NULL by default, retrieve data in "inv"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function checks if the inverse matrix is present.
## If not present, it will proceed to calculate the inverse
## and print out to screen the result i.e. inverse matrix

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #Retrieve data from makeCacheMatrix()
        if(!is.null(inv)) { #If NULL, calculate using Solve else print out "inv"
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #Retrieve original matrix
        inv <- solve(data, ...) #Inverse matrix
        x$setinv(inv) #Initialize list $setinv with data i.e. inverse matrix
        inv #Print to screen the inverse matrix
}
