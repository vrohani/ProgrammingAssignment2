# ------  Instructin of Usage: ------------------------------------------
#
# > source("cachematrix.R")                   // Importing cachematrix.R
# > m <- matrix(rnorm(25), nrow = 5)          // Create a matrix m
# > special_m <- makeCacheMatrix(m)           // Create our special matrix supporting the Caching feature
# > special_m$get()                           // Return the matrix
# > cacheSolve(special_m)                     // Return the inverse
# > cacheSolve(special_m)                     // Call the 2nd time, so return
#                                             // the cached inverse


# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL

    # Setter for the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Getter for the matrix
    get <- function() x

    # Setter for the inverse
    setinv <- function(inverse) inv <<- inverse
    # Getter for the inverse
    getinv <- function() inv

    # Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. In case that inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, return it
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }

    # The inverse is not yet calculated, so we calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return it
    inv
}
