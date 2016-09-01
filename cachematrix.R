## This is the Programming Assignment 2 for R programming
## course in Coursera. It sets a practical example of lexical scoping by 
## creating values stored on the cache and retrieving them for function 
## aplication

## This function generates a list that store 4 functions to set and get a matrix
## and its inverse to (set) and from (get) the cache

makeCacheMatrix <- function(x = matrix()) { 
        inv <- NULL
        set <- function(y) { ## You can change the matrix value (x$set)
                x <<- y
                inv <<- NULL ## Resets the value of the inverse to NULL when you change the stored matrix 
        }
        get <- function() x ## Prints the matrix from the cache
        setInvM <- function(InvM) inv <<- InvM ## Sets and change the stored inverse
        getInvM <- function() inv ## prints the inverse from the cache
        list(set = set, get = get, setInvM = setInvM, getInvM = getInvM)
}


## This function takes as argument the output of makeCacheMatrix, it checks the
## value of the inverse stored, retrieves it if it exists or calculates it if 
## its value is NULL

cacheSolve <- function(x, ...) {
        inv <- x$getInvM() 
        if(!is.null(inv)){
                message("getting cached data") ## Message only appears if the 
                return(inv)                    ## value is on the cache
        }
        matr <- x$get()
        inv <- solve(matr,...) ## Calculates inverse if is not on the cache
        x$setInvM(inv) ## Stores the inverse on the cache (makeCacheMatrix)
        inv  ## Returns a matrix that is the inverse of 'x'
}
