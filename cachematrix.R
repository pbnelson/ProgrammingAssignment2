## makeCacheMatrix(), cacheSolve(), makeCacheMatrix_test()
##
## two helper methods for matrix inversion computation caching. Usage:
##    cM <- makeCacheMatrix(aMatrix)    # Create a cacheMatrix object,
##                                      # having value aMatrix (optional).
##    cM$get()                          # Returns the underlying matrix.
##    cM$set(aNewMatrix)                # Resets value to some new matrix.
##    cacheSolve(cM)                    # Returns value of matrix inverse,
##                                      # and caches for future requests.
##
##    makeCacheMatrix_test()            # Unit tests to validate all code.
##




## makeCacheMatrix(x)
##
## Creates a CacheMatrix "object" having value x, if provided. Otherwise
## requires using of aCacheMatrix$set(x) to set the value. Methods available
## are: set(x), get(), setSolve(solution), getSolve().
##

makeCacheMatrix <- function(x = matrix()) {

    xInverted <- NULL

    set <- function(y) {
        x <<- y
        xInverted <<- NULL
    }

    get <- function() {
        x
    }

    setSolve <- function(solution) {
        xInverted <<- solution
    }

    getSolve <- function() {
        xInverted
    }

  list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
}



## cacheSolve(x)
##
## Returns the inverse of a cached matrix (see above), specified by x.
## The first time it is invoked the result is cached, and subsequent calls
## return the cached value.
##

cacheSolve <- function(x, ...) {

    # read value from CacheMatrix object
    xInverted <- x$getSolve()

    # If value is non-null, it has been previously saved in the CacheMatrix
    # object, so print a little message to that affect.
    if(!is.null(xInverted)) {
        message("getting cached data")
    }

    # Otherwise, the value is null, which means it was never before computed
    # or saved. so, naturally, compute it and save it.
    else {
        aMatrix <- x$get()  # get the matrix
        xInverted <- solve(aMatrix, ...)  # compute its inverse
        x$setSolve(invertedMatrix) # save the solution
    }

    # Finally, return the value retrieved/computed
    return(xInverted)
}




# for the sake of creativity, the following performs the same function,
# but it as compact a form as I can conceive.
cacheSolveTerse <- function(x, ...) {
    xInverted <- x$getSolve()
    if(!is.null(xInverted))     message("getting cached data")
    else                        xInverted <- x$setSolve(solve(x$get()))
    return(xInverted)
}




###########################################
# UNIT TEST
#
# Validate the above code.
#
# usage: makeCacheMatrix_test()
#
# Look for all messages having form "passed unit test ##"
# and none of form "failed unit test ##".
#

makeCacheMatrix_test <- function() {

    # make two matrices for testing
    library(datasets)  # get iris dataset...
    # m1,m2 = sample matrices of virginica and setosa, respectively
    m1 <- as.matrix(split(iris,iris$Species)$virginica[1:4 , 1:4])
    m2 <- as.matrix(split(iris,iris$Species)$setosa   [1:3 , 1:3])

    # cM1 is the 'cached matrix' object. leave it unset with the constructor,
    # then use the set() method to set it.
    cM1 <- makeCacheMatrix()
    cM1$set(m1)
    cM1$get()

    # the following three statements should all return the same value
    cM1_s1 = solve(m1)
    cM1_s2 = cacheSolve(cM1)
    cM1_s3 = cacheSolve(cM1)

    # and check that all three are equal
    if (identical(cM1_s1, cM1_s2) && identical(cM1_s2, cM1_s3))
        print("passed unit test #1")
    else
        message("failed unit test #1")

    # This section demonstrates that whatever we do to the
    # orignal matrix variable, the cachedMatrix is not affected
    # (recall) that cM1 was built from m1.
    cM1_orig <- cM1$get()
    m1 <- m2
    cM1_unadulterated <- cM1$get()
    if (identical(cM1_orig, cM1_unadulterated))
        print("passed unit test #2")
    else
        message("failed unit test #2")

    # for cM2, set the value within the constructor
    cM2 <- makeCacheMatrix()
    cM2$set(m2)
    cM2$get()
    # the following three statements should all return the same value
    cM2_s1 = solve(m2)
    cM2_s2 = cacheSolve(cM2)
    cM2_s3 = cacheSolve(cM2)
    if (identical(cM2_s1, cM2_s2) && identical(cM2_s2, cM2_s3))
        print("passed unit test #3")
    else
        message("failed unit test #3")
}