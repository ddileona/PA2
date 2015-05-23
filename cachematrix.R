## Put comments here that give an overall description of what your
## functions do



makeCacheMatrix <- function(x = matrix()) {
    #This function creates a special "matrix" object 
    #that can cache its inverse.
    #This function does NOT invert the matrix. See cacheSolve for the
    #actual matrix inversion
    
    #Inputs:
    #x: the matrix to be inverted; If the matrix is not numeric, it is 
    #      coerced to numeric
    
    #Outputs:
    #This function outputs a list of functions:   
    # set: sets the value of the matrix
    # get: gets the value of the matrix
    # setinverse: sets the value of the matrix inverse
    # getinverse: gets the value of the mean
    # Access the functions using the [output name]$[fcn name] syntax
    
    #Example:
    # a <- makeCacheMatrix(x)
    # a is a list of the 4 functions
    # Access the functions using a$[name of fcn]
    # a is used with cacheSolve.R, which looks for the inverse 
    #    using a$getinverse() and calculates it if it is not found 
    #    (ie NULL)
    # Before the inverse has been calculated, a$getinverse() returns
    #    NULL
    #After the inverse has been calculated, a$getinverse() returns the 
    #    matrix inverse
    #
    # b <- cacheSolveMatrix(a)
    # Returns the matrix inverse
    #The matrix inverse can now be accessed in b or by using 
    #    a$getinverse()
    
    
    m <- NULL #create empty vector
    set <- function(y) {
        x <<- y  #assigns the value of y to x
        m <<- NULL #initializes the inverse, m, to NULL everywhere
    }
    
    get <- function() {   #this func takes no inputs
        x                   #return the matrix
    }
    
    setinverse <- function(inv) {  
        m <<- inv    #setinverse sets the value of m to the provided inverse
    }
    getinverse <- function() { #this function takes no inputs
        m    #getinverse returns m which is the inverse
    }
    
    #makeCacheMatrix returns a list of these functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}


###################################################################

cacheSolve <- function(x, ...) {
    #This function returns a matrix that is the inverse of 'x'
    #It computes the inverse of the special "matrix" 
    #returned by makeCacheMatrix above. If the inverse has already 
    #been calculated (and the matrix has not changed), then the 
    #cacheSolve should retrieve the inverse from the cache.
    
    #Inputs
    #x:the output of makeCacheMatrix
    
    #Outputs:
    #A matrix that is the inverse of x
    
    #Example:
    # a <- makeCacheMatrix(x)
    # b <- cacheSolveMatrix(a)
    # b is the matrix inverse of a
    #The matrix inverse can now be accessed in b or by using 
    #    a$getinverse()
    
    
    m <- x$getinverse() #set the m to be the value of getinverse in 
                        #the output of makeCacheMatrix 
    
    
    #if m is a value (not null) then the inverse has
    #already been calculated. Retrieve the inverse from the cache
    if(!is.null(m)) { 
        message("getting cached data")
        return(m)   #forces the function to return now and skip the following
    }
    
    
    #if m is null (ie if the inverse has not already been calculated)
    
    data <- x$get()   #assign the value of x (the matrix of interest) 
                      #to data
    m <- solve(data, ...) #take the inverse of the matrix of interest 
                           #and assign it to m
    x$setinverse(m)     #set the value ofthe inverse in the cache
    
    m                   #return the inverse
}
