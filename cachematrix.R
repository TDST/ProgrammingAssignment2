#####
##
## Function makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse. This is only called once for
## a given matrix. The special matrix is a list containing the methods to set the matrix, retrieve the matrix data, store the inverse,
## and retrieve the inverse.
##
##      Argument(s):    matrix
##      Return:         list containing matrix object access methods, instantiated with a copies of the matrix argument
##
##	Function is based on the code makeVector( ), provided in the assignment description
##
makeCacheMatrix <- function( x = matrix( ) ){

        m <- NULL			# initialize member variable, that will store the cached inverse 

        set <- function( y ){		# function to initialize a matrix object
                x <<- y			#    assign new matrix value
                m <<- NULL		#    reset inverse variable (note: automatically triggers a new inverse computation)
        }

        get <- function( ){		# function to retrieve the value of the matrix
        	x
	}

        setinverse <- function( inverse ){ # function to cache the matrix inverse
        	m <<- inverse
	}

        getinverse <- function( ){	# function to retrieve the pre-computed inverse
		m			#    if it exists, otherwise NULL
	}

        list( set=set, get=get, setinverse=setinverse, getinverse=getinverse ) # return the corresponding matrix object
}


#####
##
## Function cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##
##      Argument(s):    matrix object
##      Return:         matrix inverse
##
##	Function is based on the code cachemean( ), provided in the assignment description
##
cacheSolve <- function( x, ... ){

        m <- x$getinverse( )		# check if the special matrix object already contains a pre-computed inverse

        if( !is.null( m ) ){		# check whether the inverse matrix was already computed and stored
                message( "getting cached inverse" ) 
                return( m )		#    and if it was, then simply return the pre-computed inverse
        }
 
        data <- x$get( )		# otherwise extract the original matrix
        m <- solve( data, ... )		#    compute its inverse and
        x$setinverse( m )		#       store this for next call

        m				# return the inverse of the matrix
}
