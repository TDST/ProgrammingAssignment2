#####
##
## Function makeCacheMatrix: this function creates a special "matrix" object that can cache its inverse.
## The special matrix is a list containing the methods to set the matrix, retrieve the matrix data, store the inverse, and retrieve the inverse.
##
makeCacheMatrix <- function( x = matrix( ) ){

        m <- NULL
        set <- function( y ){		# function to set the value of the matrix
                x <<- y
                m <<- NULL
        }

        get <- function( ) x		# function to retrieve the value of the matrix
        setinverse <- function( inverse ) m <<- inverse	# function to cache the matrix inverse
        getinverse <- function( ) m	# function to retrieve the pre-computed inverse
					#    if it exists, otherwise NULL

        list( set=set, get=get, setinverse=setinverse, getinverse=getinverse ) # return the corresponding special "matrix" object
}


#####
##
## Function cacheSolve: this function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
##
cacheSolve <- function( x, ... ){

        m <- x$getinverse( )		# check if the special matrix object already contains a pre-computed inverse

        if( !is.null( m ) ){		# inverse matrix was already computed and stored
                message( "getting cached inverse" )
                return( m )		# simply return the pre-computed inverse
        }
 
        data <- x$get( )		# otherwise extract the original matrix
        m <- solve( data, ... )		#    compute the inverse of matrix and
        x$setinverse( m )		#    store this for next call

        m				#    return the inverse of the matrix
}
