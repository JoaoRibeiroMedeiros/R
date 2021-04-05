Hist3D<- function(x,y, breaks){ #breaks is 2^N
    
    len<- length(x) 
    
    rangeX <- max(x) - min(x)
    
    rangeY <- max(y) - min(y)
    
    N<-log(breaks)/log(2)
    
    intervalox <- rangeX/breaks
    
    intervaloy <- rangeY/breaks
    
    midsxx<- matrix(ncol = breaks, nrow = breaks)
    
    midsyy<- matrix(ncol = breaks, nrow = breaks)
        
    breaksx<- seq( min(x) ,  max(x) , intervalox ) # length BREAKS +1
    
    breaksy<- seq( min(y) ,  max(y) , intervaloy ) # length BREAKS +1
    
    pesos<- matrix(ncol = breaks, nrow = breaks)
    pesos<- is.na(pesos) - 0L
    pesos<- pesos - 1

    
    for( i in 1: len)
    {
        indicex<- 0
        indicey<- 0
        
        b<- breaks
        
        for( j in 1:N )
        {
        	b<- b/2
            if(breaksx[indicex+b+1] <= x[i] )
            {
                binario <-1
            }
            if( x[i] < breaksx[indicex+b+1 ] )
            {
                binario <- 0 
            }
          
          indicex<- indicex + b*binario
        }
        
         b<- breaks
         
         for( j in 1:N )
        {
        	b<- b/2
            if(breaksy[indicey+b+1] <= y[i] )
            {
                binario <-1
            }
            if( y[i] < breaksy[indicey+b+1 ] )
            {
                binario <- 0 
            }
          
          indicey<- indicey + b *binario
            
        }
        
        pesos[indicex,indicey] <- pesos[indicex,indicey] + 1
        
    }
    
    pesos <- pesos /len
    
    pesos <- pesos / (( breaksx[2] - breaksx[1] ) * ( breaksy[2] - breaksy[1] ))
    
    #return(pesos)
    
    final<-list("density"=pesos,"breaksx"=breaksx,"breaksy"=breaksy )
    
    return(final)
    
}

