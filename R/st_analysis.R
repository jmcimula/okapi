#The function to count the number of followers and comments of the article posted on the website
get_st_analysis <- function (link){
 
    ky_link     <- readLines(link,warn = F)
	
	nb_follower <- ky_link[grep("followers",ky_link)]
    
	nb_comment  <- ky_link[grep("followers",ky_link) + 1]
	
	
	j <- 10
	
	for (i in 1:j){
	    
		regX <- paste('([[:digit:]]{',i,'})',sep="")
		
		    if (stringr::str_detect (nb_follower,regX) == TRUE){
		    
		        k <- stringr::str_extract (nb_follower,regX)
				
				c_list <- as.character(unlist(k)) #Convert list to caracter	
				
		    }#End if 
	}#End for
	
	#Last value nb_follower
	nb_follower <- as.numeric(c_list)
	
	for (i in 1:j){	   
	        
		regX <- paste('([[:digit:]]{',i,'})',sep="")
		
            if (stringr::str_detect (nb_comment, regX) == TRUE){
		                  
						   k <- stringr::str_extract (nb_comment,regX)
						   
						   c_list <- as.character(unlist(k)) #Convert list to caracter		
						   
		    }#End if
			
	}#End for
	
	nb_comment <- as.numeric(c_list)#Last value nb_comment
	
	df_return <- data.frame (follower = nb_follower, comments = nb_comment)
	
	df_return <- as.data.frame(df_return)
	
	return (df_return)
	
 }#End function