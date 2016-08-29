#' The package for Okapi News Tracker
#'
#'@param news type of of news to track for articles of Okapi
#'@param number is the number of days ago
#'@return dashboard with the title of the article and some statistics about followers and comments
#'
#'@author Jean Marie Cimula
#'@details
#'An implementation of dashboard to follow the lifecycle of articles posted on the website of Okapi Radio i.e followers and comments
#'
#'@export

okapi_news <- function (news, number){
#get_okapi_news <- function (news, from, to, format) #Just fit it with this model after

  sch <- "http://www.radiookapi.net/"

  number <- as.integer(number)

	plink <- paste(sch, news, sep="")

	text_df <- data.frame()

	for (i in 0 : number){

			#Link parameters
			plink <- paste(plink, "?page=", sep="")

			plink <- paste(plink, i ,sep="")

			dpd <- readLines(plink, warn = F)

			dpd <-  iconv(dpd,"UTF-8","latin1")

			#reference expression
			ky_word <- "views-field views-field-title"

			pdg <- grep (ky_word, dpd)

			klen <- length(pdg)

        for (i in 1: klen){

				#First line depending to the grep result
				ky_pub <- dpd[pdg[i]]

				ky_pub <- stringr::str_trim(ky_pub)#Removing left and right hidden characters

	            #Reference character
				sc_value <- unlist(gregexpr(pattern = ky_word, ky_pub)) + stringr::str_length(ky_word) + 2

				ky_pub   <- substr(ky_pub,sc_value,stringr::str_length(ky_pub))

				ky_pub   <- stringr::str_trim(ky_pub)

				#Reference character
				ky_html  <- "a href"

				sc_value <- unlist(gregexpr(pattern = ky_html, ky_pub)) + stringr::str_length(ky_html) + 2

				ky_pub   <- substr(ky_pub, sc_value, stringr::str_length(ky_pub))

				#Reference character
				sc_value <- unlist(gregexpr(pattern = '>', ky_pub))

				ky_link <- paste(sch, stringr::str_trim( substr ( ky_pub, 1, sc_value[1] - 2) ), sep="")#Getting the Link

				kpc <- stringr::str_trim(substr(ky_pub, sc_value[1] + 1, sc_value[2] - 4))#Substring to get the kpc

				#Following line in the data
				ky_pub <- dpd[pdg[i]+1]

				#Reference character

				sc_value <- unlist(gregexpr(pattern = '>', ky_pub))

				ky_datepub <- stringr::str_trim(substr(ky_pub,sc_value[2] + 1,sc_value[2] + 18))#Substring to get the date of Pub.

				nb_words <- sum(stringi::stri_count_words(readLines(ky_link)))

				const_df <- data.frame (news = kpc, datepub = ky_datepub, link = ky_link, nb_words = nb_words)

				text_df <- rbind(text_df, const_df)#, get_st_analysis (ky_link) still pending

		}#Loop to extract data
	}
    #return the data frame

    return (as.data.frame(text_df))

}#End function
