
################################################################################
## Name: 			Imputing.R
## Description: 	To replace all NAs in the steps column with medians computed
##			     for its associated interval. 
## Output: 		A data frame with column steps replaced with median values
## To Invoke:		Imputing_NAs(df, d, data_check)
##				where: df is a data frame aggregate of median steps taken per 
##					  interval.
##					  d is the original data frame read in from activity.csv
##					  data_check is the logical data frame made from d that 
##					  indicates whether a row has good data (no NAs is TRUE)  
##                         or bad data (NAs were found is FALSE). 
## Invokes:		Replace(it)
#################################################################################

#################################################################################
## Name: 			Replace
## Description: 	To return the median calculated for a given interval.  
## Output: 		The median calculated for the supplied interval
## To Invoke:		Replace(it)
##				where: it is a data frame containing NAs that need to be  
##					  replaced with medians stored at that NAs interval.
#################################################################################
Replace<- function (it){
		good_value<-0
		for (j in 1:nrow(df)) {
			#If the intervals match then...
			if(it$interval == df$interval[j]) {
				#send back the median calculated for this interval
				good_value<-df$median[j]
				break
			} 
		}
		good_value
	}

Imputing_NAs<-function(df,d,data_check){
	#loop over 17568 rows
	for (i in 1:length(data_check)) {
		#If this value is an NA then...
		if(data_check[i]==FALSE){
			#replace it with the median for its interval
			d$steps[i]<-Replace(d[i,])
		} 
	}
	d
}