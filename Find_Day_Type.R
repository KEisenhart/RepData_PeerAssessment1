################################################################################
## Name: 			Find_Day_Type.R
## Description: 	To determine if a given date fell on a weekend or a weekday. 
##			     Then update the Day column accordingly. 
## Output: 		A data frame with column Day filled in correctly
## To Invoke:		Find_Day_Type(df)
##				where: df is a data frame with a column called date that 
##					  will be used to determine if a given date is a
##					  weekday or weekend.
#################################################################################

Find_Day_Type<-function(df){
	#loop over 17568 rows
	for (i in 1:nrow(df)) {
		if(weekdays(as.Date(df$date[i])) %in% c("Saturday","Sunday")) {
			df$Day[i]<-"weekend"
		} else {
			df$Day[i]<-"weekday"
		}
	}
	df
}