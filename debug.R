calcdamage <- function(amount=0, exponent="x")
{
        if (amount == 0) result <- 0
        else if (exponent == "K" | exponent == "k") result <- amount * 1E3
        else if (exponent == "M" | exponent == "m") result <- amount * 1E6
        else if (exponent == "B" | exponent == "b") result <- amount * 1E9
        else result <- amount
        result
}

#mydata$cropdamage <- apply(mydata,1,calcdamage(mydata$CROPDMG,mydata$CROPDMGEXP))
mydata$propdamage <- mapply(calcdamage, mydata$PROPDMG,mydata$PROPDMGEXP)
#mydata$cropdamage  <- mapply(calcdamage, mydata$CROPDMG,mydata$CROPDMGEXP)
head(mydata$cropdamage,100)