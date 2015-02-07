plot4 <- function(filename, startDate = NA, endDate = NA) {

### filename in working directory!!!
### function-call: plot2("household_power_consumption.txt", "01/02/2007", "02/02/2007")

    #read col with dates in order to get the lines from...to
    f <- read.table(filename, sep=";", header=TRUE, na.strings="?", nrows=10)
    #then determine classes of columns
    classes <- sapply(f, class)
    #keep classses...need it further down
    classesReduced <- c(classes[1:2], rep("NULL",length(classes)-2))
    #even reading only one columns can take some time...so please be patient
    fDatesOnly <- read.table(filename, sep=";", header=TRUE, na.strings="?", colClasses=classesReduced)
    fDatesOnly <- as.Date(fDatesOnly[1,], "%d/%m/%Y")

    #determine lines of interest
    date1 <- as.Date(startDate, "%d/%m/%Y")
    date2 <- as.Date(endDate, "%d/%m/%Y")
    date2 <- date2 + 1

    startRow <- match(date1,fDatesOnly)
    endRow <- match(date2, fDatesOnly)
    endRow <- endRow - 1
    numRows <- endRow - startRow + 1

    #now the relevant cluster of rows has been identified reading should be "smooth"
    f <- read.table("household_power_consumption.txt", sep=";", header=TRUE, na.strings="?", colClasses=classes, skip = startRow, nrows=numRows)
    names(f) <- names(classes)

    #combine Date and Time column to enable time series plots
    datetime <- as.POSIXct(paste(f$Date, f$Time), format="%d/%m/%Y %H:%M:%S")
    #add column to dataframe
    f["datetime"] <- datetime
    
    #plot first on screen and then copy to png-file
     	
    #plot4: canvas with 4 plots 
    par(mfcol=c(2,2))
    par(cex.lab=.8)
    par(cex.axis=.8)
    plot(f$datetime, f$Global_active_power, type="n", main = "", xlab = "", ylab="Global active power") 
    lines(f$datetime, f$Global_active_power)
    
    plot(f$datetime, f$Sub_metering_1, type="n", main = "", xlab = "", ylab="Energy sub metering") 
    lines(f$datetime, f$Sub_metering_1, col="black")
    lines(f$datetime, f$Sub_metering_2, col="red")
    lines(f$datetime, f$Sub_metering_3, col="blue")
    legend("topright", legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=1, col=c("black", "red", "blue"), border="black", cex = .75)

    plot(f$datetime, f$Voltage, type="n", main = "", xlab = "datetime", ylab="Voltage") 
    lines(f$datetime, f$Voltage)
    
    plot(f$datetime, f$Global_reactive_power, type="n", main = "", xlab = "datetime", ylab="Global_reactive_power") 
    lines(f$datetime, f$Global_reactive_power)
    
    dev.copy(png, "plot4.png")     	#copy screen to png-file
    dev.off()				#close device!
    

}
