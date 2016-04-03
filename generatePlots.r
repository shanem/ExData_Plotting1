loadHouseholdPowerData <- function() {
    read.csv2("household_power_consumption.txt", sep=";")
}

generatePlot1 <- function(filteredHouseholdPower) {
    par(mfrow=c(1,1))
    hist(as.double(as.character(filteredHouseholdPower$Global_active_power)),
         col="red",
         xlab="Global Active Power (kilowatts)",
         main="Global Active Power")
    dev.copy(png,'plot1.png')
    dev.off()
}

generatePlot2 <- function(filteredHouseholdPower) {
    dateTime <- with(filteredHouseholdPower, as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    par(mfrow=c(1,1))
    plot(dateTime, filteredHouseholdPower$Global_active_power,
         ylab="Global Active Power (kilowatts)",
         xlab="",
         type="n")
    lines(dateTime, filteredHouseholdPower$Global_active_power)
    dev.copy(png,'plot2.png')
    dev.off()
}

generatePlot3 <- function(filteredHouseholdPower) {
    dateTime <- with(filteredHouseholdPower, as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    par(mfrow=c(1,1))
    plot(dateTime, filteredHouseholdPower$Sub_metering_1,
         ylab="Energy sub metering",
         xlab="",
         ylim=c(0, 40),
         type="n")
    par(col="black")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_1)))
    par(col="red")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_2)))
    par(col="blue")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_3)))
    legend("topright",
           lty=1,
           legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
           col=c("black", "red", "blue"),
           text.col="black",
           bty="n",
           inset=c(0.07, 0),
           xpd=1)
    dev.copy(png,'plot3.png')
    dev.off()
}

generatePlot4 <- function(filteredHouseholdPower) {
    dateTime <- with(filteredHouseholdPower, as.POSIXct(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    par(mfrow=c(2,2))
    
    par(col="black")
    plot(dateTime, filteredHouseholdPower$Global_active_power,
         ylab="Global Active Power",
         xlab="",
         ylim=c(0, 8),
         type="n")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Global_active_power)))
    
    par(col="black")
    plot(dateTime, filteredHouseholdPower$Voltage,
         ylab="Voltage",
         xlab="datetime",
         ylim=c(234, 246),
         type="n")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Voltage)))
    
    plot(dateTime, filteredHouseholdPower$Sub_metering_1,
         ylab="Energy sub metering",
         xlab="",
         ylim=c(0, 40),
         type="n")
    par(col="black")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_1)))
    par(col="red")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_2)))
    par(col="blue")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Sub_metering_3)))
    legend("topright",
           lty=1,
           legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
           col=c("black", "red", "blue"),
           text.col="black",
           bty="n",
           inset=c(0.17, 0.02),
           cex=0.8,
           xpd=1)
    
    par(col="black")
    plot(dateTime, filteredHouseholdPower$Global_reactive_power,
         ylab="Global Reactive Power",
         xlab="datetime",
         ylim=c(0, 0.5),
         type="n")
    lines(dateTime, as.double(as.character(filteredHouseholdPower$Global_reactive_power)))
    
    dev.copy(png,'plot4.png')
    dev.off()
}

generateAllPlots <- function() {
    householdPower <- loadHouseholdPowerData();
    filteredHouseholdPower <- householdPower[householdPower$Date %in% c('1/2/2007', '2/2/2007'), ]
    generatePlot1(filteredHouseholdPower);
    generatePlot2(filteredHouseholdPower);
    generatePlot3(filteredHouseholdPower);
    generatePlot4(filteredHouseholdPower);
}