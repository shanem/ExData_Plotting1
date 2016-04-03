loadHouseholdPowerData <- function() {
    read.csv2("household_power_consumption.txt", sep=";")
}

drawPlot3 <- function(filteredHouseholdPower) {
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
           inset=c(0.1, 0),
           xpd=1)
    dev.copy(png,'plot3.png')
    dev.off()
}

generatePlot3 <- function() {
    householdPower <- loadHouseholdPowerData();
    filteredHouseholdPower <- householdPower[householdPower$Date %in% c('1/2/2007', '2/2/2007'), ];
    drawPlot3(filteredHouseholdPower);
}