loadHouseholdPowerData <- function() {
    read.csv2("household_power_consumption.txt", sep=";")
}

drawPlot2 <- function(filteredHouseholdPower) {
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

generatePlot2 <- function() {
    householdPower <- loadHouseholdPowerData();
    filteredHouseholdPower <- householdPower[householdPower$Date %in% c('1/2/2007', '2/2/2007'), ];
    drawPlot2(filteredHouseholdPower);
}