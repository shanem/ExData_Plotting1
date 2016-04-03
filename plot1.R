loadHouseholdPowerData <- function() {
    read.csv2("household_power_consumption.txt", sep=";")
}

drawPlot1 <- function(filteredHouseholdPower) {
    par(mfrow=c(1,1))
    hist(as.double(as.character(filteredHouseholdPower$Global_active_power)),
         col="red",
         xlab="Global Active Power (kilowatts)",
         main="Global Active Power")
    dev.copy(png,'plot1.png')
    dev.off()
}

generatePlot1 <- function() {
    householdPower <- loadHouseholdPowerData();
    filteredHouseholdPower <- householdPower[householdPower$Date %in% c('1/2/2007', '2/2/2007'), ];
    drawPlot1(filteredHouseholdPower);
}