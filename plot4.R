plot4 <- function () {
        # The dataset was downloaded in the working directory before data processing and plotting
        
        library(data.table)
        library(dplyr)
        
        zfilename <- "exdata-data-household_power_consumption.zip"
        if (!file.exists("household_power_consumption.txt"))
        {unzip(zfilename)}
        # Unzip the dataset
        
        data <- fread("household_power_consumption.txt", na.strings = "?")
        # Read the dataset with function fread() in "data.table" package;
        # As the "NA" value is represent as "?" in the dataset,
        # Specify the NA value in the "na.strings" argument;
        
        data.subset <- filter(data, Date == "1/2/2007"|Date == "2/2/2007")
        # Select the data on "2007-02-01" and "2007-02-02"
        
        data.subset <- mutate(data.subset, DateAndTime = paste(Date, Time))
        dt <- as.data.frame(select(data.subset, DateAndTime)) 
        dt <- strptime(dt[,1], "%d/%m/%Y %H:%M:%S")
        data.subset <- mutate(data.subset, DateAndTime = as.POSIXct(dt))
        # Combine the character strings "Date" and "Time" of the selected dataset
        # Save the combined character string into a new varible "DateAndTime"
        # Convert the "DateAndTime" into PosIXct class with funciton strptime()
        
        par(mfrow = c(2, 2))
        # Set the layout of the diagrams into 2X2
        
        with(data.subset, plot(DateAndTime, Global_active_power, type = "n", ylab = "Gobal Active Power (kilowatts)", xlab = ""))
        with(data.subset, lines(DateAndTime, Global_active_power))
        # plot Global_active_power against DateAndTime
        
        with(data.subset, plot(DateAndTime, Voltage, type = "n", xlab = "datatime"))
        with(data.subset, lines(DateAndTime, Voltage))
        # plot Voltage against DateAndTime

        with(data.subset, plot(x = DateAndTime, y = Sub_metering_1, type = "n", ylab = "Energy sub metering", xlab = ""))
        with(data.subset, lines(x = DateAndTime, y = Sub_metering_1, col = "black"))
        with(data.subset, lines(x = DateAndTime, y = Sub_metering_2, col = "red"))
        with(data.subset, lines(x = DateAndTime, y = Sub_metering_3, col = "blue"))
        legend("topright", 
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
               lty = c("solid", "solid", "solid"), 
               col = c("black", "red", "blue"),
               bty = "n",
               text.font = 0.1,
               xjust = 1
        )
        # plot Sub_metering_1, 2 and 3 against DateAndTime
        
        with(data.subset, plot(DateAndTime, Global_reactive_power, type = "n", xlab = "datatime"))
        with(data.subset, lines(DateAndTime, Global_reactive_power))
        # plot Global_reactive_power against DateAndTime
        
        dev.copy(png, "plot4.png")
        dev.off()
        # Create the daigram "plot4.png"
}