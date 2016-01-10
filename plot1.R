plot1 <- function () {
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
        
        with(data.subset, hist(Global_active_power, 
                             col = "red", 
                             main = "Gobal Active Power",
                             xlab = "Gobal Active Power (kilowatts)")
        )
        # plot the histgram;
        
        dev.copy(png, "plot1.png")
        dev.off()
        # Create the daigram "plot1.png"
}
