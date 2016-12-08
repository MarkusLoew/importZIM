#' Import function for YARA ZIM Plant technology ZIM sensor files that extracts sensor ID information, and re-organises the temperature and humidity data
#'
#' @description Imports ZIM water sensor files that extracts sensor ID information from sensor names. Merges Pp data with humidity and temperature. See http://yara.zim-plant-technology.com/ for the actual sensors.
#' @param file The name of the file to be imported. Character string.
#' @return Returns a data frame.
#' @examples
#' \dontrun{
#' df <- importZIM(file)
#' }
#' @export

importZIM <- function(file) {

df <- utils::read.csv(file, sep = "\t")

# names
# "Timestamp..in.Local.time"
# "Yara.Water.Sensor.ID..4755.kPa...1411.1.MS.Agface"
# "Humidity.ID..1808.....1423.3.MS.Agface" 
# "Temperature.ID..1417..C...1411.3.MS.Agface"

# identify sensors
# sensors have the "ID" in their name
# that only leaves the timestamp
name.sensor <- grep("ID", names(df))

# identify Timestamp
name.time <- grep("Timestamp", names(df))

# identify Water sensors
name.water <- grep("Yara\\.Water\\.Sensor\\.ID\\.\\.", names(df))

# identify humidity sensor
name.hum <- grep("Humidity", names(df))

# identify temperature sensor
name.temp <- grep("Temperature", names(df))

# list of all sensor names
sensors.list <- names(df)[name.sensor]

# for temperature, not all characters are imported, have to adjust the name
# adjustment to match the general SensorID pattern. i.e. three ... after the unit
sensors.list <- gsub("\\.\\.C\\.\\.", "\\.degC\\.\\.", sensors.list)

# similar for humidity sensor
# humidity has five "."
sensors.list <- gsub("\\.\\.\\.\\.\\.", "\\.Percent\\.\\.\\.", sensors.list)

# Sensor name ends at the first ".."
sensor.name <- sapply(strsplit(sensors.list, split = "\\.\\."), "[", 1)

# get rid of ".ID" in the name
sensor.name <- gsub("\\.ID", "", sensor.name)

# get sensor number with unit
# temp and humidity sensor don't have their units here!
sensor.num <- sapply(strsplit(sensors.list, split = "\\.\\."), "[", 2)

# only keep the sensor number
sensor.ID   <- sapply(strsplit(sensor.num, split = "\\."), "[", 1)
sensor.unit <- sapply(strsplit(sensor.num, split = "\\."), "[", 2)

# split at the "..." to get to transmitter #, port #, and site
sensor.connect <- sapply(strsplit(sensors.list, split = "\\.\\.\\."), "[", 2)

# first element of sensor.connect is transmitter
# second element is port #
# third element is "MS" for measurement site, discarded
# fourth element is the measurement site
sensor.transmitter     <- sapply(strsplit(sensor.connect, split = "\\."), "[", 1)
sensor.transmitterport <- sapply(strsplit(sensor.connect, split = "\\."), "[", 2)
sensor.site            <- sapply(strsplit(sensor.connect, split = "\\."), "[", 4)

# put it all together
the.names <- as.data.frame(names(df)[-name.time], header = FALSE)
names(the.names)      <- "Name"
the.names$Sensortype  <- sensor.name
the.names$SensorID    <- sensor.ID
the.names$Unit        <- sensor.unit
the.names$Transmitter <- sensor.transmitter
the.names$Port        <- sensor.transmitterport
the.names$Site        <- sensor.site


# reshape the data
df.melt <- reshape2::melt(df, id.vars = "Timestamp..in.Local.time")
names(df.melt) <- gsub("Timestamp\\.\\.in\\.Local\\.time", "Timestamp", names(df.melt))

df.melt$Timestamp <- as.POSIXct(df.melt$Timestamp)

# merge with extracted sensor information
zim <- merge(the.names, df.melt,
	     by.x = "Name",
	     by.y = "variable")

# convert some elements to factors
to.factor <- c("Name", "Sensortype", "SensorID", "Unit", "Transmitter", "Port", "Site")
zim[, to.factor] <- lapply(zim[, to.factor], as.factor)

# move humidity and temperature to separate vectors, to allow merge per timestamp
met <- zim[zim$Sensortype == "Humidity" |
           zim$Sensortype == "Temperature", ]

to.keep <- c("Timestamp", "Sensortype", "Unit", "value")
met <- met[, to.keep]

# reshape the data frame to be able to merge with zim
met.cast <- reshape2::dcast(met, Timestamp ~ Sensortype + Unit)

# combine zim with met.cast
# first, remove Humidity and Temperature from zim
zim.nomet <- zim[zim$Sensortype == "Yara.Water.Sensor",]

# merge
zim.met <- merge(zim.nomet, met.cast)

# rename the sensor, get rid of Unit, as it's now redundant
names(zim.met) <- gsub("value", "Pp_kPa", names(zim.met))
zim.met$Unit <- NULL

return(zim.met)
}
