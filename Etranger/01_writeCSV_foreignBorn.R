############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Open Sans"

############################################################################################
###		Plot world map % immigrant foreign born
############################################################################################

data.file <- 'UN_MigrantStock_country2013.csv'
data.read <- read.csv(data.file)
data.read$iso2 <- countrycode(data.read[,2], origin = "iso3n", destination = "iso2c")
data.read$continent <- countrycode(data.read[,'iso2'], origin = "iso2c", destination = "continent")
# for countries with no match, match the name
#idx <- which(is.na(data.read$iso2))
#data.read[idx,'iso2']<- countrycode(data.read[idx,1], origin = "country.name", destination = "iso2c")

# Remove Holy See
data.read <- data.read[which(!data.read$iso2 %in% c('VA', 'AD','MC', 'IM', 'GI')),]

write.table(data.read, file="UN_MigrantStock_country2013_iso2.csv", sep=",", row.names = F)
write.table(data.read[which(data.read$continent == 'Europe'),], file="UN_MigrantStock_europe2013_iso2.csv", sep=",", row.names = F)

############################################################################################
###		Plot evolution of the foreign born population (as a percentage of the total population)
############################################################################################

data.file <- 'eurostat_foreignerPopulation_merged.csv'
data.read <- read.csv(data.file)
data.read$iso2 <- countrycode(data.read[,1], origin = "country.name", destination = "iso2c")


write.table(data.read, file="eurostat_percForeigners.csv", sep=",", row.names = F)