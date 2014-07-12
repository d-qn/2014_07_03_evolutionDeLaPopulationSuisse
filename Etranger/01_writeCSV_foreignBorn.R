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
data.read <- data.read[which(data.read$iso2 != 'VA'),]

write.table(data.read, file="UN_MigrantStock_country2013_iso2.csv", sep=",", row.names = F)
write.table(data.read[which(data.read$continent == 'Europe'),], file="UN_MigrantStock_europe2013_iso2.csv", sep=",", row.names = F)

############################################################################################
###		Plot evolution of the foreign born population (as a percentage of the total population)
############################################################################################

# data.file <- '302012021T006.csv'
# data.read <- read.csv(data.file)
#
# colnames(data.read) <- gsub("^X", "", colnames(data.read))
#
# country_sub <- c('France', 'Germany', 'Spain', 'Switzerland', 'United Kingdom', 'United States', 'Brazil', 'China', 'India','Russian Federation')
# data.read$iso2 <- countrycode(as.character(data.read[,1]), origin = "country.name", destination = "iso2c")
#
# d.sub <- data.read[data.read$Pays %in% country_sub,]
# # translate country names
# d.sub$Pays <- as.character(country_names[match(d.sub$iso2, country_names[,1]),'ISO.3166.1.French.short.name..proper.reading.order.'])
# d.sub <- melt(d.sub)
# colnames(d.sub)[3] <- 'annee'
#
# ggplot(data = d.sub) + geom_line(aes(annee, value, group = Pays, color = Pays))