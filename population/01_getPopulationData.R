############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

############################################################################################
### Compute the population growth index for some countries
############################################################################################

infile <- read.csv("Data-Table 1.csv", stringsAsFactors = F)
colnames(infile) <- gsub("^X", "", colnames(infile))

#swi_isocodes <- c("CHN", "BRA", "IND")
sampleCountry <- c("CHE", "ITA", "FRA", "DEU", "AUT", "GBR")


data <- melt(dplyr::filter(infile, Country.Code %in% sampleCountry)[,c(1, 5:ncol(infile))])
data$variable <- as.numeric(as.character(data$variable))

data$index <- unsplit(by(data, data$Country.Name, function(d) {(d$value / d$value[1]) * 100 }), data$Country.Name)


ggplot(data) + geom_line(aes(variable, index, group = Country.Name, color = Country.Name)) + ggtheme_ygrid

colnames(data) <- c('country', 'year', 'value', 'index')

data2 <- reshape(data[,c(1, 2,4)], idvar = "country", timevar = "year", direction = "wide")
colnames(data2) <- gsub("index\\.", "", colnames(data2))

write.csv(data2, "populationGrowth.csv", row.names = F)

############################################################################################
### Get the population of switzerland over since 1850
############################################################################################

data.file <- 'hs-f-01.01.01.03.csv'
data.read <- read.csv(data.file)

## Reformat
# get rid of the ...
for(i in 2:ncol(data.read)) {
	data.read[,i] <- as.numeric(as.character(data.read[,i]))
}

# get only the total rows only
tmp <- t(data.read[data.read$Nationalité == "Total ",-1])
pop <- data.frame(annee = as.numeric(gsub("^X", "", row.names(tmp))), total = as.vector(tmp))


# get the data from 2010
d2 <- read.csv("px-f-01-2A00.csv", sep ="\t")

write.csv(rbind(pop, d2), "populationTotal.csv", row.names = F)


