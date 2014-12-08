############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
library(pxR)

############################################################################################
### load mariage rate since 1861
############################################################################################

data.px <- read.px("data/px-f-01-2B10.px")
d <- data.px$DATA[[1]]
d$Année <- as.numeric(as.character(d$Année))
colnames(d) <- c('indicator', 'year', 'value')
# age au premier mariage
dage <- dplyr::filter(d, indicator == "Age moyen de l'homme au 1er mariage" |
	indicator == "Age moyen de la femme au 1er mariage")
dage$indicator <- gsub("^.*(homme|femme).*$", "\\1", dage$indicator)

ggplot(dage) + geom_line(aes(year,value, group = indicator, color = indicator)) + ggtheme_ygrid

# write csv for datawrapper
dage <- dage[which(!is.na(dage$value)),]
write.csv(dcast(dage, year  ~ indicator), "data/ageMoyenMariage.csv", row.names = F)

############################################################################################
### load divorce rate since 1861
############################################################################################

data.px <- read.px("data/px-f-01-2C10.px")
d <- data.px$DATA[[1]]
d$Année <- as.numeric(as.character(d$Année))
colnames(d)[1] <- 'indicator'

variables <- c('Indicateur conjoncturel de divortialité', 'Durée moyenne du mariage au moment du divorce')
sapply(variables, function(var) {
	print(ggplot(data = d[which(d[,1] == var),2:3]) + geom_line(aes(Année, value)) + ggtitle(var) +
	theme_minimal() + ylab(var))
})

# subset the data to get only year where 'Mariages pour 1000 habitants' was defined
write.csv(dplyr::filter(d, indicator == "Indicateur conjoncturel de divortialité")[,2:3], "data/indicateurDivortalité.csv", row.names = F)
