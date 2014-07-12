############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Open Sans"


data.file <- 'wbdata.Rdata'
############################################################################################
###  Get WB data
############################################################################################

#### A) Using WB

if(file.exists(data.file)) {
	load(data.file)
} else {
	library(WDI)

	indicators <- structure(c('SP.DYN.LE00.IN', 'SP.DYN.TFRT.IN'), names = c('lifeExpectancy', 'fertilityRate'))

	data.dl <- sapply(1:length(indicators), function(i) {
		wbdat <- WDI(indicator = indicators[i],
		  start = 1950,  end = 2014, extra = TRUE, cache = NULL)
		colnames(wbdat)[3] <- 'value'
		wbdat$indicator <- names(indicators)[i]
		wbdat
	}, simplify = FALSE)
	data.dl <- do.call(rbind, data.dl)
	data.dl$indicator <- factor(data.dl$indicator)

	save(data.dl, file = data.file)
}
############################################################################################
### Plot life expectancy
############################################################################################

le <- data.dl[data.dl$indicator == names(indicators)[1],]
le <- le[order(le$year),]
le$color <- 1

ggplot(data = le) + geom_line(aes(year, value, group = country, color = "grey")) + ggtheme

# library(rCharts)
# r7 <- Rickshaw$new()
# r7$layer (
#   value ~ year,
#   data = le,
#   groups = "country",
#   type = "line",
#   height = 840,
#   width = 840
# )


