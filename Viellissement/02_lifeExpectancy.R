############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")

data.file <- 'wbdata.Rdata'
############################################################################################
###  Get WB data
############################################################################################

#### A) Using WB
indicators <- structure(c('SP.DYN.LE00.IN', 'SP.DYN.TFRT.IN'), names = c('lifeExpectancy', 'fertilityRate'))

if(file.exists(data.file)) {
	load(data.file)
} else {
	library(WDI)

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

swi_iso2 <- c("CH", "CN", "RU", "BR", "IN", "JP", "EU", "ZQ", "ZG", "XU", "Z4")
countries.iso2 <- c(swi_iso2, c('RW', 'SL', 'KH'))

le <- data.dl[data.dl$indicator == names(indicators)[1],]
le <- le[order(le$year),]

# filter out Israel
le <- le[le$iso2c != "IL",]

# get the world average
le3 <- le[le$iso2c == "1W",]
# get only the countries, i.e. region is not "Aggregates" or NA
le <- le[!is.na(le$region) & (le$region != "Aggregates" | le$iso2c %in% countries.iso2) ,]

# colors
le1 <- le[!le$iso2c %in% countries.iso2,]
le2 <- le[le$iso2c %in% countries.iso2,]

# translate country names to french
tCountry <- le2$country
tCountry<- as.character(country_names[match(le2$iso2c, as.character(country_names[,1])), 9])
idx <- which(is.na(tCountry))
tCountry[idx ]<- le2$country[idx]
le2$country <- tCountry

# relevel factors
ff <- dplyr::filter(le2, year == max(le2$year)-1)
le2$country <- factor(le2$country, levels = ff$country[order(ff$value)])

#write.csv(rbind(le1, le2, le3), "lifeExpectancy.csv")

plotMultiLineChart <- function(df1, df2, df3, yscale = "", title = "", xaxisPadding = 3) {
	gp <- ggplot() + geom_line(data = df1, aes(year, value, group = country), color = "darkgrey", alpha = 0.5, size = 0.2) + ggtheme +
	 	guides(colour = FALSE) + #selected countries
	 	geom_line(data = df2, aes(year, value, group = country, color = country), size = 0.5) +
	 	scale_colour_manual(values = swi_22rpalette) + geom_line(data = df3, aes(year, value, group = country),
	 	color = "black", size = 1.6, alpha = 0.6)

	gp + scale_y_continuous(name = yscale, expand = c(0.01,0.01)) + ggtitle(title)+
	  scale_x_continuous(name = "", limits = c(min(le$year), max(le$year) + xaxisPadding), expand = c(0.0,0.0)) +
	  geom_dl(data = df2, aes(x = year + 0.1, y = value, label = country, color = country), list("last.points", cex = 0.75)) +
	  geom_dl(data = df3, aes(x = year + 0.1, y = value, label = country), list("last.points", cex = 0.75))
 }

lep <- plotMultiLineChart(le1, le2, le3, title = "Espérance de vie")
lep
pdf("lifeExpectancy.pdf", width = 12, height = 12 * 1.3 )
lep
dev.off()

### get the variation over time
wle <- le3[(!is.na(le3$value)),]
diff(wle[c(1, nrow(wle)), 'value']) / wle[1, 'value']
diff(wle[c(1, nrow(wle)), 'value'])
wle[c(1, nrow(wle)), 'year']


############################################################################################
### Plot fertility rate
############################################################################################
countries.iso2 <- c(swi_iso2, c('RW', 'YE', 'NE'))

fr <- data.dl[data.dl$indicator == names(indicators)[2],]
fr <- fr[order(fr$year),]

# filter out Hong-Kong and Macau
fr <- fr[!fr$iso2c %in% c('HK', 'MO'),]

# get the world average
fr3 <- fr[fr$iso2c == "1W",]
# fr3 <- fr[fr$iso2c %in% c("1W", 'Z4', 'Z7', 'ZG', 'ZJ', 'ZQ'),]
# get only the countries, i.e. region is not "Aggregates" or NA
fr <- fr[!is.na(fr$region) & (fr$region != "Aggregates"  | fr$iso2c %in% countries.iso2),]

# colors
fr1 <- fr[!fr$iso2c %in% countries.iso2,]
fr2 <- fr[fr$iso2c %in% countries.iso2,]

# translate country names to french
tCountry <- fr2$country
tCountry<- as.character(country_names[match(fr2$iso2c, as.character(country_names[,1])), 9])
idx <- which(is.na(tCountry))
tCountry[idx ]<- fr2$country[idx]
fr2$country <- tCountry


# refactor factors
ff <- dplyr::filter(fr2, year == max(fr2$year)-1)
fr2$country <- factor(fr2$country, levels = ff$country[order(ff$value)])

# get some of the max countries
dplyr::filter(fr1, value >= quantile(fr1$value, 0.99, na.rm =T) )
dplyr::filter(fr1, year == 2012)
# check fertility rate China
dplyr::filter(fr2, country == "Chine")

frp <- plotMultiLineChart(fr1, fr2, fr3, title = "Taux de fécondité") + scale_y_continuous("", limits = c(1, 9.25), breaks = 1:9)
frp
pdf("fertilityRate.pdf", width = 12, height = 12 * 1.3 )
frp2 <- frp + geom_hline(yintercept = 2.1, color = "darkgrey")
dev.off()

devSVG(file = "fertilityRateR.svg", width = 12, height = 12 * 1.3)
frp2
dev.off()



wfr <- fr3[(!is.na(fr3$value)),]
diff(wfr[c(1, nrow(wfr)), 'value']) / wfr[1, 'value']
diff(wfr[c(1, nrow(wfr)), 'value'])
wfr[c(1, nrow(wfr)), 'year']