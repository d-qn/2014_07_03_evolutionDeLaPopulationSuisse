source("~/swissinfo/_helpers/helpers.R")
require(rgdal)
require(rgeos)

dates <- c(1850, 2013)
breaks <- c(0, 50, 200, 300, 500, 750, 1000, 2000, Inf)


currPath <- getwd()
sapply(dates, function(date) {
	setwd(currPath)
	table <- read.csv(paste("densité_", date, ".csv", sep =""), stringsAsFactors = F)

	# available layers: municipalities , cantons, districts
	layer <- 'municipalities'

	#setwd(swissMapShp2010.path)
	setwd(swissMapShp.path)
	ch <- readOGR(".", layer = layer)

	setwd(swissMapShp.path)
	lake <- readOGR(".", layer = "lakes")
	country <- readOGR(".", layer = "country")

	# convert to data frame for plotting with ggplot - takes a while
	ch.df <- formatShp(ch)
	lake.df <- formatShp(lake)

	bin <- cut(table[,3], breaks)

	idxM <- match(ch.df$BFS_NUMMER, table$ID)
	browser()
	# debug non matching commune
	# ch.df[match(levels(ch.df$BFS_NUMMER)[which(is.na(idxM))], ch.df$BFS_NUMMER),]
	# nm <- as.character(ch.df[match(levels(ch.df$BFS_NUMMER)[which(is.na(idxM))], ch.df$BFS_NUMMER),'NAME'])

	ch.df$bin <- bin[idxM]

	gp <- ggplot(ch.df, aes(x = long, y = lat, group = group)) +
    	geom_polygon(colour = alpha("black", 0), size = 0.01, aes(fill = bin)) + scale_fill_brewer(palette = "Purples") +
		geom_polygon(data = country, fill = NA, colour = alpha("black", 0.8), size = 0.2) +
		geom_polygon(data = lake.df, fill = alpha("lightgrey", alpha = 1), colour = alpha("lightgrey", alpha = 1)) + ggtheme2
	gp

	gp <- ggplot(ch.df, aes(x = long, y = lat, group = group)) +
    	geom_polygon(colour = alpha("black", 0), size = 0.01, aes(fill = bin)) + scale_fill_brewer(palette = "Purples") +
		geom_polygon(data = country, fill = NA, colour = alpha("white", 0), size = 0.2) + ggtheme2
	gp

	setwd(currPath)
	ggsave(paste("map_" , date, ".png", sep =""), plot=gp)

})

