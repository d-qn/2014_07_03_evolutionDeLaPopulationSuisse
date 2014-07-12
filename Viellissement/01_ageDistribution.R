############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
font <- "Open Sans"

library(animation)

data.file <- 'contenate_allData.csv'

############################################################################################
###		Load all data in one data.frame
############################################################################################

data <- read.csv(data.file, stringsAsFactors = F)
colnames(data)[2:ncol(data)] <- gsub("^X", "", colnames(data)[2:ncol(data)])

data <- data.frame(Age = data[,1], melt(data[,2:ncol(data)]))
colnames(data) <- c('Age', 'Annee', "value")
data$Annee <- as.numeric(as.character(data$Annee))
datan <- data
data$Age <- reorder(data$Age, as.numeric(gsub("\\+$", "", as.character(data$Age))))
datan$Age <- as.numeric(gsub("\\+$", "", as.character(datan$Age)))

# compute for each year, the proportion of by age group
prop <- ddply(data, .(Annee), summarize,
	Age = Age,
	value = value,
	prop = value / sum(value) * 100)


#ggplot(data = datan) + geom_histogram(aes(Age, value)) + facet_wrap( ~ Annee) + theme_minimal()


#ggplot(data = prop) + geom_bar(aes(Age, prop), stat = "identity") + facet_wrap( ~ Annee) + theme_minimal()
xlabel <- rep('', nlevels(data$Age))
idx.x <- c(seq(min(as.numeric(data$Age)), max(as.numeric(data$Age)), 10), nlevels(data$Age))
xlabel[idx.x]<- levels(data$Age)[idx.x]
descr <- paste("proportion de la population suisse par âge (", paste(range(data$Annee), collapse ="-"), ")", sep="")
title <- 'Vieillissement de la population'

# load logo
g <- rasterGrob(swi_logo, interpolate=TRUE)


plotayear <- function(data, a, title = "", descr = "") {

	dd <- prop[prop$Annee == a,]
	ghist <- ggplot(data = dd) + geom_bar(aes(Age, prop), size =0.01, stat = "identity",
		color = swi_9palette[4], fill = swi_9palette[4]) + ggtheme  + scale_x_discrete("Age", xlabel) +
		scale_y_continuous(name = "%", limits = c(0, max(prop$prop)), expand = c(0.01,0.01)) +
		geom_text(data = data.frame(x = levels(prop$Age)[nlevels(prop$Age)-5], y = max(prop$prop)-0.7, label = as.character(a)),
		aes(label = label, x = x, y = y), family = font, alpha = 0.4, size = 50,  color = swi_9palette[9], hjust = 1) +
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop), label = title), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 5, hjust = 0, vjust =0,
		fontface ="bold") +
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.08, label = descr), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 4, hjust = 0,vjust =0) +
		theme(axis.text = element_text(size = 14, family = font, lineheight = 0), plot.margin = unit(c(0.5,1,1.1,0), "lines"))
	#ghista <- ghist + annotation_custom(grob = textGrob("swissinfo.ch"), xmin = 95, xmax = 100, ymin = -0.3, ymax = -0.3)
	#ghist <- ghist + geom_vline(xintercept = mean(sum(dd$value * as.numeric(dd$Age)-1) / sum(dd$value)), colour = swi_9palette[7], alpha = 0.4)
	ghista <- ghist + annotation_custom(grob = g, xmin = nlevels(prop$Age)-nlevels(prop$Age)/10, xmax = nlevels(prop$Age),
	ymin = -0.23, ymax = -0.3)
    gt <- ggplot_gtable(ggplot_build(ghista))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.newpage()
    grid.draw(gt)
}
a <- unique(prop$Annee)[10]
plotayear(data, a, title, descr)

#filter the data to only have even years
data.sub <- data[data$Annee %% 2 ==0,]


saveGIF({
	for(a in unique(data.sub$Annee)) {
		plotayear(data.sub, a, title = title, descr)
	}
}, movie.name = "populationAge.gif", interval = 0.2, nmax = 50, ani.width = 640, ani.height = 570, loop=TRUE, outdir = getwd())


