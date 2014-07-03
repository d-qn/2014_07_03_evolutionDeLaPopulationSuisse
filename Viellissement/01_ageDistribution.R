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
	prop = value / sum(value) * 100)


#ggplot(data = datan) + geom_histogram(aes(Age, value)) + facet_wrap( ~ Annee) + theme_minimal()


#ggplot(data = prop) + geom_bar(aes(Age, prop), stat = "identity") + facet_wrap( ~ Annee) + theme_minimal()
xlabel <- rep('', nlevels(data$Age))
idx.x <- c(seq(min(as.numeric(data$Age)), max(as.numeric(data$Age)), 10), nlevels(data$Age))
xlabel[idx.x]<- levels(data$Age)[idx.x]
title <- "Proportion de la population Suisse par âge"


plotayear <- function(data, a) {
	ghist <- ggplot(data = prop[prop$Annee == a,]) + geom_bar(aes(Age, prop), size =0.01, stat = "identity", color = 	 	 	 		swi_9palette[3], fill = swi_9palette[3]) + ggtheme  + scale_x_discrete("", xlabel) +
	scale_y_continuous(name = "%", limits = c(0, max(prop$prop)), expand = c(0.01,0.01)) +
	geom_text(data = data.frame(x = levels(prop$Age)[nlevels(prop$Age)], y = max(prop$prop)-0.6, label = as.character(a)),
	aes(label = label, x = x, y = y), family = font, alpha = 0.4, size = 60,  color = swi_9palette[9], hjust = 1) +
	geom_text(data = data.frame(x = levels(prop$Age)[1],
	y = max(prop$prop)-0.07, label = title), aes(label = label, x = x, y = y), family = font, alpha = 1, size = 7, hjust = 0) + 	theme(axis.text = element_text(size = 14, family = font, lineheight = 0), plot.margin = unit(c(0,2,4,0), "lines"))
	ghista <- ghist + annotation_custom(grob = textGrob("swissinfo.ch"), xmin = 95, xmax = 100, ymin = -0.3, ymax = -0.3)
    gt <- ggplot_gtable(ggplot_build(ghista))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.newpage()
    grid.draw(gt)
}
a <- unique(prop$Annee)[10]
plotayear(data, a)




saveGIF({
	for(a in unique(prop$Annee)) {
		plotayear(data, a)
	}
}, movie.name = "populationAge.gif", interval = 0.1, nmax = 50, ani.width = 900, ani.height = 600)


