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


ggplot(data = datan) + geom_histogram(aes(Age, value)) + facet_wrap( ~ Annee) + theme_minimal()


#ggplot(data = prop) + geom_bar(aes(Age, prop), stat = "identity") + facet_wrap( ~ Annee) + theme_minimal()
xlabel <- rep('', nlevels(data$Age))
idx.x <- c(seq(min(as.numeric(data$Age)), max(as.numeric(data$Age)), 10), nlevels(data$Age))
xlabel[idx.x]<- levels(data$Age)[idx.x]
title <- "Proportion de la population Suisse par âge"

plotayear <- function(data, a) {
	ghist <- ggplot(data = prop[prop$Annee == a,]) + geom_bar(aes(Age, prop), size =0.01, stat = "identity", color = swi_9palette[9],
	fill = swi_9palette[3]) + ggtheme + ylim(c(0, max(prop$prop))) + scale_x_discrete("Age", xlabel) + ylab ("%") +
	geom_text(data = data.frame(x = levels(prop$Age)[70], y = max(prop$prop)-1.1, label = as.character(a)),
	aes(label = label, x = x, y = y), font = font, alpha = 0.1, size = 66) + geom_text(data = data.frame(x = levels(prop$Age)[1],
	y = max(prop$prop)-0.05, label = title), aes(label = label, x = x, y = y), font = font, alpha = 1, size = 6, hjust = 0,
	fontface ="bold")
	plot(ghist)
}
a <- unique(prop$Annee)[1]
plotayear(data, a)




saveGIF({
	for(a in unique(prop$Annee)) {
		plotayear(data, a)
	}
}, movie.name = "populationAge.gif", interval = 0.1, nmax = 50, ani.width = 640, ani.height = 500)


