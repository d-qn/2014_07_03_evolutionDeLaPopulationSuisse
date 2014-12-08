############################################################################################
###		SETTINGS
############################################################################################
source("~/swissinfo/_helpers/helpers.R")
library(animation)

data.file <- 'contenate_allData.csv'


data <- read.csv(data.file, stringsAsFactors = F)
colnames(data)[2:ncol(data)] <- gsub("^X", "", colnames(data)[2:ncol(data)])

data <- data.frame(Age = data[,1], melt(data[,2:ncol(data)]))
colnames(data) <- c('Age', 'Annee', "value")
data$Annee <- as.numeric(as.character(data$Annee))
data$Age <- as.numeric(gsub("\\+", "", data$Age))

range(tapply(data$value, data$Annee, sum))

annees <- unique(data$Annee)

a <- 1900
ageCutoff <- 60

dd <- data[data$Annee == a,]
b <- sum(dd$value)
p <- sum(dplyr::filter(dd, Age >= ageCutoff)$value) / sum(dd$value)

df1 <- data.frame(y = b, x = 1)
ggplot(data = df1) + geom_bar(aes(x, y), stat="identity", width = 2) + ggtheme_ygrid +
	scale_y_continuous(name = "", limits = c(0, 8.5 * 10^6), expand = c(0.0,0.0), labels  = function(x) x / 10^6) +
	theme(axis.ticks.x = element_blank(), axis.line.x = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text())


multiplot


“ggplot(BOD, aes(x=Time, y=demand)) + geom_bar()”

saveGIF({
	for(a in annees) {
		plotayear(data, a, title = title, descr)
	}
}, movie.name = "popAge.gif", interval = 0.2, nmax = 50, ani.width = 640, ani.height = 570, loop = TRUE, outdir = getwd())

