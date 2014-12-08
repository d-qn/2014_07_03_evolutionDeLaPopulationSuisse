############################################################################################
###		SETTINGS
############################################################################################

source("~/swissinfo/_helpers/helpers.R")
library(animation)

displayStatistics <- F
data.file <- 'contenate_allData.csv'

text.file <- 'trad.csv'

############################################################################################
###		HELPERS
############################################################################################

ggthemeNoFont <- {
    #based theme_bw eliminates baground, gridlines, and chart border
  theme_bw() + theme(plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_line(size = 0.2),
   plot.title = element_text(hjust = 0),panel.grid.major = element_line(colour = "#efe9e0")
 )
}
############################################################################################
###		Load translation data
############################################################################################

text <- read.csv(text.file, stringsAsFactors = F)

############################################################################################
###		Load all data in one data.frame
############################################################################################

data <- read.csv(data.file, stringsAsFactors = F)
colnames(data)[2:ncol(data)] <- gsub("^X", "", colnames(data)[2:ncol(data)])

data <- data.frame(Age = data[,1], melt(data[,2:ncol(data)]))
colnames(data) <- c('Age', 'Annee', "value")
data$Annee <- as.numeric(as.character(data$Annee))
# keep age in datan for statistics computation
datan <- data
data$Age <- reorder(data$Age, as.numeric(gsub("\\+$", "", as.character(data$Age))))
datan$Age <- as.numeric(gsub("\\+$", "", as.character(datan$Age)))

# compute for each year, the proportion of by age group
# library(plyr)
# prop <- plyr::ddply(data, .(Annee), summarize,
# 	Age = Age,
# 	value = value,
# 	prop = value / sum(value) * 100)

prop <- do.call(rbind, by(data, data$Annee, function(d) {
	data.frame(Age = d$Age, Annee = d$Annee[1], value = d$value, prop = d$value / sum(d$value) * 100)
}))
rownames(prop) <- NULL

xlabel <- rep('', nlevels(data$Age))
idx.x <- c(seq(min(as.numeric(data$Age)), max(as.numeric(data$Age)), 10), nlevels(data$Age))
xlabel[idx.x]<- levels(data$Age)[idx.x]

# load logo
g <- rasterGrob(swi_logo, interpolate=TRUE)

### Get some key numbers
if(displayStatistics) {
	sum(dplyr::filter(datan, Annee == 1860, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 1860, Age >= 20, Age <= 64)$value)
	sum(dplyr::filter(datan, Annee == 1901, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 1901, Age >= 20, Age <= 64)$value)
	sum(dplyr::filter(datan, Annee == 2012, Age > 65)$value) / sum(dplyr::filter(datan, Annee == 2012, Age >= 20, Age <= 64)$value)
}

plotayear2 <- function(data, a, title = "", descr = "", xlab = 'Age', ylab = "%", family = font) {

	dd <- prop[prop$Annee == a,]
	ghist <- ggplot(data = dd) + geom_bar(aes(Age, prop), size =0.01, stat = "identity",
		color = swi_9palette[4], fill = swi_9palette[5]) + ggthemeNoFont  + scale_x_discrete(xlab, xlabel) +
		scale_y_continuous(name = ylab, limits = c(0, max(prop$prop)), expand = c(0.005,0.005)) +
		# the year in big
		geom_text(data = data.frame(x = levels(prop$Age)[nlevels(prop$Age)-5], y = max(prop$prop)-0.67, label = as.character(a)),
		aes(label = label, x = x, y = y), family = family, alpha = 0.6, size = 60,  color = swi_9palette[9], hjust = 1) +
		# the title
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.05, label = title), aes(label = label, x = x, y = y), family = family, alpha = 1, size = 9, hjust = 0, vjust = 0,
		fontface ="bold") +
		# the description
		geom_text(data = data.frame(x = levels(prop$Age)[1],
		y = max(prop$prop)-0.157, label = descr), aes(label = label, x = x, y = y), family = family, alpha = 0.8, size = 6, hjust = 0,vjust =0) +
		# theme
		theme(axis.text = element_text(size = rel(1), lineheight = 0), plot.margin = unit(c(0.8,1,1.1,0), "lines"),
		axis.title =  element_text(size = rel(1.5)), text = element_text(family = family))
	ghista <- ghist + annotation_custom(grob = g, xmin = nlevels(prop$Age)-nlevels(prop$Age)/8, xmax = nlevels(prop$Age),
	ymin = -0.15, ymax = -0.22)
    gt <- ggplot_gtable(ggplot_build(ghista))
    gt$layout$clip[gt$layout$name=="panel"] <- "off"
	grid.newpage()
    grid.draw(gt)
}

a <- unique(prop$Annee)[10]
plotayear2(data, a, text[1,1], text[2,1], text[3,1], text[4,1], text[5,1])

# some tests for exotic characters
plotayear2(data, a, iconv(text[1,4]), text[2,4], text[3,4], family = "")
i <- 5
plotayear2(data, a, iconv(text[1,i]), text[2,i], text[3,i], family = "")
plotayear2(data, a, iconv(text[1,i]), text[2,i], text[3,i] )


# take only every 4 years
data.sub <- data[data$Annee %% 4 == 0,]

for(i in 1:ncol(text)) {

	fontToBeUsed <- text[5,i]
	cat("\n", colnames(text)[i], "\t with font:", fontToBeUsed, "\n")
	saveGIF({
		for(a in c(unique(data.sub$Annee), 2012)) {
			plotayear2(data.sub, a, title = text[1,i], descr = text[2,i], xlab = text[3,i], ylab = text[4,i], family = fontToBeUsed)
		}
	}, movie.name = paste("populationAge_", colnames(text)[i], ".gif", sep =""), interval = 0.35, nmax = 50, ani.width = 640*1.1,
	ani.height = 640*1.1, loop = TRUE, outdir = getwd())
}







# i <- 1
# plotayear2(data, a, iconv(text[1,i]), text[2,i], text[3,i], family = "Hei")
#
#
# i <- 4
# plotayear2(data, a, iconv(text[1,i]), text[2,i], text[3,i], family = "Hei")
# i <- 5
# plotayear2(data, a, iconv(text[1,i]), text[2,i], text[3,i], family = "Osaka")
#
#
# #Sys.setlocale(locale =c ('zh_CN.UTF-8'))
# x <- read.csv(textConnection("
# 名称,类,学生
# 木材,2,2
# 表,3,4
# 笔,4,2
# 垃圾桶,5,6
# 杯,6,3"), header = TRUE)
# rownames(x) <- x[,1]
# x <- x[,-1]
# barplot(t(x), horiz = TRUE, beside = TRUE, legend.text = TRUE)
# barplot(t(x), horiz = TRUE, beside = TRUE, legend.text = TRUE, family = "Hei")