barNaming <- function(vec) { #Do not repeat the same label
	retVec <- vec
	for (k in 2:length(vec)) {
		if (vec[k - 1] == vec[k])
		retVec[k] <- ""
	}
	return(retVec)
}

plot_admix <- function(q_table, K) {
	mp <- barplot(t(as.matrix(q_table[, 1:K])), col=rainbow(K),border=0, space=0, axes=F, axisname=F)
	text(mp, par("usr")[3], labels = barNaming(q_table$Pop), srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex=.2)	
}

K <- 3 
basename <- "EuAs"
tblName <- paste(basename, as.character(K), "Q", sep = ".")
famName <- paste(basename, "fam",  sep = ".")
plotName <- paste("plot", as.character(K), "pdf", sep = ".")

tbl <- read.table(file = tblName)
indTable<-read.table(file = famName, col.names = c("Pop","Sample","c1","c2","c3","c4"))
merged=cbind(tbl, indTable)

#Sort by population
orderedPop = merged[order(merged$Pop),]

#Plot
pdf(file = plotName, 20,4)
	plot_admix(orderedPop, K)
dev.off()


