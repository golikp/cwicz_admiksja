#Read table with results for K = 3
tbl <- read.table("EuPop.3.Q")
indTable<-read.table("EuPop.fam", col.names=c("Pop","Sample","c1","c2","c3","c4"))
merged=cbind(tbl, indTable)

#Sort by population
orderedPop = merged[order(merged$Pop),]

#Sort by ancestral proportions
orderedQ1 = merged[order(merged$V1),]
orderedQ2 = merged[order(merged$V2),]
orderedQ3 = merged[order(merged$V3),]

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

pdf(file="plotPop.pdf", 20,4)
plot_admix(orderedPop, 3)
dev.off()

pdf(file="plotQ1.pdf", 20,4)
plot_admix(orderedQ1, 3)
dev.off()

pdf(file="plotQ2.pdf", 20,4)
plot_admix(orderedQ2, 3)
dev.off()

pdf(file="plotQ3.pdf", 20,4)
plot_admix(orderedQ3, 3)
dev.off()

#TODO: use a variable and loops to make it work for any K value not just 3
