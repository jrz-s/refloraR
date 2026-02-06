
data<-read.table("clipboard", header = T, sep = "\t")
py <- aggregate(data, by = list(data$yearcollected), FUN = 'NROW')

plot(py$Group.1[py$Group.1 > 1975],py$species[py$Group.1 > 1975], type = 'l')
cor.test(py$Group.1[py$Group.1 > 1975],py$species[py$Group.1 > 1975], type = 'l')
