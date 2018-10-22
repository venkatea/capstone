# biplot.cluster performs a biplot on the first two principal components of
# the data used to create a clustering solution
# Programmer: Dan Putler
# Started: 09/08/02
# Last Modified: 20/08/02

cluster.biplot <- function (x,clusters){
        biplot(princomp(x), xlabs = as.character(clusters))
}
