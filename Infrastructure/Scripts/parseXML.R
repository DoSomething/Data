library(XML)

doc <- xmlParse('~/Downloads/100000001-18143293917.txt')

rootNode <- xmlRoot(doc)

xmlList <- xmlSApply(rootNode,function(x) xmlSApply(x, xmlValue))

data <- data.frame(t(xmlList),row.names=NULL)

flattened <- data.frame(do.call(c, unlist(data, recursive=F)))
