x<-matrix(c(10,7,46,53,17,17,40,9,2,13,8,11,4,4,0,3,2,3,4,0),ncol=2, byrow =TRUE)
x

chisq.test(x)
fisher.test(x[6:10,],simulate.p.value = TRUE,B=150000)
fisher.test(x[1:4,],simulate.p.value = TRUE,B=150000)

fisher.test(x[-c(4,5),],simulate.p.value = TRUE,B=150000)
chisq.test(x[-c(4,5),])

fisher.test(x[c(4,5),],simulate.p.value = TRUE,B=150000)


