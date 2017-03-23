require('data.table')
require('foreign')
require('ggplot2')
require('Hmisc')
setwd('~/Dropbox/')

aut.dt = read.spss('research/Brzustowicz/March 24/December 7 2016/ASDdisregulation 2317.4.sav',to.data.frame=TRUE)
aut.dt = data.table(aut.dt)
summary(aut.dt)
non_na_iq.dt= aut.dt[!is.na(a81new) & !is.na(IQ)]

clnlss.dt = aut.dt[!is.na(LIAff) & !is.na(CSLNLSS) & !is.na(a81new)]
clnlss.dt[,a81_Binomial := a81new - 1]

m0 =glm(clnlss.dt$a81_Binomial ~ clnlss.dt$CSLPJSS,family = 'binomial')
summary(m0)


#Personal Agression logistic model test
pers.ag.dt = aut.dt[!is.na(LIAff) & !is.na(a83new) ]
dim(pers.ag.dt)
pers.ag.dt[,a83_Binomial := a83new - 1]
pers.ag.dt[,b83_Binomial := b83new - 1]

pers.ag.dt[,a82_Binomial := a82new - 1]
pers.ag.dt[,b82_Binomial := b82new - 1]

pers.ag.dt[,a81_Binomial := a81new - 1]
pers.ag.dt[,b81_Binomial := b81new - 1]


m0 =glm(pers.ag.dt$a83_Binomial ~ pers.ag.dt$LIAff,family = 'binomial')
summary(m0)

table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$a83_Binomial)

fisher_a83 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$a83_Binomial))
pva_liaf = fisher_a83[1]
conf1_liaf = unlist(fisher_a83[2])[1]
conf2_liaf = unlist(fisher_a83[2])[2]
or_liaf = unlist(fisher_a83[3])

fisher_b83 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$b83_Binomial))
pva_liaf2 = fisher_b83[1]
conf1_liaf2 = unlist(fisher_b83[2])[1]
conf2_liaf2 = unlist(fisher_b83[2])[2]
or_liaf2 = unlist(fisher_b83[3])


fisher_a82 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$a82_Binomial))
pva_liaf3 = fisher_a82[1]
conf1_liaf3 = unlist(fisher_a82[2])[1]
conf2_liaf3 = unlist(fisher_a82[2])[2]
or_liaf3 = unlist(fisher_a82[3])

fisher_b82 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$b82_Binomial))
pva_liaf4 = fisher_b82[1]
conf1_liaf4 = unlist(fisher_b82[2])[1]
conf2_liaf4 = unlist(fisher_b82[2])[2]
or_liaf4 = unlist(fisher_b82[3])

fisher_a81 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$a81_Binomial))
pva_liaf5 = fisher_a81[1]
conf1_liaf5 = unlist(fisher_a81[2])[1]
conf2_liaf5 = unlist(fisher_a81[2])[2]
or_liaf5 = unlist(fisher_a81[3])

fisher_b81 = fisher.test(table(factor(pers.ag.dt$LIAff,levels = c(1,0)),pers.ag.dt$b81_Binomial))
pva_liaf6 = fisher_b81[1]
conf1_liaf6 = unlist(fisher_b81[2])[1]
conf2_liaf6 = unlist(fisher_b81[2])[2]
or_liaf6 = unlist(fisher_b81[3])
pvals = sapply(c(pva_liaf,pva_liaf2,pva_liaf3,pva_liaf4,pva_liaf5,pva_liaf6), round ,2)




confs1 = sapply(c(conf1_liaf, conf1_liaf2,conf1_liaf3,conf1_liaf4,conf1_liaf5,conf1_liaf6), log2)
confs2 = sapply(c(conf2_liaf, conf2_liaf2,conf2_liaf3,conf2_liaf4,conf2_liaf5,conf2_liaf6), log2)
ors = sapply(c(or_liaf, or_liaf2,or_liaf3,or_liaf4,or_liaf5,or_liaf6), log2)

errbar(yplus = confs1, yminus = confs2,y = ors, x = c(0,1,2,3,4,5), ylim = c(-10,10), xlim = c(-1,6))
abline(h = 0)
text(y = -10, labels = c('a83','b83','a82','b82','a81','b81'), x = c(0,1,2,3,4,5))
text(y = 3, labels = pvals, x = c(0,1,2,3,4,5))


errbar(yplus = confs1, yminus = confs2,y = ors, x = c('a83','b83','a82','b82','a81','b81'), ylim = c(-9,9))
abline(v = 0)



#a82 Agression to care givers
ag.care.dt = aut.dt[!is.na(LIAff) & !is.na(a82new) ]
dim(ag.care.dt)
ag.care.dt[,a82_Binomial := a82new - 1]
#pers.ag.dt[,b83_Binomial := b83new - 1]

m0 =glm(ag.care.dt$a82_Binomial ~ ag.care.dt$CSLINSS,family = 'binomial')
summary(m0)
