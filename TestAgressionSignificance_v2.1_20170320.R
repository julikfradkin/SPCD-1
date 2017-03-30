require('data.table')
require('foreign')
require('ggplot2')
require('Hmisc')
require('gridExtra')

aut.dt = read.spss('~/Desktop/02_SPCD_JulikFradkin/SPCD/ASDdisregulation 2317.4.sav',to.data.frame=TRUE)
aut.dt = data.table(aut.dt)
summary(aut.dt)
aut.dt

aut2.dt = read.spss('~/Desktop/02_SPCD_JulikFradkin/SPCD/ASDdisregulation_20170329.sav',to.data.frame=TRUE)
aut2.dt = data.table(aut2.dt)
summary(aut2.dt)
aut2.dt

#a81new, b81new agression to family
#a82new, b82new agression outside the family
#a83new, b83new agression to self

# 0 is no agression 1 is agression
# CSL a bunch of castle variables

pers.ag.dt = aut.dt[!is.na(LIAff) & !is.na(a83new) ]
dim(pers.ag.dt)
pers.ag.dt[,a83_Binomial := a83new - 1]
pers.ag.dt[,b83_Binomial := b83new - 1]

pers.ag.dt[,a82_Binomial := a82new - 1]
pers.ag.dt[,b82_Binomial := b82new - 1]

pers.ag.dt[,a81_Binomial := a81new - 1]
pers.ag.dt[,b81_Binomial := b81new - 1]

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
names = c('a83','b83','a82','b82','a81','b81')
liaf.sig  = data.table(confs1,confs2,ors,names)

ggplot() + geom_pointrange(data = liaf.sig, aes(x = names, y = ors, ymin = confs1, ymax = confs2)) + geom_hline(yintercept = 0) + ylim(c(-10,10))


#Now look at CSL


pers.ag.dt = aut.dt[!is.na(CSLNLSS) & !is.na(a83new) ]
dim(pers.ag.dt)
sum(is.na(pers.ag.dt[,.(a81new,a82new,a83new,b81new,b82new,b83new)]))

pers.ag.dt[,a83_Binomial := a83new - 1]
pers.ag.dt[,b83_Binomial := b83new - 1]

pers.ag.dt[,a82_Binomial := a82new - 1]
pers.ag.dt[,b82_Binomial := b82new - 1]

pers.ag.dt[,a81_Binomial := a81new - 1]
pers.ag.dt[,b81_Binomial := b81new - 1]

table(pers.ag.dt$CSLNLSS,pers.ag.dt$a83_Binomial)

ggplot(data = pers.ag.dt, aes(x = factor(a83_Binomial), y = CSLNLSS,fill = a83_Binomial)) + geom_boxplot() + geom_jitter(width = .2)
ggplot(data = pers.ag.dt, aes(x = factor(a82_Binomial), y = CSLNLSS,fill = a82_Binomial)) + geom_boxplot() + geom_jitter(width = .2)
ggplot(data = pers.ag.dt, aes(x = factor(a81_Binomial), y = CSLNLSS,fill = a81_Binomial)) + geom_boxplot() + geom_jitter(width = .2)

#______________________________________________General Case_______________________________________

aut.dt[,a83_Binomial := a83new - 1]
aut.dt[,b83_Binomial := b83new - 1]
aut.dt[,a82_Binomial := a82new - 1]
aut.dt[,b82_Binomial := b82new - 1]
aut.dt[,a81_Binomial := a81new - 1]
aut.dt[,b81_Binomial := b81new - 1]

plotCastle = function(Castle_type,agression){
  inn.dt = in.dt[!is.na(in.dt[[Castle_type]]) & !is.na(in.dt[[agression]])]
  
  signiff = round(wilcox.test(inn.dt[inn.dt[[agression]] == 0][[Castle_type]],
              inn.dt[inn.dt[[agression]] == 1][[Castle_type]])[[3]],3)
  
  (ggplot(data = inn.dt, aes( x = factor(inn.dt[[agression]]), y = inn.dt[[Castle_type]],fill = factor(inn.dt[[agression]]) ))
    + geom_boxplot()
    + geom_jitter(width = .2)
    + guides(fill=FALSE)
    + labs( x = Castle_type, y = strsplit(agression, split = "_")[[1]][1] ) 
    + geom_label(inherit.aes = FALSE,aes(label = signiff,y = 130 ,x = 1.5))
    + coord_cartesian(ylim = c(35,140))
    )
}

in.dt = aut.dt

csl1 = grid.arrange(plotCastle('CSLNLSS','a81_Binomial'),plotCastle('CSLNLSS','a82_Binomial'),plotCastle('CSLNLSS','a83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','01_CSLNLSS_20170322', '.png', sep = "") , plot = csl1)
csl2 = grid.arrange(plotCastle('CSLASSS','a81_Binomial'),plotCastle('CSLASSS','a82_Binomial'),plotCastle('CSLASSS','a83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','02_CSLASSS_20170322', '.png', sep = "") , plot = csl2)
csl3 = grid.arrange(plotCastle('CSLMCSS','a81_Binomial'),plotCastle('CSLMCSS','a82_Binomial'),plotCastle('CSLMCSS','a83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','03_CSLMCSS_20170322', '.png', sep = "") , plot = csl3)
csl4 = grid.arrange(plotCastle('CSLPJSS','a81_Binomial'),plotCastle('CSLPJSS','a82_Binomial'),plotCastle('CSLPJSS','a83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','04_CSLPJSS_20170322', '.png', sep = "") , plot = csl4)
srssc = grid.arrange(plotCastle('SRSSCI','a81_Binomial'),plotCastle('SRSSCI','a82_Binomial'),plotCastle('SRSSCI','a83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','05_SRSSCI_20170322', '.png', sep = "") , plot = srssc)





csl1 = grid.arrange(plotCastle('CSLNLSS','b81_Binomial'),plotCastle('CSLNLSS','b82_Binomial'),plotCastle('CSLNLSS','b83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','06_CSLNLSS_B_20170322', '.png', sep = "") , plot = csl1)
csl2 = grid.arrange(plotCastle('CSLASSS','b81_Binomial'),plotCastle('CSLASSS','b82_Binomial'),plotCastle('CSLASSS','b83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','07_CSLASSS_B_20170322', '.png', sep = "") , plot = csl2)
csl3 = grid.arrange(plotCastle('CSLMCSS','b81_Binomial'),plotCastle('CSLMCSS','b82_Binomial'),plotCastle('CSLMCSS','b83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','08_CSLMCSS_B_20170322', '.png', sep = "") , plot = csl3)
csl4 = grid.arrange(plotCastle('CSLPJSS','b81_Binomial'),plotCastle('CSLPJSS','b82_Binomial'),plotCastle('CSLPJSS','b83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','09_CSLPJSS_B_20170322', '.png', sep = "") , plot = csl4)
srssc = grid.arrange(plotCastle('SRSSCI','b81_Binomial'),plotCastle('SRSSCI','b82_Binomial'),plotCastle('SRSSCI','b83_Binomial'),ncol = 2)
ggsave( paste('~/Desktop/02_SPCD_JulikFradkin/SPCD/','10_SRSSCI_B_20170322', '.png', sep = "") , plot = srssc)







