# Main function of analysis of Parrondos Paradox
# require(ggplot2)
# require(scales)
# install.packages("scales")
library(gridExtra)
library(ggplot2)
library(scales)

ggplot2::elemen

opts=theme(
  legend.position = "bottom",
  legend.background = element_rect(colour = "black"),
  panel.background = element_rect(fill="gray98"),
  panel.border = element_rect(colour="black", fill=NA),
  axis.line = element_line(size = 0.5, colour = "black"),
  axis.ticks = element_line(colour="black"),
  panel.grid.major = element_line(colour="gray75", linetype = 2),
  panel.grid.minor = element_blank(),
  axis.text.y = element_text(colour="gray25", size=15),
  axis.text.x = element_text(colour="gray25", size=15),
  text = element_text(size=20),
  plot.title = element_text(size = 35))
PlayGameA = function(profit, x, c) {if (runif(1) < c-x) profit+1 else profit-1}
PlayGameB = function(profit, x1, c1, x2, c2) {if (profit%%3>0) PlayGameA(profit, x=x1, c=c1) else PlayGameA(profit, x=x2, c=c2)}
####################################################################
#EVOLUTION
####################################################################
noplays=500
alpha=0.005
profit0=0
results=data.frame(Play=0, ProfitA=profit0, ProfitB=profit0, ProfitAB=profit0)
for (i in 1:noplays) {results=rbind(results, c(i,
                                               PlayGameA(profit=results[results$Play==(i-1),2], x =alpha, c =0.5),
                                               PlayGameB(profit=results[results$Play==(i-1),3], x1=alpha, c1=0.75, x2=alpha, c2=0.1),
                                               if (runif(1)<0.5) PlayGameA(profit=results[results$Play==(i-1),4], x =alpha, c =0.5) else PlayGameB(profit=results[results$Play==(i-1),4], x1=alpha, c1=0.75, x2=alpha, c2=0.1)
))}
results=rbind(data.frame(Play=results$Play, Game="A",   Profit=results$ProfitA),
              data.frame(Play=results$Play, Game="B",   Profit=results$ProfitB),
              data.frame(Play=results$Play, Game="A+B", Profit=results$ProfitAB))
ggplot(results, aes(Profit, x=Play, y=Profit, color = Game)) +
  scale_x_continuous(limits=c(0,noplays), "Plays")+
  scale_y_continuous(limits=c(-75,75), expand = c(0, 0), "Profit")+
  labs(title="Evolution of profit games along 500 plays")+
  geom_line(size=3)+opts
####################################################################
#DISTRIBUTION
####################################################################
noplays=1000
alpha=0.005
profit0=0
results2=data.frame(Play=numeric(0), ProfitA=numeric(0), ProfitB=numeric(0), ProfitAB=numeric(0))
for (j in 1:100) {results=data.frame(Play=0, ProfitA=profit0, ProfitB=profit0, ProfitAB=profit0)
for (i in 1:noplays) {results=rbind(results, c(i,
                                               PlayGameA(profit=results[results$Play==(i-1),2], x =alpha, c =0.5),
                                               PlayGameB(profit=results[results$Play==(i-1),3], x1=alpha, c1=0.75, x2=alpha, c2=0.1),
                                               if (runif(1)<0.5) PlayGameA(profit=results[results$Play==(i-1),4], x =alpha, c =0.5)
                                               else PlayGameB(profit=results[results$Play==(i-1),4], x1=alpha, c1=0.75, x2=alpha, c2=0.1)))}
results2=rbind(results2, results[results$Play==noplays, ])}
results2=rbind(data.frame(Game="A", Profit=results2$ProfitA),
               data.frame(Game="B", Profit=results2$ProfitB),
               data.frame(Game="A+B", Profit=results2$ProfitAB))
ggplot(results2, aes(Profit, fill = Game)) +
  scale_x_continuous(limits=c(-150,150), "Profit")+
  scale_y_continuous(limits=c(0,0.02), expand = c(0, 0), "Density", labels = percent)+
  labs(title=paste("Parrondo's Paradox (",as.character(noplays)," plays)",sep=""))+
  geom_density(alpha=.75)+opts
