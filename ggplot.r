#给图里面加箭头
library(ggrepel)
ggplot(mtcars)+ 
  geom_point(aes(wt, mpg), color="grey", size=5)+
  geom_label_repel(aes(wt, mpg, fill=factor(cyl),label=rownames(mtcars)，angle=ifelse(mtcars$cyl==4, 90, 0)),arrow = arrow(length=unit(0.01, "npc")))
  

#桃心
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
t =seq(0,2*pi,by=0.1)
x =16*sin(t)^3
y =13*cos(t)-5*cos(2*t)-2*cos(3*t)-cos(4*t)
a =(x -min(x))/(max(x)-min(x))
b =(y -min(y))/(max(y)-min(y))
hearted=data.frame(t,x,y,a,b)
ggplot(hearted,aes(a,b))+
  geom_path(color=2,size=2)+
  theme_map()+
  annotate("text",label="你如packages\n我如R\n没有你就没有我的全世界\n
           ",color="red",x=0.5,y=0.6,size=6)
