#给图里面加箭头
library(ggrepel)
ggplot(mtcars)+ 
  geom_point(aes(wt, mpg), color="grey", size=5)+
  geom_label_repel(aes(wt, mpg, fill=factor(cyl),label=rownames(mtcars)),arrow = arrow(length=unit(0.01, "npc")))
  
