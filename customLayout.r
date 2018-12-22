#customLayout用于拼图特别方便，尤其是仪表盘布局,支持R内置的base绘图对象，ggplot2对象（与grid结合 ）
library(ggplot2)
library(customLayout)
# 创建拼图画布
lay1 <- lay_new( 
  mat = matrix(1:4, ncol = 2), 
# 矩阵分布，mat表示指定排版的数字矩阵 
  widths = c(3,2),             
# 设定宽度比例
  heights = c(2,1)             
# 设置高度比例)
lay_show(lay1);# 显示拼图画布 
# 创建第2个拼图画布，与第1个结构一样，只是比例不一样
lay2 <- lay_new(  matrix(1:4, nc = 2), widths = c(3, 5),heights = c(2, 4))
lay_show(lay2)

#1.1 画布合并
#lay_bind_col() 画布列合并
#lay_bind_col(x, y, widths = c(1, 1), addmax = TRUE) 参数widths表示指定合并宽度比
#lay_bind_row() 画布行合并
#lay_bind_row(x, y, heights = c(1, 1), addmax = TRUE) 参数heights表示指定合并高度比
library(ggplot2)library(customLayout)# 画布列合并
cl_1 <- lay_bind_col(lay1, lay2, widths = c(3, 1))  
# 指定合并宽度比为3:1
lay_show(cl_1)# 画布行合并
cl_2 <- lay_bind_row(lay1, lay2, heights = c(3, 1))  
# 指定合并高度比为3:1
lay_show(cl_2)# 多次合并
lay_show(lay_bind_col(cl_1, cl_2, widths = c(2:1)))

#1.2 画布嵌套
#关键函数：
#lay_split_field(lay, newlay, field)
#参数lay表示大画布，参数newlay表示要嵌套进去的小画布，field表示指定要嵌套的区域编号
library(ggplot2)
library(customLayout)
slay <- lay_split_field(lay1, lay2, field = 1)  
# 将画布lay2嵌套进lay1的第1个区域，即左上角格子
lay_show(slay)
slay2 <- lay_split_field(lay = lay2, new = lay1, field = 4)  
# 将画布lay1嵌套进lay2的第4个区域，即右下角格子
lay_show(slay2)

#1.3 填充图片
#关键函数：
#lay_set(layout) 将画布layout设置为绘图布局，用于base绘图对象
#lay_grid(grobs, lay, ...) 将绘图对象grobs填充到画布lay中， 用于ggplot2等绘图对象
#基础绘图：
par(mar = c(3, 2, 2, 1)) # 设定页边距# 创建排版画布
lay1  <- lay_new(   
  matrix(1:4, nc = 2), # 2行2列布局画布
  widths = c(3, 2),  heights = c(2, 1))
lay2 <- lay_new(matrix(1:3)) # 3行1列布局画布
cl <- lay_bind_col(lay1, lay2, widths = c(3, 1)) 
# 画布列合并
lay_show(cl)
lay_set(cl) 
# 设定绘图对象布局 
set.seed(123)# 绘图填充
plot(1:100 + rnorm(100)) # 填充到画布第1格
plot(rnorm(100), type = "l") # 填充到画布第2格
hist(rnorm(500))acf(rnorm(100)) # 填充到画布第4格
pie(c(3, 4, 6), col = 2:4)
pie(c(3, 2, 7), col = 2:4 + 3)
pie(c(5, 4, 2), col = 2:4 + 6)

#ggplot2绘图：
library(ggplot2)
library(customLayout)
library(gridExtra)# 创建排版画布
lay1 <- lay_new(matrix(1:2, ncol = 1))  # 2行1列画布
lay2 <- lay_new(matrix(1:3))  # 3行1列画布
cl <- lay_bind_col(lay1, lay2, widths = c(3, 1))  # 画布列合并# 创建数据cuts <- sort(unique(diamonds[["cut"]]), decreasing = TRUE)
make_cut_plot <- function(cut) {
    dd <- diamonds[cut == diamonds[["cut"]], ]  # 
    ggplot(dd) + 
      geom_point(aes(carat, price)) + 
     facet_wrap("cut")  # 封装分面}
plots <- lapply(cuts, make_cut_plot)  
# 对不同切割水平的进行作图
lay_grid(plots, cl)  
# 将绘图对象依次填充到cl画布中

