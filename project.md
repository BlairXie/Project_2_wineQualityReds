
TITLE by 谢冰
========================================================

```{r echo=FALSE, message=FALSE, warning=FALSE, packages}
library(ggplot2)
library(GGally)
library(memisc)
```

```{r echo=FALSE, message=FALSE, warning=FALSE, Load_the_Data}
# Load the Data
wineQualityReds <- read.csv("wineQualityReds.csv")
```


##### 这个报告探索了一个整洁的的数据集包含1599种红酒，以及11个关于关于酒的化学成分的变量。至少3名葡萄酒专家对每种酒的质量进行了评分，分数在0（非常差）和10（非常好）之间。我要探究的是固定酸度，挥发性酸度，柠檬酸，残糖，氯化物，游离二氧化硫，总二氧化硫，密度，pH值，硫酸盐，酒精对红酒质量评级的影响。

# Univariate Plots Section


```{r echo=FALSE, message=FALSE, warning=FALSE}
dim(wineQualityReds)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
str(wineQualityReds)
```

#####这个数据集含有12个有用变量，其中X只是序号，共有1599个观察值。

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds[, c(2:13)])
```

#####这里我发现游离二氧化硫、总二氧化硫的最大值和第3分位数的差别很大，可能包含异常值。

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = quality)) +
  geom_bar() 
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds$quality)
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$quality, useNA = "ifany")
```


#####绝大部分红葡萄酒质量得了5、6、7这三个分数，其中质量为5分和6分的红葡萄酒差最多，少部分落在4分，但是还有绝少数落在3分和8分，虽然是按0~10给分，但是质量太低红葡萄酒当然没有市场，也就是为什么没有3分以下的缘故， 质量为8分的红葡萄酒也是极少的，我接下来想探索是哪个或者哪些化学成分影响了红葡萄酒的质量。 

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = fixed.acidity)) + 
  geom_histogram()
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = fixed.acidity)) + 
  geom_histogram(binwidth = 0.05)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds$fixed.acidity)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$fixed.acidity, useNA = "ifany")
```

#####调整binwidth后发现，固定酸度是以0.1为步长的一组离散的数值，我在想可能是测量精度有限。大部分红葡萄酒的固定酸度落在7~9g/dm^3之间。




```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = volatile.acidity)) + 
  geom_histogram()
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = volatile.acidity)) + 
  geom_histogram(binwidth = 0.01) +
  scale_x_continuous(breaks = seq(0, 1.6, 0.1)) +
  scale_y_continuous(breaks = seq(0, 60, 10)) 
  
  
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds$volatile.acidity)
```

#####大部分的红酒的挥发性酸度都在0.39~0.64之间，极少数超过1g/dm^3。固定酸度和挥发性酸度在分布上看非常相似，说明这两个理化指标存在一定的相似性。

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = citric.acid)) + 
  geom_histogram()
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = citric.acid)) + 
  geom_histogram(binwidth = 0.01) +
  scale_y_continuous(breaks = seq(0, 150, 25))
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds$citric.acid)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$citric.acid, useNA = "ifany")
```

#####有132种红酒的柠檬酸是0，可能和材料说明柠檬酸少量发现有关，也可能是数据缺失，其他种红酒中柠檬酸含量为0.02，0.24和0.49较多，较为突出，红酒柠檬酸大于0.55的很少。柠檬酸这项指标在不同品种红酒中呈现正偏态分布。

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = residual.sugar)) + 
  geom_histogram()
```



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = chlorides)) + 
  geom_histogram()
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = free.sulfur.dioxide)) + 
  geom_histogram()
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$free.sulfur.dioxide, useNA = "ifany")
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = total.sulfur.dioxide)) + 
  geom_histogram()
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$total.sulfur.dioxide,useNA = "ifany")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = sulphates)) + 
  geom_histogram() 
```

#####上面所展示的几个变量,残糖、氯化物、游离二氧化硫、总二氧化硫、硫酸盐不同程度出现了长尾，对其做对数转换再进行观察。并且虽然刚开始我觉得游离二氧化硫、总二氧化硫的最大值和第3分位数的差别很大，可能包含异常值，但是实际上只是出现了长尾现象，只是正偏态分布的表现，并非是异常值。

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = log(residual.sugar))) + 
  geom_histogram()
```



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = log(chlorides))) + 
  geom_histogram()
```



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = log(free.sulfur.dioxide))) + 
  geom_histogram()
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = log(total.sulfur.dioxide))) + 
  geom_histogram()
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = log(sulphates))) + 
  geom_histogram()
```

#####改变长尾数据，以上指标，残糖、氯化物、游离二氧化硫、总二氧化硫、硫酸盐均呈现正态分布或者近似正态分布。

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = density)) + 
  geom_histogram()
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = pH)) + 
  geom_histogram()
```


#####密度和PH呈现正态分布，说明所有的样本该两项指标符合正常，没有明显的特征。



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = alcohol)) + 
  geom_histogram()
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = alcohol)) + 
  geom_histogram(binwidth = 0.08) +
  scale_x_continuous(breaks = seq(8, 15, 0.5)) +
  scale_y_continuous(breaks = seq(0, 200, 25))
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
summary(wineQualityReds$alcohol)
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
table(wineQualityReds$alcohol,useNA = "ifany")
```

#####通过分析，红酒的酒精含量百分比绝大多种都分布在9.4%和9.5%，并且往酒精含量增加的方向红酒分布呈现下降趋势。而且酒精含量占比总是断断续续分布在一定区间，某些区间是没有的。酒精这项指标在不同品种红酒中呈现正偏态分布。


# Univariate Analysis

### What is the structure of your dataset?

#####数据集中有1599种红酒，具有11个输入变量，包括客观测试（固定酸度，挥发性酸度，柠檬酸，残糖，氯化物，游离二氧化硫，总二氧化硫，密度，pH值，硫酸盐，酒精）和1个输出变量，基于感官数据（质量）。

#####（最差）—————>（最好）

#####质量：3,4,5,6,7,8

### What is/are the main feature(s) of interest in your dataset?

#####数据集的主要特征是质量和酒精。可能酒精和其他变量的组合可以更好的预测质量。




### What other features in the dataset do you think will help support your \
investigation into your feature(s) of interest?

#####密度和PH值是正态分布，说明几乎所有的红酒在这两个指标上几乎是无差别的。而固定酸度，挥发性酸度，柠檬酸，残糖，氯化物，游离二氧化硫，总二氧化硫，硫酸盐这些变量不同程度出现长尾，正偏态分布，搭配酒精应该能更好的预测变量，有助于分析。


### Of the features you investigated, were there any unusual distributions? 

#####不寻常的分布：1,酒精含量占比总是断断续续分布在一定区间，某些区间是没有的。2,有132种红酒的柠檬酸是0，可能和材料说明柠檬酸少量发现有关，也可能是数据缺失。

###Did you perform any operations on the data to tidy, adjust, or change the form of the data? If so, why did you do this?


#####我对质量、固定酸度、柠檬酸和酒精进行了分类统计，这样可以清晰看到每个数值对应的个数。同时，我还对残糖、氯化物、游离二氧化硫、二氧化硫总量、硫酸盐等几个具有长尾现象的数据进行了对数变换，通过变换，我们可以非常清晰的看到数据的分布及他们的共同特点。

# Bivariate Plots Section

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggpairs(wineQualityReds[,c(2:13)],
        lower = list(continuous = wrap("points", shape = I('.'))),
        upper = list(combo = wrap("box", outlier.shape = I('.')),
                     continuous = wrap("cor", size = 1))) +
  theme_grey(base_size = 5) +
  theme(axis.text = element_text(size = 3),
  legend.background = element_rect(fill = "white"),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_blank()
)
```


#####从数据图中可以看出，红酒质量和固定酸度，挥发性酸度，柠檬酸，硫酸盐，酒精的相关性较强，接下来将重点分析酒精质量和这些强相关性变量之间的关系，并分析理化性质之间的一些强相互关系。


#####1 分析红酒质量和这些强相关性变量之间的关系

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = fixed.acidity)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality-2, y = fixed.acidity), method="lm")
# quality - 2 is corresponding to the factor
```

#####红酒质量在567三个水平中的任一个时对应的固定酸度在6到12均有分布，平滑后呈缓慢上升状态。说明固定酸度和酒精质量相关性较弱。


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = volatile.acidity)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality - 2, y = volatile.acidity), method="lm")
# quality - 2 is corresponding to the factor
```

#####从箱线图的分布和平滑直线可以看出，挥发性酸度越高，红酒质量越低，呈现比较明显的负相关关系。


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = citric.acid)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality - 2, y = citric.acid), method="lm")
# quality - 2 is corresponding to the factor
```


#####从箱线图的分布和平滑直线可以看出，红酒质量越高，其柠檬酸集中分布在含量更高的地方，但是柠檬酸的含量高并无法保证酒精质量就一定高，柠檬酸是必要非充分因素。柠檬酸和红酒质量呈现比较明显的正相关关系。


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = sulphates)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality-2, y = sulphates), method="lm") 
# quality - 2 is corresponding to the factor
```

#####从箱线图的分布和平滑直线可以看出，红酒质量越高，其硫酸盐集中分布在含量更高的地方，但是硫酸盐的含量高并无法保证酒精质量就一定高，硫酸盐是必要非充分因素。



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = alcohol)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality-2, y = alcohol), method="lm") 
# quality - 2 is corresponding to the factor
```



#####从箱线图的分布和平滑直线可以看出，红酒质量越高，其酒精集中分布在含量更高的地方，但是酒精的含量高并无法保证酒精质量就一定高，酒精含量也是必要非充分因素。酒精含量和红酒质量呈现明显的正相关关系。

#####2 分析理化性质之间的一些强相互关系


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = citric.acid, y = fixed.acidity)) +
  geom_point() +
  geom_smooth(method="lm")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(wineQualityReds$citric.acid,wineQualityReds$fixed.acidity,
         method="pearson")
```


#####柠檬酸和固定酸度呈现明显的正相关关系。


```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = citric.acid, y = volatile.acidity)) +
  geom_point() +
  geom_smooth(method="lm")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(wineQualityReds$citric.acid,wineQualityReds$volatile.acidity,
         method="pearson")
```

#####柠檬酸和挥发酸度呈现明显的负相关关系。柠檬酸是非挥发性酸。



```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = total.sulfur.dioxide, 
                                   y = free.sulfur.dioxide)) +
  geom_point() +
  geom_smooth(method="lm")
```

```{r echo=FALSE, message=FALSE, warning=FALSE}
ggplot(data = wineQualityReds, aes(x = total.sulfur.dioxide, 
                                   y = free.sulfur.dioxide)) +
  geom_point(alpha = 0.5,position = 'jitter') +
  xlim(0, quantile(wineQualityReds$total.sulfur.dioxide,0.99)) +
  geom_smooth(method="lm")
```


```{r echo=FALSE, message=FALSE, warning=FALSE}
cor.test(wineQualityReds$total.sulfur.dioxide,
         wineQualityReds$free.sulfur.dioxide,
         method="pearson")
```
#####游离二氧化硫和总二氧化硫之间呈明显的正相关关系。

# Bivariate Analysis

### Talk about some of the relationships you observed in this part of the \
investigation. How did the feature(s) of interest vary with other features in \
the dataset?
  
我感兴趣的变量是红酒的质量。通过分析红酒质量和这些强相关性变量之间的关系，我观察到和红酒质量呈比较明显的正相关关系的理化性质有柠檬酸、硫酸盐，酒精含量，和红酒质量呈比较明显的负相关关系的理化性质有挥发性酸度。并且这些理化性质都对红酒的质量的影响都是必要非充分的，比如，红酒质量越高，其柠檬酸集中分布在含量更高的地方，但是柠檬酸的含量高并无法保证酒精质量就一定高，柠檬酸是必要非充分因素。这也说明红酒质量是多个理化性质共同作用的结果。  
  
### Did you observe any interesting relationships between the other features \
  (not the main feature(s) of interest)?

关于其他特征（不是感兴趣的主要特征）之间的关系，我也探索了分析理化性质之间的一些强相互关系，有如下的观察结果，柠檬酸和固定酸度呈现明显的正相关关系。柠檬酸和挥发酸度呈现明显的负相关关系。柠檬酸是非挥发性酸。游离二氧化硫和总二氧化硫之间呈明显的正相关关系。
  
### What was the strongest relationship you found?
  
主要特征和其他理化性质特征的关系中，我发现的最强的关系是酒精含量和红酒质量，呈最明显的正相关关系。非主要特征的关系中，我发现的最强的关系是柠檬酸和固定酸度呈现最明显的正相关关系。  


# Multivariate Plots Section


```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(data = wineQualityReds, aes(x = alcohol, y = citric.acid)) +
  geom_point(aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette= 'Reds',
                     guide = guide_legend(title="quality",reverse = TRUE))
```

#####散点图可以看出，红酒质量为6、7、8的大都位于右上角，红酒质量为3、4、5的大都位于左下方，更仔细看，两个变量对红酒质量的影响程度不同，柠檬酸一定时，随着酒精含量增加，红酒质量提高了。酒精含量在较低的范围时，提高柠檬酸含量，红酒质量不变，酒精含量在较高的范围时，提高柠檬酸含量，红酒质量提高。


```{r echo=FALSE, message=FALSE, warning=FALSE }
ggplot(data = wineQualityReds, aes(x = alcohol, y = sulphates)) +
  geom_point(aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette= 'Reds',
                     guide = guide_legend(title="quality",reverse = TRUE) )
```

#####散点图可以看出,硫酸盐一定时，随着酒精含量增加，红酒质量提高了。酒精含量在较低的范围时，提高硫酸盐含量，红酒质量不变，酒精含量在较高的范围时，提高硫酸盐含量，红酒质量提高。


```{r echo=FALSE, message=FALSE, warning=FALSE }

ggplot(data = wineQualityReds, aes(x = citric.acid, y = sulphates)) +
  geom_point(alpha = 0.5, aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette= 'Reds',
                     guide = guide_legend(title="quality",reverse = TRUE) )
```

#####散点图可以看出，柠檬酸含量一定时，随着硫酸盐含量增加，红酒质量提高了。硫酸盐含量在较低的范围时，提高柠檬酸含量，红酒质量基本不变，硫酸盐含量在较高的范围时，提高柠檬酸含量，红酒质量有所提高。

```{r echo=FALSE, message=FALSE, warning=FALSE }

ggplot(data = wineQualityReds, aes(x = alcohol, y = volatile.acidity)) +
  geom_point(alpha = 0.5, aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette= 'Reds',
                     guide = guide_legend(title="quality",reverse = TRUE) )
```

#####散点图可以看出，挥发性酸度较低时，提高酒精含量，红酒质量提高，酒精含量较高时，降低挥发性酸度，红酒质量提高。



```{r echo=FALSE, message=FALSE, warning=FALSE }

m1 <- lm(I(quality) ~ I(alcohol),data = wineQualityReds)
m2 <- update(m1,  ~ . + I(sulphates))
m3 <- update(m2,  ~ . + I(citric.acid))
m4 <- update(m3,  ~ . + I(volatile.acidity))
m5 <- update(m4,  ~ . + I(fixed.acidity))
m6 <- update(m5,  ~ . + I(chlorides))
m7 <- update(m6,  ~ . + I(total.sulfur.dioxide))

mtable(m1, m2, m3, m4, m5, m6, m7)
```

#####建立线性模型，可以看出红酒质量=2.652+0.288alcohol+0.888sulphates-0.203citric.acid-1.173volatile.acidity+0.037fixed.acidity-1.576chlorides-0.002total.sulfur.dioxide+误差


# Multivariate Analysis

### Talk about some of the relationships you observed in this part of the \
investigation. Were there features that strengthened each other in terms of \
looking at your feature(s) of interest?

酒精含量和柠檬酸，硫酸盐，三者有相互加强功能。

### Were there any interesting or surprising interactions between features?

酒精含量对红酒质量起到决定性作用，柠檬酸，硫酸盐，挥发性酸度一定时，酒精含量提高，红酒质量明显提高。酒精含量较低时，提高柠檬酸或者硫酸盐含量，或者降低挥发性酸度，红酒的质量基本不变。酒精含量较高时，提高柠檬酸或者硫酸盐含量，或者降低挥发性酸度，红酒的质量有明显提升。

### OPTIONAL: Did you create any models with your dataset? Discuss the \
strengths and limitations of your model.

我使用酒精，硫酸盐，柠檬酸，挥发性酸度，固定酸度，氯化物，总二氧化硫为红酒质量建立了线性模型，此模型的优点是，综合考虑了多方面因素对红酒质量的影响，提供了特征（变量）与结果之间关系的强度和大小的估计。如果我们知道一种红酒对应的上述的理化性质，可以用来预测该种红酒的质量。此模型的缺点是，对数据做出了很强的假设，该模型的形式必须由使用者事先指定，不能很好地处理缺失数据，易受异常数据的干扰，需要一些统计知识来理解模型。而且，我们也看到，从柠檬酸在m3的线性模型中系数为正，而增加了挥发性酸度等其他变量后柠檬酸在线性模型中的系数变为负，柠檬酸和红酒质量的关系的转变，也是和原来的分析存在一定差距的。多变量之间的相互影响也比较难从此模型中得到理解。

------

# Final Plots and Summary

### Plot One

```{r echo=FALSE, message=FALSE, warning=FALSE, Plot_One}
ggplot(data = wineQualityReds, aes(x = alcohol)) + 
  geom_histogram(binwidth = 0.08, fill = 'red', color = 'blue') +
  scale_x_continuous(breaks = seq(8, 15, 0.5)) +
  scale_y_continuous(breaks = seq(0, 200, 25)) +
  ggtitle('Count of Alcohol') +
  xlab('alcohol(% by volume)')
```

#####通过分析，红酒的酒精含量百分比绝大多种都分布在9.4%和9.5%，并且往酒精含量增加的方向红酒分布呈现下降趋势。而且酒精含量占比总是断断续续分布在一定区间，某些区间是没有的。酒精这项指标在不同品种红酒中呈现正偏态分布。


### Plot Two

```{r echo=FALSE, message=FALSE, warning=FALSE, Plot_Two}

ggplot(data = wineQualityReds,aes(x = factor(quality), 
                                  y = alcohol)) +
  geom_boxplot() +
  stat_summary(aes(shape="mean"), geom = 'point', 
               fun.y  = mean, color = 'red') +
  geom_point(alpha = 0.1, position = "jitter") +
  geom_smooth(aes(x = quality-2, y = alcohol), method="lm") +
  ggtitle('Quality by Alcohol') +
  ylab('alcohol(% by volume)') +
  xlab('quality(score between 0 and 10)')
# quality - 2 is corresponding to the factor
```

#####从箱线图的分布和平滑直线可以看出，红酒质量越高，其酒精集中分布在含量更高的地方，但是酒精的含量高并无法保证酒精质量就一定高，酒精含量也是必要非充分因素。酒精含量和红酒质量呈现明显的正相关关系。




### Plot Three

```{r echo=FALSE, message=FALSE, warning=FALSE, Plot_Three}

ggplot(data = wineQualityReds, aes(x = alcohol, y = citric.acid)) +
  geom_point(aes(color = factor(quality))) +
  scale_color_brewer(type='seq', palette= 'Reds',
                     guide = guide_legend(title="quality",reverse = TRUE)) +
  ggtitle('Quality by Citric.acid and Alcohol') +
  ylab('citric.acid(g/dm^3)') +
  xlab('alcohol(% by volume)')
```

#####散点图可以看出，红酒质量为6、7、8的大都位于右上角，红酒质量为3、4、5的大都位于左下方，更仔细看，两个变量对红酒质量的影响程度不同，柠檬酸一定时，随着酒精含量增加，红酒质量提高了。酒精含量在较低的范围时，提高柠檬酸含量，红酒质量不变，酒精含量在较高的范围时，提高柠檬酸含量，红酒质量提高。

------
  
# Reflection
  
#####红酒数据集包括12个变量，1599种红酒的信息。我通过单变量分析，双变量分析，多变量分析寻找这11个理化性质和红酒质量之间的内在联系，进而能帮助我们分析和预测红酒质量。单变量分析阶段，我分析了数据的结构，对数据集进行一些初步探索，将所有连续变量用直方图表示，分类变量用柱状图表示，并且对一些长尾数据做了对数转化来更好的分析其分布及特征。在单变量分析时，我对数据集的了解不多，只能对每一个逐个进行分析，好在图片的解释性强，我便将我分析的重点投入到非正态分布的变量中。

双变量分析阶段，我一开始就先用ggpairs绘制散点图矩阵，并针对相关系数比较大的变量进行进一步的分析，对分类变量（质量）和理化性质变量（柠檬酸、硫酸盐，酒精含量）之间进行分析时，我采用了箱线图，并在箱线图基础上叠加了设置了抖动和透明度的散点图，并绘制了平滑直线，这一步令我对探究红酒质量和理化性质变量之间的关系进展顺利，通过箱线图，我观察到和红酒质量呈比较明显的正相关关系的理化性质有柠檬酸、硫酸盐，酒精含量，和红酒质量呈比较明显的负相关关系的理化性质有挥发性酸度。除了分析酒精质量和这些强相关性变量之间的关系，我还分析了理化性质之间的一些强相互关系。柠檬酸和固定酸度呈现明显的正相关关系。柠檬酸和挥发酸度呈现明显的负相关关系。柠檬酸是非挥发性酸。游离二氧化硫和总二氧化硫之间呈明显的正相关关系。

多变量分析阶段，我将我分析的重点放在红酒质量有强相关关系的理化性质变量之间是否对红酒质量的影响有相互加强的作用。发现了酒精含量对红酒质量起到决定性作用，柠檬酸，硫酸盐，挥发性酸度一定时，酒精含量提高，红酒质量明显提高。酒精含量较低时，提高柠檬酸或者硫酸盐含量，或者降低挥发性酸度，红酒的质量基本不变。酒精含量较高时，提高柠檬酸或者硫酸盐含量，或者降低挥发性酸度，红酒的质量有明显提升。并且，我我使用酒精，硫酸盐，柠檬酸，挥发性酸度，固定酸度，氯化物，总二氧化硫为红酒质量建立了线性模型，可用于分析各预测红酒质量。

在将来的工作中，我将进一步探究理化性质变量之间是否对红酒质量的影响有相互加强的作用，我希望可以一次性对更多变量之间进行探究，比如，我将探究每个质量等级的柠檬酸，硫酸盐，挥发性酸度三者的分布，这样能更好分析理化性质对红酒质量的影响。
