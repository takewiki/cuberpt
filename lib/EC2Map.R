#setup the echart2 related map dealing  function on this file 
#*******copywrite (c) hulilei @ 2017
#0 加载相关的库文件 
nameValue <- function (data)
{
  ncount <-nrow(data)
  res <- vector(mode='list',ncount)
  for (i in 1:ncount)
  {
    res[[i]] <-list(name=data[i,1],
                    value=data[i,2])
  }
  return(res)
}

option.series.map1.getData <- function(data)
{
  
  names(data) <-c('series','name','value')
  cateData <- as.character(data[,1])
  cateData <- unique(cateData)
  cateCount <- length(cateData)
  raw <-split(data,data[,1,drop=FALSE])
  res <-vector(mode='list',length = cateCount)
  for (i in 1:cateCount)
  {
    res[[i]] <- list(
      name = cateData[i],
      type='map',
      mapType='china',
      roam=FALSE,
      itemStyle=list(
        normal=list(label=list(show=TRUE)),
        emphasis=list(label=list(show=TRUE))
      ),
      data = nameValue(raw[[i]][,c(2,3)])
    )
  }
  return(res)
} 
option.base.map1 <- function()
{
  res <- list(   title = NA,
                         tooltip = NA,
                         legend = NA,
                         dataRange = NA,
                         toolbox = NA,
                         roamController = NA,
                         series = NA)
  return(res)
  
}
option.title.map1 <- function(option,text='',subtext='powered by ReshapeData',x='center')
{
  res <- list(   text= text,
                   subtext= subtext ,
                   x= x)
  option$title <-res
  return(option)
}
option.tooltip.map1 <- function(option,trigger='item')
{
  
  res <- list(trigger= trigger)
  option$tooltip <-res
  return(option)
}

option.legend.map1 <- function(option,orient,x,data)
{
  data <- data[,1]
  data <- as.character(unique(data))
  res <- list( orient = "vertical",
                  x = "left",
                  data = data)
  option$legend <-res
  return(option)
}
option.dataRange.map1 <- function(option,data,
                                  x = "left",
                                  y = "bottom",
                                  text = c("高","低"),           # 文本，默认为数值文本
                                  calculable = TRUE)
{
  res <- list( min = min(data),
                     max = max(data),
                     x = x,
                     y = y,
                     text = text,           # 文本，默认为数值文本
                     calculable = calculable)
  option$dataRange <- res
  return(option)
}

option.toolbox.map1 <- function (option,show=T,orient = "vertical", x = "right",
                                 y = "center",mark=T,dataViewShow=T,dataReadOnly=T,
                                 restore=T,saveAsImage=T)
{
  option$toolbox <-list( show = show,
                         orient = orient,
                         x = x,
                         y = y,
                         feature = list(     mark = list(show = mark),
                                             dataView = list(show=dataViewShow, readOnly=dataReadOnly),
                                             restore = list(show = restore),
                                             saveAsImage = list(show = saveAsImage)))
  return(option)
}

option.roamController.map1 <- function(option,show=T,x='right')
{
  option$roamController <- list( show = show,
                                 x = x,
                                 mapTypeControl= list("china"= TRUE) )

  return(option)
}
option.series.map1 <- function(option,data)
{
  data_series <- option.series.map1.getData(data)
  option$series <-data_series
  return(option)
}



