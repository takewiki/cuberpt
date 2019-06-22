#setup the echart2 related bar dealing  function on this file 
#*******copywrite (c) hulilei @ 2017
#0 加载相关的库文件 

#1.01 生成堆积图的分析数据-----
barGroup <- function(col_count,groupName=c("G1","G2","G3"))
{
  res <- switch(col_count,
                'unGrouped',
                'ungrouped',
                "ungrouped",
                c(rep(groupName[1],2),rep(groupName[2],2)),
                c(rep(groupName[1],3),rep(groupName[2],2)),
                c(rep(groupName[1],3),rep(groupName[2],3)),
                c(rep(groupName[1],3),rep(groupName[2],3),rep(groupName[3],1)),
                c(rep(groupName[1],3),rep(groupName[2],3),rep(groupName[3],2)),
                c(rep(groupName[1],3),rep(groupName[2],3),rep(groupName[3],3)))
  return(res)
  
}

#2.05 echartInfo-----

#2.05.01 EC2.baseOption 获取基本的echart选项-----
#2.05.01.01设置bar的基本设置选项----
EC2.Option.Bar <- function(trigger='axis',pointer='shadow',xyExchange=0,calculable=TRUE)
{
  res  <- list(
    title=NA,
    tooltip=list(
      trigger=trigger,
      axisPointer=list(type=pointer)
    ),
    legend=list(
      data=NA
    ),
    toolbox=NA,
    calculable=calculable,
    xAxis=NA,
    yAxis=NA,
    series=NA)
};


#2.05.01.02设置bar7的基本设置选项----
EC2.Option.Bar7 <- function(trigger='axis',pointer='shadow',xyExchange=0,calculable=TRUE)
{
  if (xyExchange == 0)
  {
    res  <- list(
      title=NA,
      tooltip=list(
        trigger=trigger,
        axisPointer=list(type=pointer),
        formatter=JS("function (params) { var tar = params[0];
                     return tar.name +  ':' + tar.value;}")
        ),
      legend=list(
        data=NA
      ),
      toolbox=NA,
      calculable=calculable,
      xAxis=NA,
      yAxis=NA,
      series=NA)
  }else{
    res  <- list(
      title=NA,
      tooltip=list(
        trigger=trigger,
        axisPointer=list(type=pointer),
        formatter=JS("function (params) { var tar = params[1];
                     return tar.name + ':' + tar.value;}")
        ),
      legend=list(
        data=NA
      ),
      toolbox=NA,
      calculable=calculable,
      xAxis=NA,
      yAxis=NA,
      series=NA) 
  }
  return(res)
  
};
#2.05.01.03设置bar11的基本设置选项----
EC2.Option.Bar11 <-function (trigger='axis',pointer='shadow',xyExchange=0,calculable=TRUE,date_Data=c('2017','2018'),playInterval=3000)
{
  base_option_bar11 <-list(timeline=NA,
                           options=NA)
  date_Data <- unique(date_Data)
  date_Data2 <-date_Data[,1]
  date_Data2 <- as.character(date_Data2)#fix2将数据框变成字符向量
  time_count <- nrow(date_Data)
  base_option_bar11$timeline <-list(
    data=date_Data2,
    autoPlay = TRUE,
    playInterval = playInterval
  )
  base_option_bar11$options <- vector(mode = 'list',time_count)
  base_option_bar11$options[[1]] <- list(
    title=NA,
    tooltip=list(
      trigger=trigger,
      axisPointer=list(type=pointer)
    ),
    legend=list(data=NA),
    toolbox =NA,
    calcuable=TRUE,
    xAxis=NA,
    yAxis=NA,
    series=NA
  )
  for (j in 2:time_count)
  {
    base_option_bar11$options[[j]] <-list(
      series=NA
    )
  }
  return(base_option_bar11)
}




# 2.05.01.04 scatter1-----
formatter_df <- function(unit_df=c('cm','kg'))
{
  res <-paste("function (params) { ",
              " if (params.value.length > 1) { ",
              "  return params.seriesName + ' :<br/>' ",
              "  + params.value[0] + '",unit_df[1], 
              ",' + params.value[1] + '",unit_df[2], "';",
              " } ",
              " else { ",
              " return params.seriesName + ' :<br/> '",
              " + params.name + ' : '",
              " + params.value + '",unit_df[2],"'",
              "}",
              "}",sep = "");
  return(res)
}


EC2.Option.Scatter1 <- function(trigger='axis',showDelay=0,unit_df=c('cm','kg'),pointer='cross')
{
  res  <- list(
    title=NA,
    tooltip=list(
      trigger=trigger,
      showDelay=showDelay,
      formatter=JS(formatter_df(unit_df = unit_df)),
      axisPointer=list(show=TRUE,
                       type=pointer,
                       lineStyle=list(
                         type='dashed',
                         width=1
                       )
      )),
    legend=list(
      data=NA
    ),
    toolbox=NA,
    xAxis=NA,
    yAxis=NA,
    series=NA)
}


#2.05.01.99设置bar的基本设置选项----

EC2.baseOption <- function (trigger='axis',pointer='shadow',showDelay=0,unit_df=c('cm','kg'),xyExchange=0,calculable=TRUE,date_Data=c('2017','2018'),playInterval=3000,chartClass='bar3')
{
  res <- switch(chartClass,
                'bar1'=EC2.Option.Bar(trigger,pointer,xyExchange,calculable),
                'bar2'=EC2.Option.Bar(trigger,pointer,xyExchange,calculable),
                'bar3'=EC2.Option.Bar(trigger,pointer,xyExchange,calculable),
                'bar5'=EC2.Option.Bar(trigger,pointer,xyExchange,calculable),
                'bar7'=EC2.Option.Bar7(trigger,pointer,xyExchange,calculable),
                'bar11' = EC2.Option.Bar11(trigger = trigger,
                                           pointer = pointer,
                                           xyExchange = xyExchange,
                                           calculable = calculable,
                                           date_Data = date_Data,
                                           playInterval = playInterval),
                'scatter1'=EC2.Option.Scatter1(trigger = trigger,
                                               showDelay = showDelay,
                                               unit_df = unit_df,
                                               pointer = 'cross'
                ))
  
  return(res)
}



#2.05.02抽象title-----
#2.05.02.01抽象出底层的bar-----

EC2.Title.Bar <- function(text,subtext,sublink='http://www.reshapedata.com')
{
  m <-list(text=text,
           subtext=subtext,
           sublink=sublink)
  return(m)
}
#2.05.02.99 EC2.option.title 在基础对象的基础上增加标题-----
EC2.option.title <-function(option,text='横向标准柱状图Bar3主标题',
                            subtext='数据来源：棱星数据',chartClass='bar3')
{
  m <- switch(chartClass,
              'bar1'=EC2.Title.Bar(text,subtext),
              'bar2'=EC2.Title.Bar(text,subtext),
              'bar3'=EC2.Title.Bar(text,subtext),
              'bar5'=EC2.Title.Bar(text,subtext),
              'bar7'=EC2.Title.Bar(text,subtext),
              'bar11'=EC2.Title.Bar(text = text,subtext = subtext),
              'scatter1'=EC2.Title.Bar(text = text,subtext = subtext))
  
  if ( chartClass == 'bar11'){
    option$options[[1]]$title <- m
  }else {
    option$title <- m
  }
  
  return(option)
  
}
#2.05.03抽象出工具栏的设置----
#2.05.03.01抽象出工具栏的设置----
EC2.Toolbox.Bar <- function(toolShow=TRUE,orient='horizontal',tool_x='right',
                            tool_y='center',markShow=TRUE,
                            dataViewShow=TRUE,dataViewReadOnly=FALSE,
                            magicTypeShow=TRUE,magicTypeOptions=c('line','bar'),
                            restoreShow=TRUE,
                            saveAsImageShow=TRUE)
{
  m <- list(
    show=toolShow,
    orient=orient,
    x=tool_x,
    y=tool_y,
    feature=list(
      mark=list(show=markShow),
      dataView=list(show=dataViewShow,readOnly=dataViewReadOnly),
      magicType=list(show=magicTypeShow,type=magicTypeOptions),
      restore=list(show=restoreShow),
      saveAsImage=list(show=saveAsImageShow)
    )
  )
  return(m)
}
EC2.Toolbox.Scatter <- function(toolShow=TRUE,markShow=TRUE,dataZoom=T,
                                dataViewShow=TRUE,dataViewReadOnly=FALSE,
                                restoreShow=TRUE,
                                saveAsImageShow=TRUE)
{
  m <- list(
    show = toolShow,
    feature = list(
      mark = list(show = markShow),
      dataZoom = list(show = dataZoom),
      dataView = list(show = dataViewShow, readOnly= dataViewReadOnly),
      restore = list(show = restoreShow),
      saveAsImage = list(show = saveAsImageShow)
    )
  )
  return(m)
}

#2.05.03.99 EC2.option.toolbox 设置工具栏-----
EC2.option.toolbox <-function(option,toolShow=TRUE,orient='horizontal',tool_x='right',
                              tool_y='center',markShow=TRUE,dataZoom=T,
                              dataViewShow=TRUE,dataViewReadOnly=FALSE,
                              magicTypeShow=TRUE,magicTypeOptions=c('line','bar'),
                              restoreShow=TRUE,
                              saveAsImageShow=TRUE,chartClass='bar3')
{
  m <- switch(chartClass,
              'bar1'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                     dataViewShow,dataViewReadOnly,
                                     magicTypeShow,magicTypeOptions,
                                     restoreShow,
                                     saveAsImageShow),
              'bar2'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                     dataViewShow,dataViewReadOnly,
                                     magicTypeShow,magicTypeOptions = c('line','bar','stack','tiled'),
                                     restoreShow,
                                     saveAsImageShow),
              'bar3'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                     dataViewShow,dataViewReadOnly,
                                     magicTypeShow,magicTypeOptions,
                                     restoreShow,
                                     saveAsImageShow),
              'bar5'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                     dataViewShow,dataViewReadOnly,
                                     magicTypeShow,magicTypeOptions,
                                     restoreShow,
                                     saveAsImageShow),
              'bar7'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                     dataViewShow,dataViewReadOnly,
                                     magicTypeShow,magicTypeOptions='bar',
                                     restoreShow,
                                     saveAsImageShow),
              'bar11'=EC2.Toolbox.Bar(toolShow,orient,tool_x,tool_y,markShow,
                                      dataViewShow,dataViewReadOnly,
                                      magicTypeShow,magicTypeOptions=c('line','bar','stack','tiled'),
                                      restoreShow,
                                      saveAsImageShow),
              'scatter1'=EC2.Toolbox.Scatter(toolShow = toolShow,
                                             markShow = markShow,
                                             dataZoom = dataZoom,
                                             dataViewShow = dataViewShow,
                                             dataViewReadOnly = dataViewReadOnly,
                                             restoreShow = restoreShow,
                                             saveAsImageShow = saveAsImageShow
                                             
              ))
  
  if ( chartClass == 'bar11'){
    option$options[[1]]$toolbox <- m
  }else {
    option$toolbox <- m
  }
  return(option)
}
#2.05.04 series data-----
#2.05.04.01 series data  bar1------

EC2.SeriesData.bar1 <- function(seriesData,type='bar')
{
  #seriesData
  colName <- names(seriesData)
  #colName
  ncount <- length(colName)
  #ncount
  res <- vector(mode = 'list',ncount)
  for ( i in  1:ncount)
  {
    res[[i]] <-list(name=colName[i],
                    type=type,
                    data=seriesData[,i],
                    markPoint=list(
                      data=list(
                        list(
                          type='max',
                          name='最大值'
                        ),
                        list(
                          type='min', name='最小值')
                      )  
                    ),
                    markLine=list(
                      data=list(
                        list(
                          type='average',
                          name='平均值'
                        )
                      )
                    ))
  }
  return(res)
}
#2.05.04.02 series data  bar2------
EC2.SeriesData.bar2 <- function(seriesData,type='bar')
{
  #seriesData
  colName <- names(seriesData)
  #colName
  ncount <- length(colName)
  group_name <- barGroup(col_count = ncount)
  #ncount
  res <- vector(mode = 'list',ncount)
  for ( i in  1:ncount)
  {
    res[[i]] <-list(name=colName[i],
                    type=type,
                    stack=group_name[i],
                    data=seriesData[,i]
    )
  } 
  return(res)
}
rd_splitData <- function (data,split_Data,splitColName,type='data.frame')
{
  split_Data <- unique(split_Data)
  ncount <- nrow(split_Data)
  res <- vector(mode="list",ncount)
  raw <-split(data,data[,splitColName,drop = FALSE])
  col1 <- names(data)
  col_series <-!col1 %in% splitColName
  
  if (type == 'data.frame')
  {
    for (i in 1:ncount)
    {
      res[[i]] <-raw[[i]][,col_series,drop=FALSE]
    }
  } 
  if (type == 'matrix')
  {
    for (i in 1:ncount)
    {
      res[[i]] <-as.matrix(raw[[i]][,col_series,drop=FALSE])
    }
  }
  
  
  return(res)
}


#2.05.04.03 series data  bar3------
EC2.SeriesData.bar3 <- function(seriesData,type='bar')
{
  #seriesData
  colName <- names(seriesData)
  #colName
  ncount <- length(colName)
  #ncount
  res <- vector(mode = 'list',ncount)
  for ( i in  1:ncount)
  {
    res[[i]] <-list(name=colName[i],
                    type=type,
                    data=seriesData[,i]
    )
  } 
  return(res)
}
#处理中间数据，如果是正数，则调整为负数
# 做上述处理为了配合图表显示
EC2.Bar5.Reshape <- function(data)
{
  data <- data[,1:3]
  if (sum(data[,1])+sum(data[,2]) == sum(data[,3])){
    res <- data
  }else if (sum(data[,1])-sum(data[,2]) == sum(data[,3])){
    data[,2] = -data[,2]
    res <- data
  }else{
    res <- NA
  }
  return(res)
}

#2.05.04.04 series data  bar5------
EC2.SeriesData.bar5 <- function(seriesData,type='bar')
{
  #seriesData
  seriesData <- EC2.Bar5.Reshape(seriesData)
  colName <- names(seriesData)
  #colName
  #ncount <- length(colName)
  #ncount
  res <- vector(mode = 'list',3)
  res[[1]] <-list(name=colName[1],
                  type=type,
                  stack='G1',
                  barWidth=5,
                  itemStyle=list(normal=list(label=list(show=TRUE))),
                  data=seriesData[,1])
  
  res[[2]] <-list(name=colName[2],
                  type=type,
                  stack='G1',
                  itemStyle=list(normal=list(label=list(show=TRUE,position='left'))),
                  data=seriesData[,2])
  
  res[[3]] <-list(name=colName[3],
                  type=type,
                  itemStyle=list(normal=list(label=list(show=TRUE,position='inside'))),
                  data=seriesData[,3])
  
  return(res)
}
EC2.Bar7.checkSum <- function(data)
{
  
  ncount <- length(data)
  if (ncount <=2)
  {
    res <- data
  } else if (ncount >2){
    d1 <-data[1]
    d2 <- sum(data[2:ncount])
    if (d1==d2){
      res <- data
    }else{
      d3 <- sum(data)
      res <-c(d3,data)
    }}
  return(res)
  
}
EC2.Bar7.auxData <- function(data)
{
  ncount <-length(data)
  auxdata <-vector(mode='numeric',ncount)
  
  auxdata[ncount] <-0
  for ( i in 2:ncount-1)
  {
    auxdata[i] <- data[1] -sum(data[2:i])
  }
  auxdata[1] <-0
  return(auxdata)
}
#2.05.04.03 series data  bar7------
EC2.SeriesData.bar7 <- function(seriesData,type='bar')
{
  seriesData <- as.numeric(seriesData[,1])
  seriesData <- EC2.Bar7.checkSum(seriesData)
  auxData <- EC2.Bar7.auxData(seriesData)
  res<-list(
    list(name='auxData',
         type=type,
         stack='G1',
         itemStyle=list(
           normal=list(
             barBorderColor='rgba(0,0,0,0)',
             color='rgba(0,0,0,0)'
           ),
           emphasis=list(
             barBorderColor='rgba(0,0,0,0)',
             color='rgba(0,0,0,0)'
           )
         ),
         data=auxData),
    list(name='seriesData',
         type=type,
         stack='G1',
         itemStyle=list(
           normal=list(
             label=list(
               show=TRUE,
               position='inside'
             )
           )
         ),
         data=seriesData
    ))
  return(res)
  
}


EC2.timeline.eachSeries <- function(Data,type='bar')
{
  #seriesData
  colName <- names(Data)
  #colName
  ncount <- length(colName)
  #ncount
  res <- vector(mode = 'list',ncount)
  for ( i in  1:ncount)
  {
    res[[i]] <-list(name=colName[i],
                    type=type,
                    data=Data[,i]
    )
  } 
  return(res)
}
EC2.timeline.getSeries <- function(SeriesTime_Data,date_Data,date_ColName)
{
  date_Data <- unique(date_Data)
  ncount <- nrow(date_Data)
  res <- vector(mode="list",ncount)
  raw <-split(SeriesTime_Data,SeriesTime_Data[,date_ColName,drop = FALSE])
  col1 <- names(SeriesTime_Data)
  col_series <-!col1 %in% date_ColName
  
  for (i in 1:ncount)
  {
    res[[i]] <-raw[[i]][,col_series,drop=FALSE]
  }
  return(res)
}
EC2.SeriesData.bar11 <- function (option,
                                  type='bar',
                                  seriesTimeData,
                                  date_Data,
                                  date_ColName)
{
  date_Data <- unique(date_Data)
  timeline_count <-nrow(date_Data)
  for ( i in 1:timeline_count){
    option$options[[i]]$series <- EC2.timeline.eachSeries(EC2.timeline.getSeries(seriesTimeData,date_Data,date_ColName = date_ColName)[[i]])
  }
  return(option)
}


# 设置系列菜点图数据scatter1------
EC2.SeriesData.Scatter1 <- function(SeriesCategoryData,dim_data,dim_ColName,type='scatter')
{
  raw <- rd_splitData(data = SeriesCategoryData,
                      split_Data = dim_data,
                      splitColName = dim_ColName,
                      type = 'matrix')
  dim_data <- unique(dim_data[,1])
  cateData <- dim_data
  ncount <- length(raw)
  res <- list(mode='list',ncount)
  for ( i in 1:ncount)
  {
    res[[i]] <- list(name = cateData[i],
                     type=type,
                     data=raw[[i]],
                     markPoint = list(
                       data = list(
                         list(type = 'max', name = '最大值'),
                         list(type = 'min', name = '最小值')
                       )
                     ),
                     markLine = list(
                       data = list(
                         list(type = 'average', name= '平均值')
                       )
                     )
                     
    )
  }
  return(res)
  
}


#2.05.04.99 设置系列数据-----
EC2.option.series <- function(option,seriesData,seriesTimeData,date_Data,date_ColName='日期',type='bar',chartClass='bar3')
{
  
  if (chartClass == 'bar11')
  {
    option<-EC2.SeriesData.bar11(option = option,
                                 type = type,
                                 seriesTimeData = seriesTimeData,
                                 date_Data = date_Data,
                                 date_ColName = date_ColName)
    res6 <-as.list(names(seriesData))
    res6 <- unlist(res6) #解除列表的影响
    option$options[[1]]$legend$data <- res6
  }else{
    res <- switch(chartClass,
                  'bar1'=EC2.SeriesData.bar1(seriesData,type),
                  'bar2'=EC2.SeriesData.bar2(seriesData,type),
                  'bar3'=EC2.SeriesData.bar3(seriesData,type),
                  'bar5'=EC2.SeriesData.bar5(seriesData,type),
                  'bar7'=EC2.SeriesData.bar7(seriesData,type),
                  'scatter1'=EC2.SeriesData.Scatter1(SeriesCategoryData = seriesTimeData,dim_data = date_Data,dim_ColName = date_ColName,
                                                     type = type
                  ))
    option$series <- res
    colName <- names(seriesData)
    res2 <-switch(chartClass,
                  'bar1'=as.list(colName),
                  'bar2'=as.list(colName),
                  'bar3'=as.list(colName),
                  'bar5'=as.list(colName[1:3]),
                  'bar7'=vector(mode = "list"))
    option$legend$data <- res2
  }
  
  return(option)
  
}

#2.05.05抽象底层的分类数据-----
#2.05.05.01抽象底层的分类数据-----
EC2.Category.bar <- function(categoryData,splitLine=FALSE)
{
  data <- categoryData[,1] 
  res <- list(
    list(type='category',
         splitLine=list(show=splitLine),
         data=data)
  )
  return(res)
}
EC2.Category.bar5 <- function(categoryData,axisTick=FALSE)
{
  data <- categoryData[,1] 
  res <- list(
    list(type='category',
         axisTick=axisTick,
         data=data)
  )
  return(res)
}
#2.05.05.99 设置分类数据-----
EC2.option.category <- function (categoryData,splitLine=FALSE,axisTick=FALSE,chartClass='bar3')
{
  res <- switch(chartClass,
                'bar1'=EC2.Category.bar(categoryData,splitLine=FALSE),
                'bar2'=EC2.Category.bar(categoryData,splitLine=FALSE),
                'bar3'=EC2.Category.bar(categoryData,splitLine=FALSE),
                'bar5'=EC2.Category.bar5(categoryData,axisTick = axisTick),
                'bar7'=EC2.Category.bar(categoryData,splitLine = FALSE),
                'bar11'=EC2.Category.bar(categoryData,splitLine=FALSE))
  
  return(res)
}
#2.05.06抽象底层的数值数据-----
#2.05.06.01抽象底层的数值数据-----
EC2.Value.bar <-function(boundaryGap=c(0,0.01))
{
  list(
    list(type='value', boundaryGap=boundaryGap)
  )
}
#2.05.06.99 设置数值数据-----
EC2.option.value <- function(boundaryGap=c(0,0.01),chartClass='bar3')
{
  res <- switch(chartClass,
                'bar1'=EC2.Value.bar(boundaryGap),
                'bar2'=EC2.Value.bar(boundaryGap),
                'bar3'=EC2.Value.bar(boundaryGap),
                'bar5'=EC2.Value.bar(boundaryGap),
                'bar7'=EC2.Value.bar(boundaryGap),
                'bar11'=EC2.Value.bar(boundaryGap))
  return(res)
  
}

rd_scatter <- function(unit_df)
{
  
  list(
    list(
      type = 'value',
      scale = T,
      axisLabel = list(
        formatter=  paste("{value} ",unit_df[1],sep="")
      )
    )
  )
}

#2.05.07 设置x,y轴数据-----

EC2.option.layxy<- function(option,x,y,categoryOnX = TRUE,chartClass='bar3')
{
  if (chartClass == 'bar11') {
    if (categoryOnX == TRUE)
    {
      option$options[[1]]$xAxis <-x
      option$options[[1]]$yAxis <-y
    }else{
      option$options[[1]]$xAxis <-y
      option$options[[1]]$yAxis <-x
    }
  }
  else{
    if (categoryOnX == TRUE)
    {
      option$xAxis <-x
      option$yAxis <-y
    }else{
      option$xAxis <-y
      option$yAxis <-x
    }
  }
  
  
  return(option)
}
