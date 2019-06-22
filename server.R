#  0.01 加载库文件------- 
# DEAL WITH DATA STEP BY STEP
#   source  --->render()    :runtime:
#   source  --->reactive()-->render()       :runtime: one data multi componet
#   source  --->reactive()    -->reactive()---->reactive-----observerEvent  (enent+Render)
#                                              :precalculate   calc at once
#   reactive---->reactive()----->eventReactive()-----render()
#                                               :calcalted step by step:

library(recharts)
library(shiny)
library(shinydashboard)
library(readxl)
library(openxlsx);
library(magrittr)



#1处理函数------
# all placed in the 'lib' directory.
#1.01 通用的处理函数-----

source(file='lib/comRDV.R',encoding = 'utf-8');

#1.02  bar related function-------


source(file='lib/EC2Bar.R',encoding = 'utf-8');


source(file='lib/EC2Map.R',encoding = 'utf-8');





#2 加载数据集-------

#2.01 模板示例数据-----

#data_template
data_template <-readxl::read_excel('test/test_bar7.xlsx');

#2.02 加载图形选项数据集----
rdsetting <- rdSettingRead(file = 'rdsetting.xlsx',sheet = 1)



#3 shiny服务器处理脚本-----

shinyServer(function(input, output,session) {
  
  #3.00 reactive arear------
  
  
  # 3.00.01 upload_data()读取上传数据到缓冲区-----
  upload_data <- reactive({
    if (input$fileType =='Excel')
    {
      inFile <- input$fileExcel
      if (is.null(inFile))
        return(NULL)
      res <-openxlsx::read.xlsx(inFile$datapath,sheet=1,colNames=input$header)
    }
    if (input$fileType == 'CSV')
    {
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      res <-read.csv(inFile$datapath, header = input$header)    
    }
    res
    
  })
  #针对数据进行封装--------
  data.upload <- reactive({
    bb <- upload_data()
  })
  
  
  
  #3.00.02获取相关字段选择信息-----
  fieldSelInfo <- reactive({
    bb <-upload_data()
    res<-getFieldType(bb)
  })
  #3.00.03 获取选择后的数据集信息------
  seldata_rea <- reactive({
    selColSet <- c(input$settingDim,input$settingMeasure,input$settingDate,input$settingPlace,input$settingLon,input$settingLat)
    bb <-upload_data()
    cc <- bb[,selColSet,drop=FALSE]
    
  })
  #已经选择的数据------
  data.selected <- reactive({
    bb <-seldata_rea()
  })
  #3.00.04 获取选择后的数据集信息统计信息----
  seldata_stat <- reactive({
    dim_Count <-length(input$settingDim)
    dim_Info <- paste0(input$settingDim,collapse = ',')
    Measure_Count <-length(input$settingMeasure)
    Measure_Info <-paste0(input$settingMeasure,collapse = ',')
    Date_Count <- length(input$settingDate)
    Date_Info <-paste0(input$settingDate,collapse = ',')
    Geo.Place_Count <- length(input$settingPlace)
    Geo.Place_Info <- paste0(input$settingPlace,collapse = ',')
    Geo.LonLat_Count <- min(length(input$settingLon),length(input$settingLat))
    Geo.LonLat_Info <- paste(input$settingLon[1:Geo.LonLat_Count],input$settingLat[1:Geo.LonLat_Count],sep = '-',collapse = ',')
    Geo_Count <- Geo.Place_Count+Geo.LonLat_Count
    Geo_Info <-paste(Geo.Place_Info,Geo.LonLat_Info,sep = ',')
    stat <- list(
      dim=list(count=dim_Count,info=dim_Info),
      measure=list(count=Measure_Count,info=Measure_Info),
      date=list(count=Date_Count,info=Date_Info),
      geo=list(
        count=Geo_Count,
        info=Geo_Info,
        place=list(count=Geo.Place_Count,info=Geo.Place_Info),
        lonlat=list(count=Geo.LonLat_Count,info=Geo.LonLat_Info)
      )
    )
    
  })
  
  
  #********数据获取***********--------------
  #  dim_Data()
  #  measure_Data()
  #  date_Data()
  #  geo_Data()$place
  #  geo_Data()$lonlat
  #  SeriesTime_Data()
  

  
  
  #3.00.05 生成分析模式代码-----
  seldata_DNA <- reactive({
    bb <- seldata_stat()
    DNA <- rdMode(bb$dim$count,bb$measure$count,bb$date$count,bb$geo$count)
    
  })
  
  #3.00.06 生成维度数据-----

  dim_Data <- reactive({
    
    bb <-seldata_rea()
    data <- bb[,input$settingDim,drop=FALSE]
  })
  map_data <- reactive({
    bb <-seldata_rea()
    data <- bb[,c(input$settingDim,input$settingPlace,input$settingMeasure),drop=FALSE]
  })
  #分析维度字段列名------
  dim_ColName <-reactive({
    res <-as.character(input$settingDim)
  })
  
  #3.00.07 生成指标数据-----
  measure_Data <-reactive({
    bb <-seldata_rea()
    data <- bb[,input$settingMeasure,drop=FALSE]
  })
  #指标字段列名-------
  measure_ColName <- reactive({
    res <- as.character(input$settingMeasure)
  })
  
  #3.00.08 生成日期数据,针对原始数据-----
 
  date_Data <-reactive({
    bb <-seldata_rea()
    data <- bb[,input$settingDate,drop=FALSE]
  })
  #日期字段列名-----
  date_ColName <- reactive({
    res <- as.character(input$settingDate)
  })
  #3.00.09 生成地理数据-----
  geo_Data <-reactive({
    bb <- seldata_rea()
    
    list(
      place=bb[,input$settingPlace[1],drop=FALSE],
      lonlat=bb[,c(input$settingLon[1],input$settingLat[1])]
    )
  })
  #地理字段列号------
  geo.place_ColName <- reactive({
    res <- as.character(input$settingPlace)
  })
  #经度列名------
  geo.lon_ColName <- reactive({
    res <- as.character(input$settingLon)
  })
  #纬度列名-------
  geo.lat_ColName <- reactive({
    res <- as.character(input$settingLat)
  })
  #3.00.10生成日期切片数据
  SeriesTime_Data <- reactive({
    bb <- seldata_rea()
    res <- bb[,c(input$settingDate,input$settingMeasure),drop=FALSE]
  })
  #3.00.11用于节点图的切片数据-------
  SeriesDim_Data <- reactive({
    bb <-seldata_rea()
    res <-bb[,c(input$settingDim,input$settingMeasure),drop=FALSE]
  })
  #
  unit_df <- reactive({
    res <- c(input$MeasureUnitName1,input$MeasureUnitName2)
  })
  
  #3.00.12 用于获取可用的图表类型-----
  rdChartOptions <- reactive({
    pp<- rdgetChartList(data = rdsetting,seldata_DNA())
    
  
  })
  #3.00.16--------
  
  
  
  #分析维度数据------
  param.dim.data <- reactive({
    dim_Data()
  })
  #分析维度名称--------
  param.dim.colName <-reactive({
    dim_ColName()
  })
  #分析指标数据-------
  param.measure.data <-reactive({
    measure_Data()
  })
  param.measure.colName <- reactive({
    measure_ColName()
  })
  param.date.data <- reactive({
    date_Data()
  })
  
  param.date.colName <- reactive({
    date_ColName
  })
  param.geo.place.data <- reactive({
    geo_Data()$place
  })
  param.geo.place.colName <- reactive({
    geo.place_ColName()
  })
  param.geo.lon.data <- reactive({
    geo_Data()$lon
  })
  param.geo.lon.colName <- reactive({
    geo.lon_ColName()
  })
  param.geo.lat.data <- reactive({
    geo_Data()$lat
  })
  param.geo.lat.colName <- reactive({
    geo.lat_ColName()
  })
  #文件类型----
  param.fileType <- reactive({
    bb<- input$fileType
  })
  
  #针对首次使用，相应的数据不再做相应的封装
  # param.firstUse <- reactive({
  #   
  # })
  param.option.title.text <- reactive({
    input$chartTitle
  })
  param.option.title.subtext <- reactive({
    input$chartSubTitle
  })
  param.option.toolbox.show <- reactive({
    bb <-input$chartToolBoxShow
  })
  param.option.toolbox.orient <- reactive({
    bb <- input$chartToolorient
  })
  param.option.toolbox.layout.xPosition <-reactive({
    bb <- input$chartTool_x
  })
  param.option.toolbox.layout.yPosition <- reactive({
    bb <-input$chartTool_y
  })
  param.option.toolbox.markLine.show <- reactive({
    bb <- input$chartToolMarkShow
  })
  param.option.toolbox.dataZoom.show <- reactive({
    bb <-input$chartTooldataZoom
  })
  param.option.toolbox.dataView.show <- reactive({
    bb <- input$chartToolDataViewShow
  })
  param.option.toolbox.magicType.show <- reactive({
    bb <- input$chartToolMagicTypeShow
  })
  param.option.Axis.exchangeXY <- reactive({
    bb <- input$chartXYExchange
  })
  
  

  
  
 # 3.01 output$var--------- 
  
  
  #3.01.03 output.fileTemplate_download1 下载Excel模板----
  output$fileTemplate_download1 <- downloadHandler(filename = function() { 'dataTemplate.xlsx' },
                                                   content = function(file) {
                                                     options("openxlsx.borderColour" = "#4F80BD") 
                                                     write.xlsx(data_template, file,colNames = TRUE, borders = "columns")
                                                   })
  
  #3.01.06 output.fileTemplate_download2 下载CSV模板-----
  output$fileTemplate_download2 <- downloadHandler(filename = function() { 'dataTemplate.csv' },
                                                   content = function(file) {
                                                     write.csv(data_template, file,row.names = FALSE)
                                                   })
  
  #3.01.08 output.contents 预览客户上传的数据集-----
  
  output$contents <- renderDataTable({
    
    upload_data()
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(5, 15,30,50,75,100), 
                   pageLength = 5))
  
  #3.01.09 output.selDimension选择维度占位符()-------
  
  output$selDimension <-renderUI({
    dimInfo <- fieldSelInfo()$Dimesion
    checkboxGroupInput("settingDim", "选择维度", dimInfo)
  })
  #3.01.10 output.selGeoPlace 选择地理1占位符----
  
  output$selGeoPlace <-renderUI({
    PlaceInfo <- fieldSelInfo()$Geo.Place
    checkboxGroupInput("settingPlace", "选择地名", PlaceInfo)
    
  })
  #3.01.11 output.selMeasure  选择指标占位符 -----
  
  output$selMeasure <-renderUI({
    MeasureInfo <- fieldSelInfo()$Measure
    checkboxGroupInput("settingMeasure", "选择指标", MeasureInfo)
  })
  
  
  #3.01.12 output.selGeoLon 选择经度占位符----
  output$selGeoLon <-renderUI({
    
    LonInfo <-  fieldSelInfo()$Geo.Lon
    checkboxGroupInput("settingLon", "选择经度",LonInfo)
    
  })
  #3.01.13 output.selDate 选择日期占位符-----
  output$selDate <-renderUI({
    DateInfo <- fieldSelInfo()$Date
    checkboxGroupInput("settingDate", "选择日期", DateInfo)
  })
  #3.01.14 output.selGeoLat 选择纬度占位符------
  output$selGeoLat <-renderUI({
    
    LatInfo <-  fieldSelInfo()$Geo.Lat
    checkboxGroupInput("settingLat", "选择纬度",LatInfo)
  })
  
  
  
  #3.01.15 FieldSelected 选择结果提示-----
  output$FieldSelected <-renderUI({
    fluidRow(
      paste0('      维度',seldata_stat()$dim$count,'个：',seldata_stat()$dim$info),
      tags$br(),
      paste0('      指标',seldata_stat()$measure$count,'个：',seldata_stat()$measure$info),
      tags$br(),
      paste0('      日期',seldata_stat()$date$count,'个：',seldata_stat()$date$info),
      tags$br(),
      paste0('      地理',seldata_stat()$geo$count,'个：',seldata_stat()$geo$info))
  })
  #3.01.16  output.DNAShow显示分析模式码-----
  output$DNAShow <- renderText({
    input$setChartType
  })
  
  
  
  
  #3.01.17 显示筛选后的数据集----
  output$SelOutDataShow<-renderDataTable({
    seldata_rea()
  },options = list(orderClasses = TRUE,
                   lengthMenu = c(5, 15,30,50,75,100), 
                   pageLength = 5))
  
  

  
  
  


  
  #3.01.18 step1 prepare data------
  output$selChartType <- renderUI({
    radioButtons(inputId = 'setChartType',
                 label = '选择合适的图表类型:',
                 choiceNames = rdChartOptions()$Fname_cn,
                 choiceValues =rdChartOptions()$Ftype )
  })
  
  #3.01.18预览图形
  chartStylePicName <- reactive({
    file <-paste('www/',input$setChartType,'.png',sep='')
  })
  
  output$previewChartStyle <-renderImage({
    list(src=chartStylePicName(),alt='没有预览图片，后续更新！',width='100%',height='100%')
    
  },deleteFile = F)
  

  
  #针对图表数据进行准备
  
  option.map1 <- reactive({
    option.base.map1() %>%option.title.map1(text=param.option.title.text(),
                        subtext = param.option.title.subtext()) %>%
      option.tooltip.map1() %>%
      option.toolbox.map1(show = param.option.toolbox.show(),
                          orient =param.option.toolbox.orient(),
                          x = param.option.toolbox.layout.xPosition(),
                          y=param.option.toolbox.layout.yPosition(),
                          mark =param.option.toolbox.markLine.show(),
                          dataViewShow =param.option.toolbox.dataView.show(),
                          dataReadOnly = F,
                          restore = T,saveAsImage = T) %>%
      option.legend.map1(data=param.dim.data())   %>%
      option.dataRange.map1(data=param.measure.data()[,1]) %>%
      option.roamController.map1() %>% option.series.map1(map_data())
    
  })
  chartSelector <-eventReactive(input$chartShow,{
    if ( input$setChartType =='map1')
    {bar3_option <- option.map1()}
    else{
      
      category_Data <-dim_Data()
      category_Data <- unique(category_Data)
      bar3_cate <- EC2.option.category(category_Data,chartClass = input$setChartType)
      bar3_value <- EC2.option.value(chartClass = input$setChartType)
      
      if (input$setChartType == 'scatter1')
      {
        bar3_cate <-rd_scatter(unit_df()[1])
        bar3_value <-rd_scatter(unit_df()[2])
      }
      
      
      if (input$chartXYExchange == FALSE)
      {
        bar3_option <-EC2.baseOption(chartClass = input$setChartType,xyExchange = 0,date_Data = date_Data(),playInterval = 3000,unit_df = unit_df(),showDelay = 0) 
      }else(
        bar3_option <-EC2.baseOption(chartClass = input$setChartType,xyExchange = 1,date_Data = date_Data(),playInterval = 3000,unit_df = unit_df(),showDelay = 0) 
      )
      
      SeriessliceData_selector <- switch(input$setChartType,
                                         'bar11'=SeriesTime_Data(),
                                         'scatter1'=SeriesDim_Data())
      splitData_selector <- switch(input$setChartType,
                                   'bar11'=date_Data(),
                                   'scatter1'=dim_Data())
      colName_selector <-switch(input$setChartType,
                                'bar11'=date_ColName(),
                                'scatter1'=dim_ColName())
      bar3_option%>%
        EC2.option.title(text=input$chartTitle,
                         subtext = input$chartSubTitle,chartClass =input$setChartType) %>%
        EC2.option.toolbox(toolShow = input$chartToolBoxShow,
                           orient = input$chartToolorient,
                           tool_x = input$chartTool_x,
                           tool_y = input$chartTool_y,dataZoom = input$chartTooldataZoom,
                           markShow = input$chartToolMarkShow,
                           dataViewShow = input$chartToolDataViewShow,
                           magicTypeShow = input$chartToolMagicTypeShow,
                           chartClass = input$setChartType) %>%
        EC2.option.series(seriesData = measure_Data(),
                          seriesTimeData = SeriessliceData_selector,
                          date_Data = splitData_selector,
                          date_ColName=colName_selector,
                          chartClass = input$setChartType) %>%
        EC2.option.layxy(y = bar3_cate,x = bar3_value,
                         categoryOnX =input$chartXYExchange,
                         chartClass = input$setChartType)
      
    }
    
    
  })
  
  
  #3.01.19 图表展示区-----
  output$chartMainOut <- renderEChart({
 
      option <-chartSelector()
   
    
       echart(option)
   
    
  })
  
  
  
  
  
  #3.01.20 report_html 下载HTML-----
  output$report_html <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_html.Rmd")
      #tempReport <- file.path("/srv/shiny-server/demo/demo112", "www/report_html.Rmd")
      #tempReport
      
      file.copy("report_html.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data1 = chartData()$a,data2 = chartData()$b,chartTitle = input$chartTitle)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #3.01.21 report_pdf 下载PDF格式报表 -----
  output$report_pdf <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_pdf.Rmd")
      #tempReport <- file.path("/srv/shiny-server/demo/demo112", "www/report_html.Rmd")
      #tempReport
      
      file.copy("report_pdf.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data1 = chartData()$a,data2 = chartData()$b,chartTitle = input$chartTitle)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  #3.01.22 report_word 下载word格式报表-----
  output$report_word <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_word.Rmd")
      #tempReport <- file.path("/srv/shiny-server/demo/demo112", "www/report_html.Rmd")
      #tempReport
      
      file.copy("report_word.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(data1 = chartData()$a,data2 = chartData()$b,chartTitle = input$chartTitle)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}
)
