#setup the public function on this file 
#*******copywrite (c) hulilei @ 2017
#0 加载相关的库文件 
library(readxl);
library(openxlsx);
library(magrittr);
library(reshape2);

#1.统一的处理func-----
#1.01  判断数据集的字段类型------
#used by 3.02
getFieldType <- function (data)
{
  #获取所有的字段列表
  nameset1 <- names(data); 
  #nameset1;
  #part 1 ：处理所有的日期字段
  # 获取所有带日期的字段如订单日期，出库日期
  typeDate_p1 <-nameset1[grep('日期',nameset1)]
  # 例外日期字段处理：Date,date,FDate,fdate
  typeDate_p2<- nameset1[nameset1 %in% c('Date','date','FDate','fdate')]
  # 合并2个向量
  typeDate <- c(typeDate_p1,typeDate_p2)
  
  #part 2  处理所有的地理信息
  #获取所有的行政单位
  typeGeo_place <- nameset1[nameset1 %in% c('省份','城市','乡镇')]
  # typeGeo_place
  
  #part 3 获取所有的经度信息
  typeGeo_lon <-  nameset1[nameset1 %in% c('经度','lon','longitude')]
  #typeGeo_lon
  #part 4 
  #获取所有的纬度信息
  typeGeo_lat <-  nameset1[nameset1 %in% c('纬度','lat','latitude')]
  #typeGeo_lat
  #合并地点，经度，经度，形成地理信息
  typeGeo <- c(typeGeo_place,typeGeo_lon,typeGeo_lat)
  
  #typeGeo
  
  typeSpecial <- c(typeDate,typeGeo)
  
  #获取除了日期、地理相关的其他字段信息
  nameset2 <- nameset1[!nameset1 %in% typeSpecial]
  #针对原始数据取一条记录进行判断
  #由于R不是显示指定数据类型，所以必须根据数据进行判定
  
  data2 <- data[1,nameset2,drop=FALSE]
  #获取第3，4（除日期，地理外）的字段信息
  type34<-lapply(data2,class)
  #将列表数据恢复为向量数据
  type34 <- unlist(type34);
  
  #type34
  typeMeasure <- names(type34[type34=='numeric'])
  #typeMeasure
  
  typeDimesion<- names(type34[type34=='character'])
  
  #typeDimesion
  #将上述数据装入列表对象
  
  fieldType <- list(
    Dimesion=typeDimesion,
    Measure=typeMeasure,
    Date = typeDate,
    Geo =typeGeo,
    Geo.Place = typeGeo_place,
    Geo.Lon =typeGeo_lon,
    Geo.Lat = typeGeo_lat
  )
  
  return (fieldType)
  # fieldType$Dimesion
  # fieldType$Measure
  # fieldType$Date
  # fieldType$Geo
  # fieldType$Geo.Place
  # fieldType$Geo.Lon
  # fieldType$Geo.Lat
  
}
# 
# #测试用例
# 
# data <-data.frame(
#   id=c(1,2,3),
#   name=c("hu","li","lei"),
#   date=c('2014-01-01','2017-04-02','2015-03-02'),
#   rev=c(12.1,132.1,142.212),
#   cost=c(1232,12322,12),
#   订单日期=c('2014-07-08','2015-01-01','2016-01-01'),
#   出库日期=c('2018-01-02','2016-03-02','2017-01-01'),
#   省份=c('aa','bb','cc'),
#   城市=c('bb','cc','dd'),
#   经度=c(121,123,122),
#   纬度=c(34,34,34),
#   lon=c(121,123,122),
#   lat=c(34,34,34), stringsAsFactors=F)
#1.02 从EXCEL中读取数据----
getDataFromExcel <- function(file,sheet=1)
{
  res <- read_excel(file,sheet)
  return(res)
};
#1.03将数据写入Excel----
writeDataToExcel <- function (data,fileName,sheetName)
{
  
  #write.xlsx(x = data,file = fileName,sheetName = sheetName,row.names = FALSE,append = T,showNA = T);
  write.xlsx(x = data,file = fileName)
  
};
#2.04A 生成字段匹配的分析码-----
rdCode <- function (x)
{
  res <- ''
  if (x <= 2)
  {
    res <- as.character(x)
  }else  {
    res <- 'N'
  }
  return(res)
};
#2.04B 生产列表分析码-----
rdMode <-function(...)
{
  dl <- list(...)
  el <-lapply(dl,rdCode)
  res <- unlist(el)
  res <-paste0(res,collapse = '')
  return(res)
};

#2.05生成相应的数据模式代码----
rdSettingGenerateor <- function()
{
  d1<-c1<-b1<-a1<-c('0','1','2','N')
  
  p <-1
  res <-character(4*4*4*4)
  for (h in seq_along(a1)){
    for (i in seq_along(b1))
    {
      for (j in seq_along(c1)){
        for (k in seq_along(d1))
        {
          res[p] = paste0(a1[h],b1[i],c1[j],d1[k],collapse = '')
          p<-p+1
        }
      }
    }
  }
  return(res)
  
}

#2.07增加新的图表类型----

rdSettingAdd <- function (data=NULL,type,Fname_cn,Fname_en=type,FValue=FALSE,FuseCount=1000)
{
  addData <- data.frame(Fmode=rdSettingGenerateor(),Ftype=type,Fvalue=FValue,
                        Fname_cn=Fname_cn,Fname_en=Fname_en,FuseCount=FuseCount,stringsAsFactors = F)
  if (is.null(data))
  {
    res <- addData
  }else{
    res<- rbind(data,addData)
  }
  return(res)
}
#2.08修改图表类型使其可用----
rdSettingModify <- function(data,Fmode,type,value=TRUE)
{
  data$Fvalue[data$Fmode == Fmode & data$Ftype ==type ] <- value
  return(data)
}
# 2.09批量按数据模式增加可用类型-----
rdSettingModifyBatch <- function(data,FBatchmode,type,value=TRUE)
{
  for (nx in FBatchmode)
  {
    data <- rdSettingModify(data,nx,type,value)
  }
  return(data)
}
#2.10读取excel配置文件，在服务器应用----

rdSettingRead <- function (file,sheet=1)
{
  openxlsx::read.xlsx(file,sheet)
}
#2.11写入excel配置文件，由后台管理员进行操作，不提供前端配置-------
rdSettingWrite <-function(data,file)
{
  openxlsx::write.xlsx(data,file = file)
}
#2.12根据数据模式获取可用图形列表-----
rdgetChartList <- function (data,Fmode)
{
  data[data$Fmode ==Fmode &  data$Fvalue == TRUE,c('Ftype','Fname_cn')]
}
#2.13根据数据模式及图形选择验收期可用性-----
rdValidation <- function(data,Fmode,type)
{
  bb <-data$Fvalue[data$Fmode ==Fmode & data$Ftype ==type]
  if (is.null(bb) | length(bb) == 0)
  {
    res<-FALSE
  }else(
    res <- bb
  )
  return(res)
}
#2.14 将字段自动换行处理------
category_changeLine <- function (data,segCount=6)
{
  ncount <- length(data)
  res <- character(ncount)
  if ( ncount <= segCount){
    res <- data
  }else{
    for (i in 1:ncount)
    {
      if (i %% 2 == 1){
        res[i] <- data[i]
      } else{
        res[i] <- paste('\n',data[i],sep = "")
      }
    }
    
  }
  return(res)
}
