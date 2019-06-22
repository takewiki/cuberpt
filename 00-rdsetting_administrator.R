
#1.01生成相应的数据模式代码----
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
#增加新的图表类型----

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
#修改图表类型使其可用----
rdSettingModify <- function(data,Fmode,type,value=TRUE)
{
  data$Fvalue[data$Fmode == Fmode & data$Ftype ==type ] <- value
  return(data)
}
# 批量按数据模式增加可用类型-----
rdSettingModifyBatch <- function(data,FBatchmode,type,value=TRUE)
{
  for (nx in FBatchmode)
  {
    data <- rdSettingModify(data,nx,type,value)
  }
  return(data)
}
#读取excel配置文件，在服务器应用----

rdSettingRead <- function (file,sheet=1)
{
  openxlsx::read.xlsx(file,sheet)
}
#写入excel配置文件，由后台管理员进行操作，不提供前端配置-------
rdSettingWrite <-function(data,file)
{
  openxlsx::write.xlsx(data,file = file)
}
#根据数据模式获取可用图形列表-----
rdgetChartList <- function (data,Fmode)
{
  data$Ftype[data$Fmode ==Fmode &  data$Fvalue == TRUE]
}
#根据数据模式及图形选择验收期可用性-----
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


rdsetting <- rdSettingAdd(type='bar1',Fname_cn = '标准柱状图(标线标点)',Fname_en = 'Bar chart with markline & markpoints')

rdsetting <- rdSettingAdd(rdsetting,type='bar3',Fname_cn = '标准条形图（XY坐标轴互换）',Fname_en = 'Bar chart with X axis and Y axis exchanged')


rdsetting <-rdSettingModify(rdsetting,'1100','bar3',TRUE)
rdsetting <-rdSettingModify(rdsetting,'1200','bar3',TRUE)
rdsetting <-rdSettingModify(rdsetting,'1N00','bar3',TRUE)

 rdSettingWrite(data = rdsetting,file = 'rdsetting.xlsx')
#
#
rdsetting <- rdSettingRead(file = 'rdsetting.xlsx',sheet = 1)
 rdsetting <-rdSettingModify(rdsetting,'1100','bar1',TRUE)
 rdsetting <-rdSettingModify(rdsetting,'1200','bar1',TRUE)
 rdsetting <-rdSettingModify(rdsetting,'1N00','bar1',TRUE)


rdValidation(rdsetting2,'1N01','bar3')


