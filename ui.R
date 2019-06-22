# copywrite (c) hulilei @2017,reshapedata tech shared.
# powered by shiny from rstudio
# base on R 3.4

#V1.06
#规范所有布局
#调整输入参数
#优化图表的结构与表达方式，进行解处处理
#增加对库的支持，增加lib



#V1.05 增加菜点图，调整工具栏与坐标轴的参数

#V1.04
# 增加交叉图表，
# 修改布局


# V1.03
#  针对结构进行优化，进行减函数的封装
#  针对后台增加管理GUI是个不错的选项
    
# 版本V1.02 针对功能进行完善，清除不使用代码
# 针对chart进行重新封装
# 删除工具栏下拉菜单，如果需要，请到V1.01中进行功能恢复


# 版本V1.01
# 增加如下功能
# 表格师选列
# 设置数据角色：维度、指标、日期、地理（名称、经伟度）
# 选择图表类型，给出建议
# 优化分析报表模板
#  胡立磊 20170514


library(shiny)
library(shinydashboard)
library(recharts)
library(htmlwidgets)

shinyUI(dashboardPage(skin = "blue",
                    
              
                    
                    #1.01ui.header -----
                    
                    dashboardHeader(title = "ReshapeDataValue:R U ser-ious?",titleWidth = '380px'),
                    
                    #2.01ui.sideBar----
                    dashboardSidebar(
                      sidebarMenu(
                        # add the item
                        
                        menuItem(text = "图表选项设置",tabName = "rdCostingTopic",icon = NULL)
                      ),
                        
                      fluidRow(  box(
                        title = "工具栏选项", status = "primary", width = 12,solidHeader = FALSE,
                        collapsible = TRUE,collapsed = TRUE,background = 'black',
                        #2.01.01工具栏选项--------
                        checkboxInput("chartToolBoxShow","是否显示工具栏?",value = T),
                        #2.01.01.01 input.chartToolMarkShow 是否显示辅助线？----
                        
                        conditionalPanel("input.chartToolBoxShow == true",
                        #2.01.01.02工具栏方向---------
                                         selectInput('chartToolorient',"工具栏方向",
                                                     choices = list('水平方向'='horizontal','垂直方向'='vertical'),
                                                     selected='vertical'),
                        #2.01.01.03水平位置选择------
                                         selectInput('chartTool_x','水平位置选择',
                                                     choices = list('左'='left','中'='center','右'='right'),
                                                     selected='right'),
                        #2.01.01.03 垂直位置选择-------
                                         selectInput('chartTool_y','垂直位置选择',
                                                     choices = list('上'='top','中'='center','下'='bottom'),
                                                     selected='top'),
                        #2.01.01.04 是否启用辅助线-----
                                         
                                         checkboxInput('chartToolMarkShow','是否启用辅助线?',value = T),
                        #2.01.01.05 是否显示数据缩放--------
                                         checkboxInput('chartTooldataZoom','是否显示数据缩放?',value = T),
                        #2.01.01.06  input.chartToolDataViewShow 是否显示数据视图------
                                         checkboxInput('chartToolDataViewShow','是否显示数据视图?',value = FALSE),
                        #2.01.01.07 input.chartToolMagicTypeShow 是否显示智图切换>------
                                         checkboxInput('chartToolMagicTypeShow','是否显示智图切换?',value = FALSE))
                      ))
                      ,
                      fluidRow( box(
                       #2.01.02坐标轴选项-----
                        title = "坐标轴选项", status = "primary", width = 12,solidHeader = FALSE,
                        collapsible = TRUE,collapsed = TRUE,background = 'black',
                        # 2.01.02.01是否交互X与Y轴------
                        checkboxInput("chartXYExchange","是否X与Y轴交换?",value = FALSE),
                        # 2.01.02.02 散点图单位1-----
                        textInput('MeasureUnitName1','散点图单位1'),
                        #2.01.02.03 散点图单位2------
                        textInput('MeasureUnitName2','散点图单位2')
                        
                      ))
                      # ,
                      # fluidRow( box(
                      #   title = "Histogram", status = "primary",width = 12, solidHeader = TRUE,
                      #   collapsible = TRUE,collapsed = TRUE,background = 'black',
                      #   textInput('text3','test for text',width = '100%')
                      # ))
                      # ,
                      # fluidRow( box(
                      #   title = "Histogram", status = "primary", width = 12,solidHeader = TRUE,
                      #   collapsible = TRUE,collapsed = TRUE,background = 'black',
                      #   textInput('text4','test for text',width = '100%')
                      # )
                      # )
                      
                    ),
                    
                    #3ui.body----
                    dashboardBody(
                      tabItems(
                        # First tab content
                        
                        tabItem(tabName = "rdCostingTopic",
                                tabsetPanel(
                                #3.01 数据上传-----  
                                  tabPanel("数据上传", 
                                           fluidRow(
                                             box(title = "选择数据源",width = 3, status = "primary",
                                                 radioButtons("fileType","文件类型",choices = c("Excel","CSV"),selected = "Excel"),
                                                 conditionalPanel("input.fileType =='Excel'",
                                  #3.01.01 input.fileExcel 请选择EXCEL-----
                                                                  fileInput("fileExcel", "请选择一下Excel文件.",buttonLabel = "浏览",
                                                                            accept = c(
                                                                              ".xls",
                                                                              ".xlsx")
                                                                  ),
                                  #3.01.02 input.firstUse1 首次使用EXCEL----
                                                                  
                                                                  checkboxInput("firstUse1","首次使用，请下载文件模板"),
                                                                  tags$hr(),
                                  #3.01.03 output.fileTemplate_download1 下载Excel模板----
                                                                  conditionalPanel("input.firstUse1 == true",
                                                                                   downloadButton("fileTemplate_download1","下载文件模板"),tags$hr())),
                                                 conditionalPanel("input.fileType == 'CSV'",
                                 #3.01.04 input.file1 请选择CSV文件-----
                                                                  fileInput("file1", "请选择一下CSV文件.",buttonLabel = "浏览",
                                                                            accept = c(
                                                                              "text/csv",
                                                                              "text/comma-separated-values,text/plain",
                                                                              ".csv")
                                 #3.01.05 input.firstUse2 首次使用CSV----
                                                                  ), checkboxInput("firstUse2","首次使用，请下载文件模板"),
                                                                  tags$hr(),
                                                                  conditionalPanel("input.firstUse2 == true",
                                 #3.01.06 output.fileTemplate_download2 下载CSV模板-----
                                                                                   downloadButton("fileTemplate_download2","下载文件模板"),tags$hr())),
                                                 
                                                                                                 #tags$hr(),
                                 #3.01.07 input.header 首行是否包含标题----
                                                 checkboxInput("header", "首行包含标题?", TRUE)
                                      
                                                 ),
                                 #3.01.08 output.contents 预览客户上传的数据集-----
                                             box(title = "预览数据集",width = 9, status = "primary",
                                                 dataTableOutput("contents"))
                                                                                        )
                                          
                                           
                                  ),
                                #3.02 数据角色----
                                  tabPanel("数据角色", 
                                           
                                           
                                           fluidRow(
                                             column(4,
                                                    fluidRow(
                                #3.02.01A output.selDimension选择维度占位符()-------
                                                      box(title = "选择维度:",width = 6, status = "primary",
                                                          uiOutput("selDimension")),
                                #3.02.01B input.settingDim  选择维度（服务器产生）----
                                
                                #3.02.02A output.selGeoPlace 选择地理1占位符----
                                                      
                                                      box(title = "选择地理1:",width = 6, status = "primary",
                                                          uiOutput("selGeoPlace"))
                                #3.02.02B input.settingPlace 选择地理1（服务器生成）-----
                                                    )
                                                    ,
                                                    fluidRow(
                                #3.02.03A output.selMeasure  选择指标占位符 -----
                                                      box(title = "选择指标:",width = 6, status = "primary",
                                #3.02.03B input.settingMeasure  选择指标（服务器）生成----
                                                          uiOutput("selMeasure")),
                                #3.02.04A output.selGeoLon 选择经度占位符----
                                                      box(title = "选择地理2:",width = 6, status = "primary",
                                                                                   uiOutput("selGeoLon"))
                                #3.02.04B input.settingLon 选择经度（服务器生成）----
                                                      
                                                     ),
                                                    fluidRow(
                                #3.02.05A output.selDate 选择日期占位符-----
                                                      box(title = "选择日期:",width = 6, status = "primary",
                                                          uiOutput("selDate")),
                                #3.02.05B input.settingDate 选择日期(服务器产生)-----
                                #3.02.06A output.selGeoLat 选择纬度占位符------
                                #3.02.06B input.settingLat  选择纬度（服务器生成）----
                                                      box(title = "选择地理3:",width = 6, status = "primary",
                                                          uiOutput("selGeoLat"))
                                                    )
                                                    ),
                                             column(8,
                                                    fluidRow(
                                                      box(title = "选择结果提示：",width = 6, status = "primary",
                               #3.02.07 FieldSelected 选择结果提示-----
                               #直接显示结果，可以不考虑其他输入
                                                          uiOutput("FieldSelected")),
                               #3.02.08  output.DNAShow显示分析模式码-----
                                                      box(title = "数据模式代码",width = 6, status = "primary",
                                                          verbatimTextOutput('DNAShow'))
                                                    ),
                                                    fluidRow(
                               #3.02.09 显示筛选后的数据集----
                                                      column(12,box(title = "显示数据集",width = 12, status = "primary",
                                                                    dataTableOutput("SelOutDataShow"))))
                                                    
                                  ))),
                               #3.03 数据分析页签-----
                                  tabPanel("数据分析", 
                                           fluidRow(
                                             #3.03.00 增加图表选项-------
                                             column(3,box(title = '智能图形筛选',width = 12,status = 'primary',
                                                          uiOutput('selChartType'),
                                                          tags$hr(),
                                                          #3.03.00A预览图形样式----
                                                          imageOutput("previewChartStyle",width = '100%',height = '100%'))),
                                              column(3, box(title = "图表选项",width = 12, status = "primary",
                                                              #3.03.01 input.chartTitle 设置图表标题-----
                                                              textInput(inputId="chartTitle","设置图表标题"),
                                                              #3.03.02 input.chartSubTitle设置图表副标题-----
                                                              textInput(inputId="chartSubTitle","设置图表副标题"),
                                                              #3.03.03 input.chartToolBoxShow 是否显示工具栏----- 
                                                           
                                                           
                                                            tags$hr(),
                                                            
                                                              #3.03.07 chartShow 制图按纽-------
                                                              # 通过此按纽进行控制，目前还缺少复位设置
                                                              
                                                              
                                                              actionButton("chartShow","制图",icon = icon("bar-chart")))),
                                              
                                              column(6,box(title = "图表展示",width = 12, status = "primary",
                                                           #3.03.08 图表展示区-----
                                                           #名称待修改
                                                           eChartOutput("chartMainOut",height = "450px")))
                            
                                    
                                            
                                           ))
                             #   ,
                             # #4.04 报告下载------
                             #      tabPanel("报告下载", fluidRow(
                             #        box(title = "html",width = 4, status = "primary",
                             # #4.04.01 report_html 下载HTML-----
                             #            downloadButton("report_html", "下载HTML格式分析报告")),
                             # #4.04.02 report_pdf 下载PDF格式报表 -----
                             #        box(title = "pdf",width = 4, status = "primary",
                             #            downloadButton("report_pdf", "下载PDF格式分析报告")),
                             # #4.04.03 report_word 下载word格式报表-----
                             #        box(title = "word",width = 4, status = "primary",
                             #            downloadButton("report_word", "下载Word格式分析报告"))
                             #        
                             #      ))
                                                                  )
                                )  
)
)))


