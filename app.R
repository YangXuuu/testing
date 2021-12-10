#==========import library==========
suppressPackageStartupMessages(library(pheatmap))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))


# functions #
#### dealname : duplicated ids , special characters, limit length to 70 ####
dealname <- function(name, data, checknames=T){
    data1 <- as.data.frame(data)
    #### 不规范输入 ####
    if( dim(data1)[1] < 1 ){
        stop("数据少于2行,未提取到需要的数据")
    }else if( dim(data1)[2] < 2 ){
        stop("数据少于2列,未提取到需要的数据")
    }
    # 替换空字符
    data1[data1==''] <- NA
    #### 检查行名 ####
    if( !is.character(data1[,1]) ){
        stop(paste0(name," 第一列输入非字符串,请根据分析工具页面上的使用说明对数据文件进行修改"))
    }
    # 删除空列和空行
    data1 <- as.data.frame(data1[,colSums(is.na(data1)) != nrow(data1)])
    data1 <- as.data.frame(data1[rowSums(is.na(data1)) != ncol(data1),])
    # 检查只有表头没有数据的文件格式
    if( dim(data1)[2] == 0 ){
        stop(paste0(name," 数据为空,请根据分析工具页面上的使用说明对数据文件进行修改"))
    }
    # 删除"MARC1","MARC2","MARCH1","MARCH2" ,抛出报错信息
    if("TRUE" %in% (data1[,1] %in% c("43891","43892"))){
        data1 <- data1[!data1[,1] %in% c("43891","43892"),]
        sink("oeweb_task.log", append=TRUE, split=FALSE)
        print("输入数据中,MARC1,MARC2,MARCH1,MARCH2被Excel自动转换为日期或数字,分析时无法替换,默认删除此类数据")
        sink()
    }
    # 去重
    data1 <- data1[!duplicated(data1),]
    #### 输入文件及分组及注释 ####
    if(checknames){
        #### 检查行名 ####
        if( !is.numeric(data1[,2]) ){
            stop(paste0(name," 第二列非数据型,请检查"))
        }
        if(typeof(data1[,1]) == "double"){
            sink("oeweb_task.log", append=TRUE, split=FALSE)
            print("输入数据中,行名为数值型,请注意！")
            sink()
        }
        names(data1)[1] <- "ids"
        #### 过滤数据 #### 
        j = 1
        for(i in 2:length(data1))
        {   
            if( is.numeric(data1[,i]) ){j = j + 1}else break
        }
        data1 <- data1[,1:j]
        if( j < 3 ){
            stop(paste0(name,"数据少于3列,未提取到需要的数据"))
        }
        if ("TRUE" %in% is.na(data1) | "TRUE" %in% is.na(names(data1)) ) { stop(paste0(name,"数据中存在空值,请检查数据的完整性")) }
        if(any(duplicated(data1[,1]))){
            len <- length(colnames(data1))
            # 取重复
            data1 <- data1[!duplicated(data1[,1:len]),]
            dupid <- data1 %>% group_by(ids) %>% summarise(freq=n()) %>% filter(freq>1) %>% select(ids)
            duplicate_data <- data1[data1$ids %in% dupid$ids, ]
            unduplicate_data <- data1[!data1$ids %in% dupid$ids,]
            # 求和,取最大值
            duplicate_data$sumvalue <- apply(duplicate_data[,2:len], 1, sum)
            clear_duplicate_info <- duplicate_data %>% group_by(ids) %>% filter(sumvalue == max(sumvalue))
            # 合并
            data1 <- rbind(unduplicate_data, as.data.frame(clear_duplicate_info[,1:len]))
            # 抛出报错信息
            sink("oeweb_task.log", append=TRUE, split=FALSE)
            print(paste0(dupid," 表达量数据中存在重复,默认保留表达量高的重复特征 "))
            sink()
        }
    }else{
        if(any(duplicated(data1[,1]))){
            stop(paste0(name," 数据中存在重复,请检查该文件"))
        }
        if ("TRUE" %in% is.na(data1) | "TRUE" %in% is.na(names(data1)) ) { stop(paste0(name,"数据中存在空值,请检查数据的完整性")) }
    }
    
    data2 <- data1[-1]
    row.names(data2) <- data1[,1]
    #### special ####
    rowname <- gsub(pattern = "γ",replacement ="gamma" ,x = row.names(data2))
    rowname <- gsub(pattern = "α",replacement ="alpha" ,x = rowname)
    rowname <- gsub(pattern = "β",replacement ="beta" ,x = rowname)
    #### 日期型基因 ####
    rowname <- gsub(pattern = "43900",replacement ="MARCH10" ,x = rowname)
    rowname <- gsub(pattern = "43901",replacement ="MARCH11" ,x = rowname)
    rowname <- gsub(pattern = "43893",replacement ="MARCH3" ,x = rowname)
    rowname <- gsub(pattern = "43894",replacement ="MARCH4" ,x = rowname)
    rowname <- gsub(pattern = "43895",replacement ="MARCH5" ,x = rowname)
    rowname <- gsub(pattern = "43896",replacement ="MARCH6" ,x = rowname)
    rowname <- gsub(pattern = "43897",replacement ="MARCH7" ,x = rowname)
    rowname <- gsub(pattern = "43898",replacement ="MARCH8" ,x = rowname)
    rowname <- gsub(pattern = "43899",replacement ="MARCH9" ,x = rowname)
    rowname <- gsub(pattern = "44075",replacement ="SEPT1" ,x = rowname)
    rowname <- gsub(pattern = "44084",replacement ="SEPT10" ,x = rowname)
    rowname <- gsub(pattern = "44085",replacement ="SEPT11" ,x = rowname)
    rowname <- gsub(pattern = "44086",replacement ="SEPT12" ,x = rowname)
    rowname <- gsub(pattern = "44088",replacement ="SEPT14" ,x = rowname)
    rowname <- gsub(pattern = "44076",replacement ="SEPT2" ,x = rowname)
    rowname <- gsub(pattern = "44077",replacement ="SEPT3" ,x = rowname)
    rowname <- gsub(pattern = "44078",replacement ="SEPT4" ,x = rowname)
    rowname <- gsub(pattern = "44079",replacement ="SEPT5" ,x = rowname)
    rowname <- gsub(pattern = "44080",replacement ="SEPT6" ,x = rowname)
    rowname <- gsub(pattern = "44081",replacement ="SEPT7" ,x = rowname)
    rowname <- gsub(pattern = "44082",replacement ="SEPT8" ,x = rowname)
    rowname <- gsub(pattern = "44083",replacement ="SEPT9" ,x = rowname)
    row.names(data2) <- rowname
    return(data2)
}

#### draw heatmap ####
draw_heatmap <- function(data, border=F, angle_col=90,border_color = NA,
                         cluster_rows=T, cluster_cols=T,
                         cutree_rows=1, cutree_cols=1,
                         scale="row", treeheight_row=30, treeheight_col=30,
                         show_rownames=F, show_colnames=T,
                         ...){
    
    #### log ####
    dt <- log2(data + 1)
    
    #### pheatmap ####
    pheatmap(dt, border=border, angle_col=angle_col,border_color = NA,
             cluster_rows=cluster_rows, cluster_cols=cluster_cols,
             cutree_rows=cutree_rows, cutree_cols=cutree_cols,
             scale=scale,
             treeheight_row=treeheight_row, treeheight_col=treeheight_col,
             show_rownames=show_rownames, show_colnames=show_colnames)
}

#### pheatmap reorder cluster result ####
reorder_heatmap <- function(data, cluster_rows=T, cluster_cols=T, scale="row",
                            ...){
    
    #### log ####
    dt <- log2(data + 1)
    
    #### reorder cluster result ####
    data1 <- pheatmap(dt, cluster_rows=cluster_rows, cluster_cols=cluster_cols, scale = scale)
    if((cluster_rows==T) & (cluster_cols==T)){
        order_row = data1$tree_row$order
        order_col = data1$tree_col$order
        data2 = data.frame(data[order_row,order_col])
    }else if((cluster_rows==T) & (cluster_cols==F)){
        order_row = data1$tree_row$order
        data2 = data.frame(data[order_row,])
    }else if((cluster_rows==F) & (cluster_cols==T)){
        order_col = data1$tree_col$order
        data2 = data.frame(data[,order_col])
    }else if((cluster_rows==F) & (cluster_cols==F)){
        data2 = data
    }
    data2 = data.frame(rownames(data2), data2, check.names = F)
    colnames(data2)[1] = "IDs"
    write.table(data2, file = "heatmap.reorder_cluster_result.xls", row.names = FALSE, quote = FALSE, sep='\t')
}



# Define UI for application that draws a cluster heatmap
ui <- fluidPage(
    
    # Application title
    titlePanel("Cluster heatmap analysis"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("input_dataset", "数据上传 (.xlsx, .txt, .csv, .xls)", multiple = FALSE, accept = c(".csv", ".xlsx", ".txt", ".xls")),
            
            # Horizontal line ------
            tags$hr(),
            
            selectInput(inputId = "Way of data preprocessing",
                        label = "数据预处理方式选择：",
                        selected = "默认无需处理",
                        choices = c("默认无需处理", "芯片数据（e.g.标准化矩阵）", "测序数据 (e.g.fpkm)", "蛋白数据 (e.g. 定量矩阵)", "代谢数据 (e.g. 质谱强度)", "微生物数据 (e.g. 相对丰度)")),
            
            
            selectInput(inputId = "Clustering on row",
                        label = "对行进行聚类",
                        selected = "是",
                        choices = c("是","否")),
            
            selectInput(inputId = "Clustering on column",
                        label = "对列进行聚类",
                        selected = "是",
                        choices = c("是","否")),
            
            selectInput(inputId = "Color system",
                        label = "聚类图色系",
                        selected = "内置默认",
                        choices = c("内置默认")),
            
            submitButton("提交运行", icon = icon("calendar-check")),
            
            # Horizontal line ------
            tags$hr(),
            
            wellPanel(
                radioButtons("extension", "另存为:",
                             choices = c("png", "pdf", "svg"), inline = TRUE),
                downloadButton("download", "下载结果", icon = icon("cloud-download-alt"))
            )
        ),
        
        
        
        
        # Main panel for displaying outputs
        mainPanel(
            tableOutput("contents"),
            plotOutput("heatmapPlot")
        )
    )
)

# Define server function
server <- function(input, output, session) {
    
    
    # plot heatmap
    output$heatmapPlot <- renderPlot({
        req(input$input_dataset)
        # make sure the uploaded file is a csv file
        ext <- tools::file_ext(input$input_dataset$name)
        validate(need(ext == "csv", "Invalid file. Please upload the right file type."))
        data <- read.csv(input$input_dataset$datapath, row.names = 1)
        draw_heatmap(data, border=F, angle_col=0, cluster_rows=T, cluster_cols=T, scale="row", show_rownames=T, show_colnames=T)
        
    })
    
    output$contents <- renderText({
        print("success!", size = 20)
    })
    
    # save heatmap using downloadhandler and plot output type
    output$download <- downloadHandler(
        filename = function(){
            paste("heatmapPlot", input$extension, sep = ".")
        },
        content = function(file){
            ggsave(file, plot_output(), device = input$extension)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)