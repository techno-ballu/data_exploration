source("download.R")
Y_LABEL_NZV = "Unique Feature Value %age"
X_LABEL_NZV = "Features"
Y_LABEL_FR_NZV = "Frequency Ratio"
Y_LABEL_Ranking = "Importance"


PERCENT <- 100
ROUND <- 1

plotOutput200 <- function (outputId, width = "100%", height = "200px", clickId = NULL, 
                           hoverId = NULL, hoverDelay = 300, hoverDelayType = c("debounce", 
                                                                                "throttle"), inline = FALSE) 
{
        if (is.null(clickId) && is.null(hoverId)) {
                hoverDelay <- NULL
                hoverDelayType <- NULL
        }
        else {
                hoverDelayType <- match.arg(hoverDelayType)[[1]]
        }
        style <- if (!inline) {
                paste("width:", validateCssUnit(width), ";", "height:", 
                      validateCssUnit(height))
        }
        container <- if (inline) 
                span
        else div
        container(id = outputId, class = "shiny-plot-output", style = style, 
                  `data-click-id` = clickId, `data-hover-id` = hoverId, 
                  `data-hover-delay` = hoverDelay, `data-hover-delay-type` = hoverDelayType)
}

textOutputWithClass <- function (classId, container = if (inline) span else div, inline = FALSE) {
        container(class = paste("shiny-text-output",classId,sep = " "))
}

category_labeling <- function(DATA,METRIC) {
        types <- lapply(DATA, class)
        distincts <- lapply(DATA, function(c) unique(c))
        categoryMappings <- list()
        
        for(i in names(DATA)){
                noOfCats <- length(levels(distincts[[i]]))
                if (noOfCats == 1) {
                        DATA[[i]] <- NULL
                } else if ((noOfCats <= length(DATA[[i]])/2 && types[[i]] == "factor") || types[[i]] == "character") {
                        means <- sapply(split(DATA[[METRIC]], DATA[[i]]), function(x) mean(x, na.rm=TRUE))
                        DATA[[i]] <- as.character(DATA[[i]])
                        for(j in names(means)) {
                                if (j == "")
                                        next
                                DATA[[i]][DATA[[i]] == j] <- means[[j]]
                        }
                        DATA[[i]] <- as.numeric(DATA[[i]])
                        categoryMappings[[i]] <- means
                }
        }
        return(list(map=categoryMappings,data=DATA))
}

isNumerical <- function(non_nas) {
        if (is.numeric(non_nas)) {
                # check for ordinal cases
                sorted <- sort(unique(non_nas))
                len <- length(sorted)
                if (is.integer(sorted) && len < 50) {
                        xAxis <- c(1:len)
                        fit <- lm(formula = sorted ~ xAxis)
                        summaryFit <- summary.lm(fit)
                        rsquared <- ifelse(is.na(summaryFit$adj.r.squared),summaryFit$r.squared,summaryFit$adj.r.squared)
                        if (is.na(rsquared))
                                return(T)
                        rsquared <- round(rsquared,1)
                        if (rsquared != 1)
                                return(T)
                } else
                        return(T)
        }
        return(F)
}

numericals <- function(data) {
        nums <- sapply(data, function(x) {
                non_nas <- x[!is.na(x)]
                return(isNumerical(non_nas))
        })
        
        # null checks
        if (length(nums) == 0)
                return(NULL)
        
        numerics <- data[ , nums]
        if (length(which(nums)) == 1) {
                numerics <- data.frame(numerics)
                colnames(numerics)[1] <- names(which(nums))
        }
        return(numerics)
}

categoricals <- function(data) {
        cats <- sapply(data, function(x) {
                non_nas <- x[!is.na(x)]
                
                if (is.numeric(non_nas)) {
                        # check for ordinal cases
                        sorted <- sort(unique(non_nas))
                        len <- length(sorted)
                        if (is.integer(sorted) && len < 50) {
                                xAxis <- c(1:len)
                                fit <- lm(formula = sorted ~ xAxis)
                                summaryFit <- summary.lm(fit)
                                rsquared <- ifelse(is.na(summaryFit$adj.r.squared),summaryFit$r.squared,summaryFit$adj.r.squared)
                                if (is.na(rsquared))
                                        return(F)
                                rsquared <- round(rsquared,1)
                                if (rsquared == 1)
                                        return(T)
                        }
                        return(F)
                }
                
                if (class(x) %in% c("character", "factor")) {
                        good.rows<-ifelse(nchar(non_nas,c("chars"),T)>=8,FALSE,TRUE)
                        
                        if (length(which(!good.rows)) == 0) {
                                print(head(x))
                                # check if date?
                                as_date <-as.Date(non_nas,format="%m/%d/%Y")
                                
                                if (sum(!is.na(as_date)) > 0)
                                        return(F)
                        }
                        
                        dis <- length(unique(non_nas))
                        if (dis <= length(non_nas)/2)
                                return(T)
                }
                return(F)
        })
        
        if (length(cats) == 0)
                return(NULL)
        
        cate <- data[ , cats]
        if (length(which(cats)) == 1) {
                cate <- data.frame(cate)
                colnames(cate)[1] <- names(which(cats))
        }
        
        return(cate)
}

shinyServer(function(input, output, session) {
        
        options(shiny.maxRequestSize=30*1024^2)
        
        values <- reactiveValues()
        
        metadata <- function(raw) {
                
                # null check
                if (is.null(raw))
                        return(NULL)
                
                names <- names(raw)
                
                # the class
                classes <- mapply(function(x,name) {
                        non_nas <- x[!is.na(x)]
                        
                        # check if date?
                        if (!is.numeric(non_nas)) {
                                good.rows<-ifelse(nchar(non_nas,c("chars"),T)>=8,FALSE,TRUE)
                                
                                if (length(which(!good.rows)) == 0) {
                                        as_date1 <-as.Date(non_nas,format="%m/%d/%Y")
                                        as_date2 <-as.Date(non_nas,format="%d/%m/%Y")
                                        as_date3 <-as.Date(non_nas,format="%Y-%m-%d")
                                        if (sum(!is.na(as_date1)) > 0 || sum(!is.na(as_date2)) > 0 || sum(!is.na(as_date3)) > 0)
                                                return("Date")                                        
                                }
                        }
                        
                        if(is.numeric(non_nas)){
                                # check for ordinal
                                sorted <- sort(unique(non_nas))
                                len <- length(sorted)
                                cat(name, class(sorted), len, '\n')
                                if (is.integer(sorted) && len < 50) { #length(non_nas)/2
                                        xAxis <- c(1:len)
                                        fit <- lm(formula = sorted ~ xAxis)
                                        summaryFit <- summary.lm(fit)
                                        rsquared <- ifelse(is.na(summaryFit$adj.r.squared),summaryFit$r.squared,summaryFit$adj.r.squared)
                                        rsquared <- round(rsquared,1)
                                        cat(name, rsquared, '\n')
                                        if (rsquared == 1) {
                                                cat(name, 'CATEGORICAL\n')
                                                return("CATEGORICAL")
                                        }
                                }
                                cat(name, 'NUMERICAL\n')
                                return("NUMERICAL")
                        } else if(class(non_nas) %in% c("character", "factor")) {
                                dis <- length(unique(non_nas))
                                if (dis <= length(non_nas)/2) {
#                                         cat(name, 'CATEGORICAL\n')
                                        return("CATEGORICAL")
                                } else {
#                                         cat(name, 'Others\n')
                                        return("Others")
                                }
                        }
                        return(NA)
                },raw,names)
                classes <- unlist(classes)
                
                # force class on columns
                for(name in names(classes)) {
                        if (!is.na(classes[[name]]) && classes[[name]] == "CATEGORICAL") {
                                raw[[name]] <- as.character(raw[[name]])
                        }
                }
                
                # the lengths
                lengths <- sapply(raw,function(x) {
                        return(length(x))
                })
                lengths <- unlist(lengths)
                
                # the missing
                missings <- sapply(raw,function(x) {
                        return(sum(is.na(x)))
                })
                missings <- unlist(missings)
                
                # the available
                availabilities <- lengths - missings
                
                missings <- paste(round((unlist(missings) / lengths)*PERCENT,ROUND),"%",sep = "")
                
                # the distincts
                distincts <- sapply(raw, function(c) {
                        t <- c[!is.na(c)]
                        length(unique(t))
                })
                
                # the mode
                findingMode <- function(x) {
                        ux <- unique(x)
                        if (length(ux) != length(x)) {
                                value <- ux[which.max(tabulate(match(x, ux)))]
                                if (is.numeric(value))
                                        value <- round(value,1)
                                return(value)      
                        }
                        return(NA)
                }
                modes <- sapply(raw, findingMode)
                
                # the mins
                mins <- sapply(raw, function(x) {
                        if (is.numeric(x))
                                return(round(min(x,na.rm = T),1))
                        return(NA)
                })
                mins <- unlist(mins)
                
                # the maxs
                maxs <- sapply(raw, function(x) {
                        if (is.numeric(x))
                                return(round(max(x,na.rm = T),1))
                        return(NA)
                })
                maxs <- unlist(maxs)
                
                # the means
                means <- sapply(raw, function(x) {
                        if (is.numeric(x))
                                return(round(mean(x,na.rm = T),1))
                        return(NA)
                })
                means <- unlist(means)
                
                # the medians
                medians <- sapply(raw, function(x) {
                        if (is.numeric(x))
                                return(round(median(x,na.rm = T),1))
                        return(NA)
                })
                medians <- unlist(medians)
                
                # the standard deviation
                sds <- sapply(raw, function(x) {
                        if (is.numeric(x))
                                return(sd(x,na.rm = T))
                        return(NA)
                })
                sds <- unlist(sds)
                
                minSD <- round((means-mins)/sds,1)
                maxSD <- round((maxs-means)/sds,1)
                outliersScore <- mapply(function(x,y) {
                        max(x,y)
                }, minSD, maxSD)
                
                isolate(order <- values$order <- sort(outliersScore,decreasing = T))
                mini <- min(order)
                maxi <- max(order)
                updateSliderInput(session,"common-threshold",min = mini, max = maxi, value = (maxi-mini)/10 + mini)
                
                # classes, missings, availabilities, distincts, modes, mins, maxs, means, medians, sds
                stats <- data.frame(Name=names,Type=classes,Missing=missings,Available=availabilities,Distinct=distincts,Mode=modes,Mean=means,Median=medians,Min=mins,Max=maxs,SD=sds,"Min SDs"=minSD,"Max SDs"=maxSD,outliers=outliersScore,stringsAsFactors = F) #outliers=outliersScore,
                return(stats)
        }
        
        # recalculate the new order of outliers score!
        calculateOrder <- function(data) {
                # the means
                means <- sapply(data, function(x) {
                        if (is.numeric(x))
                                return(round(mean(x,na.rm = T),1))
                        return(NA)
                })
                means <- unlist(means)
                
                # the standard deviation
                sds <- sapply(data, function(x) {
                        if (is.numeric(x))
                                return(round(sd(x,na.rm = T),1))
                        return(NA)
                })
                sds <- unlist(sds)
                
                # the mins
                mins <- sapply(data, function(x) {
                        if (is.numeric(x))
                                return(round(min(x,na.rm = T),1))
                        return(NA)
                })
                mins <- unlist(mins)
                
                # the maxs
                maxs <- sapply(data, function(x) {
                        if (is.numeric(x))
                                return(round(max(x,na.rm = T),1))
                        return(NA)
                })
                maxs <- unlist(maxs)
                
                minSD <- round((means-mins)/sds,1)
                maxSD <- round((maxs-means)/sds,1)
                outliersScore <- mapply(function(x,y) {
                        max(x,y)
                }, minSD, maxSD)
                
                isolate({
                        order <- values$order <- sort(outliersScore,decreasing = T)
                        mini <- min(order)
                        maxi <- max(order)
                        updateSliderInput(session,"common-threshold",min = mini, max = maxi, value = (maxi-mini)/10 + mini)
                })
        }
        
        tooltip_rows <- "<h3>y wells</h3><p>have x% features missing</p><p><i>(click bar to remove these wells)</i></p><p>text</p>"
        tooltip_cols <- "<h3>y features</h3><p>have values missing for x% wells</p><p><i>(click bar to remove these features)</i></p><p>text</p>"
        tooltip_nums <- "<h3>y counts</h3><p>between <b>low</b> (x1%) & <b>high</b> (x2%)</p><p><i>(click bar to replace these outliers by NA)</i></p>"
        tooltip_cats <- "<h3>y counts</h3><p>for category <b>x</b></p><p><i>(click bar to replace this category by NA)</i></p>"
        tooltip_trans <- "<h3>y counts</h3><p>between <b>low</b> & <b>high</b></p>"
        tooltip_pair <- "<h3>y</h3><p>n missing for key</p><p><i>(click to remove this feature)</i></p>"
        tooltip_rank <- "<h4> x <p>feature</p> </h4><p> has z imporatnace value</p>"
        
        DATA <- reactive({
                # input$file1 will be NULL initially. After the user selects
                # and uploads a file, it will be a data frame with 'name',
                # 'size', 'type', and 'datapath' columns. The 'datapath'
                # column will contain the local filenames where the data can
                # be found.
                inFile <- input$file1
                if (is.null(inFile))
                        return(NULL)
                
                data <- read.csv(inFile$datapath, fileEncoding="latin1", stringsAsFactors=F)  
                values$data <- data
                #     str(values$data)
                return(data)
        })
        
        loadData <- function(counter) {
                print(paste("try loading data using option # ",counter))
                tryCatch({
                        if (counter == 1)
                                data <- values$data <- fread(input$file1$datapath, stringsAsFactors=F, data.table = F,verbose=F, header = T ) #fileEncoding="latin1"     
                        else if (counter == 2)
                                data <- values$data <- read.csv(input$file1$datapath, fileEncoding="latin1", stringsAsFactors=F, header = T) 
                        else if (counter == 3)
                                data <- values$data <- read.csv(input$file1$datapath, fileEncoding="latin1", stringsAsFactors=F, sep = "", header = F)
                        else if (counter == 4)
                                data <- values$data <- read.csv(input$file1$datapath, fileEncoding="latin1", stringsAsFactors=F, sep = "", header = T)                       
                }, error = function(e) {
                        print(e)
                        throw("Error in loading CSV!")
                })
                
                # getting the # of rows & columns to check if data was loaded correctly!
                ncols <- ncol(data)
                nrows <- nrow(data)
                print(paste(ncols,nrows))
                
                if (ncols <= 1 || nrows <= 1) {
                        print("CSV read improperly!")
                        throw("CSV read improperly!");
                }
                
                return(data)
        }
        
        updateDATA <- observe({
                if (!is.null(input$file1$datapath)) {
                        
                        isolate({
                                data <- NULL
                                ncols <- nrows <- 0
                                counter <- 0
                                bool <- TRUE
                                while(bool) {
                                        tryCatch({
                                                counter <- counter + 1
                                                data <- loadData(counter)
                                                bool <- FALSE
                                        }, 
                                        warning = function(e) {
                                                print(paste("option",counter,"gave a warning!trying next approach..."))
                                                if (counter > 5)
                                                        throw("Could not load CSV!");
                                        }, 
                                        error = function(e) {
                                                print(paste("option",counter,"did not work!trying next approach..."))
                                                if (counter > 5)
                                                        throw("Could not load CSV!");
                                        }, finally = print(paste("Loaded data successfully with",nrow(data),"rows"))) ##
                                }
                                
                                values$copy <- values$data
                                values$logs <- c()
                                values$features <- names(values$data)
                                
                                
                                if (!is.null(numericals(values$data)))
                                        numericNames <- colnames(numericals(values$data))
                                
                                allNames <- colnames(values$data)
                                # the mode
                                findingIDs <- function(x) {
                                        ux <- unique(x)
                                        if (length(ux) == length(x)) {
                                                return(T)      
                                        }
                                        return(F)
                                }
                                #         ids <- sapply(values$data, findingIDs)
                                ids <- apply(values$data, 2, findingIDs)
                                idCols <- names(which(ids))
                                if (length(idCols) == 0) {
                                        idCols <- c("auto-id")
                                        #           values$data[[idCols]] <- 1:nrow(data)
                                        id <- 1:nrow(data)
                                        values$data <- cbind("auto-id"=id,data)
                                }
                                #         inds <- which(numericNames %in% c(input$id))
                        })
                        updateTabsetPanel(session, "explore-data", selected = "metadata")
                        updateSelectInput(session, "id", choices = idCols) # , selected = numericNames[1]
                        updateSelectInput(session, "metrics", choices = numericNames) # , selected = numericNames[1]
                        updateSelectInput(session, "useless", choices = allNames)
                        updateSelectInput(session, "selected", choices = allNames, selected = allNames)
                }
        })
        
        # Update variable selection
        observe({
                if (input$id != "" && !is.null(values$features)) {
                        isolate({
                                if (!is.null(numericals(values$data)))
                                        numericNames <- colnames(numericals(values$data))
                                
                                if (!is.null(categoricals(values$data))) {
                                        categoricalNames <- colnames(categoricals(values$data))
                                }
                                
                                indN <- which(numericNames %in% c(input$id))
                                indC <- which(categoricalNames %in% c(input$id))
                                
                                if (length(indN) > 0)
                                        numericNames <- numericNames[-c(indN)]
                                else if (length(indC) > 0)
                                        categoricalNames <- categoricalNames[-c(indC)]
                                
                                #         if (input$x=="" && input$y=="" && is.null(input$quickFindNum) && input$quickFindCat=="") { #&& is.null(input$quickFindCat)
                                updateSelectInput(session, "x", choices = numericNames)
                                updateSelectInput(session, "y", choices = numericNames)
                                updateSelectInput(session, "quickFindNum", choices = numericNames)
                                updateSelectInput(session, "quickFindCat", choices = categoricalNames)
                                updateSelectInput(session, "xC", choices = categoricalNames,selected = categoricalNames[1])
                                updateSelectInput(session, "yC", choices = categoricalNames,selected = categoricalNames[2])
                                updateSelectInput(session, "xCN", choices = categoricalNames,selected = categoricalNames[1])
                                updateSelectInput(session, "yCN", choices = numericNames,selected = numericNames[1])
                                updateSelectInput(session, "numericFeatures", choices = numericNames) #  
                                updateSelectInput(session, "categoricalFeatures", choices = categoricalNames) #  
                                updateSelectInput(session, "numericbin_feature", choices = numericNames, selected = numericNames[1])
                                updateSelectInput(session, "rankingFeature", choices = numericNames)
                                
                                
                        })
                }
        })
        
        # Update variable selection
        observe({
                if (!is.null(input$selected)) {
                        isolate(values$data <- values$data[,c(input$selected)])
                }
        })
        
        # Update features
        observe({
                if (!is.null(values$data)) {
                        isolate(values$features <- names(values$data))
                }
        })
        
        #   DATA <- DATA <- NULL
        output$statistics <- renderDataTable({
                metadata(values$data)
        },options = list(
                pageLength = 10,
                lengthChange=FALSE,
                searching=FALSE,
                autoWidth=TRUE,
                paging=FALSE,
                scrollCollapse=TRUE,
                scrollX=TRUE,
                scrollY="435px",
                #     dom='T<"clear">lfrtip',
                dom='rt<"pull-left top-padding"i><"pull-right top-padding"T><"clear">',
                #     ordering=FALSE,
                tableTools=list(
                        "sSwfPath" = "/dataTables.extentions/copy_csv_xls_pdf.swf",
                        "aButtons" = list("copy","print",list("sExtends" = "collection",
                                                              "sButtonText" = "Save",
                                                              "aButtons" = c("xls","pdf")
                        ))
                )
        ))
        
        # columns order
        observe({
                # convert to 1 based index
                colIDs <- input$columnsOrder + 1
                isolate({
                        n <- names(values$data)[colIDs]
                        values$data <- values$data[,c(n)]
                })
        })
        
        # columns visibility
        observe({
                colID <- input$columnVisibility
                if (!is.null(colID))
                        isolate({
                                col <- names(values$data)[colID$column+1]
                                state <- colID$state
                                if (state == T) {
                                        values$data[[col]] <- values$copy[[col]]
                                } else {
                                        values$data <- values$data[,which(!names(values$data) %in% c(col))]
                                }
                        })
        })
        
        output$rowsNCols <- renderUI({
                text <- ""
                data <- values$data
                isolate({
                        if (!is.null(data)) {
                                columns <- ncol(data)
                                rows <- nrow(data) 
                                summ <- paste("<b>Summary</b>")
                                rowsTxt <- paste("# of rows:columns = ",rows,":",columns,sep="")
                                colsTxt <- paste("<pre>",rowsTxt,"</pre>",sep="")
                                text <- paste(summ,colsTxt,sep = "<br/>")
                        }
                        HTML(text)
                })
        })
        
        output$contents <- renderDataTable({
                values$data[1:100,]
        }, options = list(
                pageLength = 10,
                lengthChange=FALSE,
                searching=TRUE,
                autoWidth=TRUE,
                paging=FALSE,
                scrollCollapse=TRUE,
                scrollX=TRUE,
                scrollY="370px",
                dom='rt<"pull-left"i><"pull-right"flp><"clear">',
                ordering=FALSE
                # will be required later
                #     drawCallback=I(
                #       'function(table) {
                #         // debugger      
                #         // var order = null;
                #         // var colReorder = new $.fn.dataTable.ColReorder( table, {
                #         //  "reorderCallback": function () {
                #   	    //     debugger
                #         //    Shiny.onInputChange("foo_info", colReorder.fnOrder());
                #         //    // order = colReorder.fnOrder()
                #   	   //    }
                #   	   //  });
                #         //if (order !== null) {
                #           //colReorder.fnOrder(order)
                #         //}
                #         // var counter = 0;
                #         // var columns = new Array();
                #         // $.each(table.aoColumns, function(c){
                #         //    if(table.aoColumns[c].bVisible == true){
                #         //        columns.push(counter)
                #         //     }
                #         //    counter++;
                #         // });
                #         Shiny.onInputChange("columnsOrder", table._colReorder.fnOrder());
                #     }')
        )
        # , callback = 'function(table) { 
        #             table.on("column-visibility", function(e, table, column, state) { 
        #               var result = {
        #                 column:column,
        #                 state:state
        #               };
        #               Shiny.onInputChange("columnVisibility", result); 
        #             }); 
        #           }'
        ) 
        
        # Update features
        observe({
                name <- input$quickFindCat
                
                if (!is.null(name) && name != "") {
                        isolate({
                                x <- values$data[[name]]
                                all <- unique(as.character(x))
                                
                                grps <- c()
                                emptyS <- all[all == ""]
                                if (length(emptyS) > 0)
                                        grps <- c(grps,"empty strings")
                                
                                # identify category groups
                                all <- all[all != ""]
                                all <- sort(all,na.last = T)
                                
                                all <- substr(all,1,1)
                                alphanum <- all[!is.na(as.numeric(all))]
                                if (length(alphanum) > 0)
                                        grps <- c(grps,"alpha")
                                
                                alphabets <- unique(toupper(all[is.na(as.numeric(all))]))
                                grps <- c(grps,alphabets)
                                
                                # Keep code for later use
                                #         grps <- lapply(all, function(item) {
                                #           if (is.na(item))
                                #             return("NAs")
                                #           
                                #           if (item != "") {
                                #             cate <- substr(item,1,1)
                                #             #           print(cate)
                                #             if (!is.na(as.numeric(cate))) { 
                                #               return("alphnumeric")
                                #             } else if (is.character(cate)) {
                                #               return(toupper(cate))
                                #             }
                                #             return("Others")
                                #           }
                                #         })
                                #         grps <- unique(unlist(grps))
                                if (length(all) > 100)
                                        updateCheckboxGroupInput(session, "namesCategories", choices = grps, selected = grps[1],inline = T)
                                else 
                                        updateCheckboxGroupInput(session, "namesCategories", choices = grps, selected = grps,inline = T)
                        })
                }
        })
        
        # Insert the right number of plot output objects into the web page
        output$clean_categorical <- renderD3BubbleChart({
                isolate(name <- input$quickFindCat)
                selectedGrp <- input$namesCategories
                
                if (is.null(name) || is.null(values$data[[name]]))
                        return(NULL)
                
                isolate({
                        x <- values$data[[name]]
                        selected <- c()
                        
                        if (!is.null(selectedGrp) && selectedGrp != "") {
                                cats <- unique(as.character(x))
                                first <- toupper(substr(cats,1,1))
                                
                                if ("alpha" %in% c(selectedGrp)) {
                                        selected <- c(selected,cats[!is.na(as.numeric(first))])
                                }
                                if ("empty strings" %in% c(selectedGrp)) {
                                        selected <- c(selected,"")
                                }
                                remove <- c("alpha","empty strings")
                                selectedGrp <- selectedGrp[! selectedGrp %in% remove]
                                selected <- c(selected,cats[which(first %in% c(selectedGrp))])
                        }
                        selected <- c(selected,NA)
                })
                
                modified <- list()
                isolate({
                        if (is.null(values$modified))
                                values$modified <- list()  
                        
                        info <- input[["modal-hidden"]]
                        if (!is.null(info)) {
                                dragged <- info$data$dragged
                                target <- info$data$target
                                
                                if (!is.null(target) && target != "NA") {
                                        values$modified <- c(values$modified,target)
                                        modified <- values$modified  
                                }
                        }
                })
                
                x <- factor(x, exclude=NULL)
                
                if (length(selected) > 0)
                        # remove un-selected categories
                        x <- subset(x, x %in% c(selected))
                
                pivot <- table(x)
                cats <- names(pivot)
                if (length(selected) > 0)
                        cats <- cats[which(cats %in% c(selected))]
                pivot <- as.numeric(pivot)
                if (length(selected) > 0)
                        pivot <- pivot[pivot > 0]
                nas <- cats[is.na(cats)]
                cats[is.na(cats)] <- "NA"
                if (length(nas) == 0) {
                        cats[length(cats)+1] <- "NA"
                        pivot[length(pivot)+1] <- 0
                }
                return(list(x=cats,y=pivot,tooltip=tooltip_cats,ylabel=paste("bubblesplot-",name,sep = ""),evt="clickdatapoint",range=list(min=min(pivot),max=max(pivot)),modified=modified)) #
        })
        
        # Insert the right number of plot output objects into the web page
        output[["cat-cat"]] <- renderD3BubbleClusterChart({
                names <- c(input$xC,input$yC)
                isolate({
                        data <- subset(values$data,select=names)
                        groups <- split(data[[input$xC]],data[[input$yC]])
                        tables <- lapply(groups, function(x) {
                                as.list(table(x))
                        })
                })
                return(list(y=tables,ylabel=paste("bubbles-clusters-plot"),groupby=input$yC,feature=input$xC,range=list(min=min(unlist(tables)),max=max(unlist(tables))))) #
        })
        
        # The output chart that shows box plot using d3 for exploring categorical(x) vs numerical(y) relationships
        output$categoricalVSnumerical <- renderD3BoxPlot({
                feature <-input$xCN
                dist <- input$yCN
                names <- c(feature,dist)
                
                isolate({
                        data <- subset(values$data,select=names)
                        # remove NAs from data
                        data <- data[!is.na(data[[dist]]), ]
                        groups <- split(data[[dist]],data[[feature]])
                        tables <- lapply(groups, function(x) {
                                lapply(x, identity)
                        })
                })
                return(list(y=tables,ylabel=paste("box-plot"),dist=dist,feature=feature,range=list(min=min(unlist(tables)),max=max(unlist(tables))))) #
        })
        
        # Insert the right number of plot output objects into the web page
        output$clean_numericals <- renderUI({
                ID <- input$id
                isolate(order <- values$order)
                input[["rearrange-outliers"]]
                if (input$numberOfNumFeatures != "All") {
                        orderedNames <- head(names(order),as.numeric(input$numberOfNumFeatures))  
                } else {
                        orderedNames <- names(order)
                }
                
                if (!is.null(input$quickFindNum))
                        orderedNames <- input$quickFindNum
                
                for (i in orderedNames) {
                        if (i == ID) 
                                next
                        
                        local({
                                feature <- i
                                plot_name <- paste("histogram-",feature,sep = "")
                                slid_name <- paste("slider_",feature,sep = "")
                                box_name <- paste("box_",feature,sep = "")
                                
                                output[[plot_name]] <- renderBarChart({
                                        #           browser(expr = identical(feature, "INITIAL_FRAC_GRADIENT"))
                                        x <- values$data[[feature]]
                                        if (is.null(values$data[[feature]]))
                                                return(NULL)
                                        t <- x[!is.na(x)]
                                        ux <- unique(t)
                                        max <- length(ux)
                                        brks <- seq(min(t),max(t),l=input[[slid_name]] + 1)
                                        if (input[[slid_name]] == max)
                                                brks <- ux
                                        
                                        h <- hist(t,plot = F,breaks=brks) # ux
                                        b <- h$breaks
                                        percentile <- ecdf(b)
                                        p <- percentile(b)*PERCENT
                                        breaks <- b[1:length(b)-1]
                                        counts <- h$counts
                                        plot_name <- gsub('[-]','_',plot_name)
                                        plot_name <- gsub('[.]','_',plot_name)
                                        return(list(x=breaks,y=counts,tooltip=tooltip_nums,ylabel="(frequency)",evt="clickdatapoint",d=p,max=max(t), type=plot_name)) #  xlabel=feature,
                                })
                                
                                output[[box_name]] <- renderPlot({
                                        x <- values$data[[feature]]
                                        t <- x[!is.na(x)]
                                        boxplot(t,horizontal = T)
                                }, height = 200, width = 350)
                        })
                }
                
                plot_output_list <- lapply(orderedNames, function(name) {
                        if (name == ID)
                                return(NULL)
                        
                        #       browser(expr = identical(name, "Acid..Gals."))
                        isolate(x <- values$data[[name]])
                        t <- x[!is.na(x)]
                        max <- length(unique(t))
                        val <- min(15, max)
                        
                        plot_name <- paste("histogram-",name,sep = "")
                        box_name <- paste("box_",name,sep = "")
                        slid_name <- paste("slider_",name,sep = "")
                        wellPanel(
                                fluidRow(
                                        column(2, 
                                               h4(name),
                                               sliderInput(slid_name, "# of bins", min = 3, max = max, value = val, step = 1),
                                               helpText(paste("Maximum bins = ", max, " (which is the # of distinct values)"))
                                        ),
                                        column(6,
                                               downloadInput('histogram'),
                                               nvd3ChartOutput(plot_name,"nvd3-histogram",height = 200)
                                        ),
                                        column(4, 
                                               downloadInput('box'),
                                               plotOutput200(box_name))
                                )
                        )
                })
                
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                
                do.call(tagList, plot_output_list)
        })
        
        observe({
                #     # listen for changes to toggle modals
                if (!is.null(input$clickdatapoint)) {
                        inp <- input$clickdatapoint
                        if (inp$featureID == "clean_row")
                                datapoint <- list(featureID=inp$featureID,barNos=inp$barNos,info=paste("Do you want to remove",inp$y,"well/s with",inp$x,"% features missing?"))
                        else if (inp$featureID == "clean_col")
                                datapoint <- list(featureID=inp$featureID,barNos=inp$barNos,info=paste("Do you want to remove",inp$y,"feature/s missing in",inp$x,"% wells?"))
                        else if (grepl("histogram",inp$featureID) == T)
                                datapoint <- list(featureID=inp$featureID,barNos=inp$barNos,info=paste("Do you want to replace",inp$value,"outlier/s by NAs ?"))
                        else if (grepl("bubbles",inp$featureID) == T) {
                                title <- paste("Do you want to replace category '",inp$dragged,"' by '",inp$target,"' ?",sep = "")
                                if (inp$dragged == "")
                                        title <- paste("Do you want to replace these empty strings by '",inp$target,"' ?")
                                datapoint <- list(featureID=inp$featureID,dragged=inp$dragged,target=inp$target,info=title)
                        }else if (inp$featureID == "nearZeroVariancePercent"){
                                datapoint <- list(featureID=inp$featureID,barNos=inp$x,info=paste("Do you want to remove",inp$x,"feature having",round(inp$y,digits=1),"% unique value percentage")) 
                        }else if (inp$featureID == "nearZeroVarianceFreq"){
                                datapoint <- list(featureID=inp$featureID,barNos=inp$x,info=paste("Do you want to remove ",inp$x,"feature  with ",round(inp$y,digits=1),"frequency ratio")) 
                        }else if (inp$featureID == "rankingAndImportance"){
                                datapoint <- list(featureID=inp$featureID,barNos=inp$x,info=paste("Do you want to remove ",inp$x,"feature  with ",round(inp$y,digits=1),"importance")) 
                        }
                        toggleModal(session, "confirm",datapoint)
                }
        })
        
        observe({
                if (!is.null(input[["modal-hidden"]])) {
                        info <- input[["modal-hidden"]]
                        bars <- info$data$barNos
                        user <- info$modalinfo[[1]]
                        comments <- info$modalinfo[[2]]
                        if (comments == "")
                                comments <- "not specified"
                        
                        id <- info$data$featureID
                        if (!is.null(bars)) 
                                toClean <- unlist(strsplit(bars,",",fixed = T))
                        isolate(data <- values$data)
                        
                        # input$xwells - the wells selected one by one to remove
                        if (!is.null(data)) { #&& !is.null(bars)
                                if (id == "clean_row") {
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"removing well",paste(toClean,collapse = ","),"(",comments,")")))
                                        data <- data[!data[[input$id]] %in% toClean, ]
                                        isolate(values$data <- data)
                                } else if (id == "clean_col") {
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"removing feature",paste(toClean,collapse = ","),"(",comments,")")))
                                        data <- data[,-which(names(data) %in% c(toClean))]
                                        isolate(values$data <- data)
                                } else if (grepl("histogram",id) == T) {
                                        feature <- unlist(strsplit(id,"-",fixed = T))[2]
                                        if (!is.null(feature)) {
                                                isolate(x <- values$data[[feature]])
                                                low <- as.numeric(toClean[1])
                                                high <- as.numeric(toClean[2])
                                                #           outliers <- x[x>low & x<=high] 
                                                #           if (low < 0 && high < 0)
                                                outliers <- x[x>=low & x<=high] 
                                                isolate(values$logs <- c(values$logs, paste(user," @ ",format(Sys.time(), "%a %b %d %X %Y"), " replacing outlier '",paste(outliers[!is.na(outliers)],collapse = ","),"' for ",feature," by NA ","(",comments,")",sep = ""))) #,"between",low,"and",high
                                                x[x %in% outliers] <- NA # replace outlier by NA?
                                                isolate(values$data[[feature]] <- x)
                                        }
                                } else if (grepl("bubbles",id) == T) {
                                        feature <- unlist(strsplit(id,"-",fixed = T))[2]
                                        dragged <- info$data$dragged
                                        target <- info$data$target
                                        
                                        if (!is.null(feature)) {
                                                isolate(x <- values$data[[feature]])
                                                if (length(dragged) == 0)
                                                        dragged = ""
                                                
                                                if (target == "NA" && length(which(isUnknown(x, c(dragged)))) > 0) {
                                                        x <- unknownToNA(x, unknown=c(dragged))
                                                } else {
                                                        outliers <- x[x == dragged] 
                                                        x[x %in% outliers] <- target
                                                }
                                                
                                                isolate(values$logs <- c(values$logs, paste(user," @ ",format(Sys.time(), "%a %b %d %X %Y"), " replacing outlier category '",dragged,"' for ",feature," by '",target,"' (",comments,")",sep = ""))) #,"between",low,"and",high
                                                
                                                x <- factor(x, exclude=NULL)
                                                isolate(values$data[[feature]] <- x)
                                        }
                                } else if (id == "auto-clean-rows-cols") {
                                        rowsThres <- as.numeric(info$modalinfo[[3]])
                                        colsThres <- as.numeric(info$modalinfo[[4]])
                                        missingRows <- round((rowsThres/PERCENT)*ncol(data),0)
                                        NA_RowWise <- apply(data, 1, function(z) sum(is.na(z)))
                                        
                                        ids <- data[[input$id]]
                                        df <- data.frame(x=ids,y=NA_RowWise) # data frame
                                        fact <- factor(df$y) # as factor
                                        wells <- split(df,fact)
                                        
                                        wells <- unlist(mapply(function(z,name) { 
                                                name <- as.numeric(name)
                                                if (name >= missingRows)
                                                        z$x
                                        },wells,names(wells)))
                                        data <- data[!ids %in% wells, ]
                                        
                                        missingCols <- round((colsThres/PERCENT)*nrow(data),ROUND)
                                        NA_ColWise <- apply(data, 2, function(z) sum(is.na(z)))
                                        fact <- factor(NA_ColWise) # as factor
                                        feats <- split(fact,fact) # splits
                                        feats <- sapply(feats,function(x) names(x))
                                        feats <- unlist(mapply(function(z,name) { 
                                                name <- as.numeric(name)
                                                if (name >= missingCols) {
                                                        z
                                                }
                                        },feats,names(feats)))
                                        data <- data[,-which(names(data) %in% c(feats))]
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"auto-cleaned wells ",paste(wells,collapse = ","),"and features",paste(feats,collapse = ","),"(",comments,")")))
                                        isolate(values$data <- data)
                                } else if (id == "auto-clean-outliers") {
                                        isolate(order <- values$order)
                                        threshold <- as.numeric(info$modalinfo[[3]])
                                        cleanOuts <- mapply(function(sds,feature) {
                                                if (sds >= threshold) {
                                                        isolate(x <- data[[feature]])
                                                        min <- min(x,na.rm = T)
                                                        max <- max(x,na.rm = T)
                                                        mean <- mean(x,na.rm = T)
                                                        sd <- sd(x,na.rm = T)
                                                        outliers <- sapply(x, function(no) {
                                                                if (!is.na(no)) {
                                                                        dist <- abs((no - mean)/sd)
                                                                        if (dist >= threshold) {
                                                                                no  
                                                                        }
                                                                }
                                                        })
                                                        outliers <- unlist(outliers)
                                                        
                                                        isolate(values$logs <- c(values$logs, paste(user," @ ",format(Sys.time(), "%a %b %d %X %Y"), " auto-cleaned outlier '",paste(outliers[!is.na(outliers)],collapse = ","),"' for ",feature," by NA ","(",comments,")",sep = ""))) #,"between",low,"and",high
                                                        x[x %in% outliers] <- NA # replace outlier by NA?
                                                        isolate(values$data[[feature]] <- x)
                                                }
                                        }, order, names(order))
                                        
                                        # revise values$order
                                        isolate(calculateOrder(values$data))
                                }
                                else if(id=="binning"){
                                        isolate({ binTableData <- hot.to.df(input$hotable1) })
                                        # Numerical feature name
                                        featureName <- info$data$featureName
                                        # New column Name
                                        newColumnName <- info$modalinfo[[2]]
                                        # No of bin
                                        nrow <- nrow(binTableData)
                                        # Get Numerical feature's data
                                        newCategoryData <- data[[featureName]]
                                        
                                        # Apply binning on numerical data
                                        for(i in 1:nrow){
                                                lbound <- binTableData[i,2]
                                                ubound <- binTableData[i,3]
                                                newCategoryData[newCategoryData < ubound & newCategoryData>=lbound ] <- binTableData[i,1]
                                        }
                                        # Add new column
                                        isolate({
                                                values$data[newColumnName]  <- newCategoryData
                                        })
                                }else if (id == "nearZeroVariancePercent") {
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"removing feature",paste(toClean,collapse = ","),"(",comments,")")))
                                        data <- data[,-which(names(data) %in% c(toClean))]
                                        isolate(values$data <- data)
                                }else if (id == "nearZeroVarianceFreq") {
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"removing feature",paste(toClean,collapse = ","),"(",comments,")")))
                                        data <- data[,-which(names(data) %in% c(toClean))]
                                        isolate(values$data <- data)
                                }else if (id == "rankingAndImportance") {
                                        isolate(values$logs <- c(values$logs, paste(user,"@",format(Sys.time(), "%a %b %d %X %Y"),"removing feature",paste(toClean,collapse = ","),"(",comments,")")))
                                        data <- data[,-which(names(data) %in% c(toClean))]
                                        isolate(values$data <- data)
                                }
                        }
                }
        })
        
        observe({
                if (input[["rearrange-outliers"]][1] > 0) {
                        isolate(data <- values$data)
                        calculateOrder(data)
                }
        })
        
        output$clean_row <- renderBarChart({
                data <- values$data
                isolate(ID <- input$id)
                NA_RowWise <- apply(data, 1, function(z) sum(is.na(z)))
                NA_RowWise <- round((NA_RowWise/ncol(data))*PERCENT,ROUND)
                
                ids <- data[[ID]]
                df <- data.frame(x=ids,y=NA_RowWise) # data frame
                fact <- factor(df$y) # as factor
                grps <- split(df,fact)
                grps <- sapply(grps,function(z) z$x)
                
                counts <- sapply(grps,function(z) length(z))
                xnames <- as.numeric(names(counts))
                updateNumericInput(session, "rows-threshold",max = max(xnames), min = min(xnames), value = max(xnames))
                return(list(x=xnames,y=as.numeric(counts),tooltip=tooltip_rows,xlabel="(% of missing features)",ylabel="(frequency of wells)",evt="clickdatapoint",d=grps, type="clean_row")) #      
        })
        
        output$clean_col <- renderBarChart({
                data <- values$data
                
                NA_ColWise <- apply(data, 2, function(z) sum(is.na(z)))
                NA_ColWise <- floor((NA_ColWise/nrow(data))*PERCENT)
                
                fact <- factor(NA_ColWise) # as factor
                grps <- split(fact,fact) # splits
                grps <- sapply(grps,function(x) names(x))
                tab <- table(NA_ColWise)
                xnames <- as.numeric(names(tab))
                updateNumericInput(session, "cols-threshold",max = max(xnames), min = min(xnames), value = max(xnames))
                return(list(x=xnames,y=as.numeric(tab),tooltip=tooltip_cols,xlabel="(% of missing wells)",ylabel="(frequency of features)",evt="clickdatapoint",d=grps, type='clean_col')) #     
        })
        
        applyCategoricalTransform <- observe({
                if (!is.null(input$type)) {
                        updateButtonInput(session, "transform-categories",feature = input$type)
                }
        })
        
        transformCats <- observe({
                obj <- input[["transform-categories"]]
                if (length(obj) > 1) {
                        #       browser()
                        if (identical("category with mean target value",obj$id)) {
                                # do category replacements on values$data
                                isolate(data <- values$data)
                                isolate(metric <- input$metrics[1])
                                result <- category_labeling(data,metric)
                                isolate(values$categoryMap <- result$map)
                                isolate(values$data <- result$data)
                        } else if (identical("category with dummy predictors",obj$id)) {
                                
                        }
                }
        })
        
        output$transforming_categories <- renderUI({
                if (is.null(values$categoryMap)) {
                        return(fluidRow(
                                column(12, h4("Click Apply!"))
                        ))
                }
                
                data <- values$categoryMap
                isolate(ID <- input$id)
                
                for (i in names(data)) {
                        # skip ID
                        if (i == ID)
                                next
                        
                        local({
                                feature <- i
                                plot_name <- paste("trans_cate_",feature,sep = "")
                                output[[plot_name]] <- renderHorizontalBarChart({
                                        p <- data[[feature]]
                                        d <- data.frame(x=names(p),y=as.numeric(p))
                                        return(d[order(-d$y),]) #          
                                })
                        })
                }
                
                plot_output_list <- lapply(names(data), function(name) { #categoricals(DATA())
                        p <- data[[name]]
                        r <- length(p)
                        plot_name <- paste("trans_cate_",name,sep = "")
                        
                        h = 150
                        if (r > 35) {
                                h = 500
                        } else if (r > 25) {
                                h = 400
                        } else if (r > 20) {
                                h = 300
                        } else if (r > 10) {
                                h = 200
                        } else if (r < 5) {
                                h = 70
                        }
                        
                        wellPanel(
                                h4(name),
                                fluidRow(
                                        column(12,nvd3ChartOutput(plot_name,"nvd3-categories",height = h))
                                ))
                })
                
                # Convert the list to a tagList - this is necessary for the list of items
                # to display properly.
                do.call(tagList, plot_output_list)
        })
        
        correlation <- reactive({
                d <- category_labeling(values$data,input$metrics)$data
                data <- numericals(category_labeling(values$data,input$metrics)$data)
                data[is.na(data)] <- 0
                cor(data,use = "everything",method = "pearson")
        })
        
        # shows pairwise correlation for numerical features
        output$COR <- renderText({
                data <- numericals(values$data)
                isolate({
                        localX <- input$x
                        feature <- input$y
                })
                input$goButton
                #     print(paste(localX,feature))
                #     round(cor(data[[localX]],data[[feature]],use="complete"), 3)
                cor(data[[localX]],data[[feature]],use="complete")
        })
        
        output$radio <- renderUI({
                isolate({
                        d <- values$data
                })
                xType <- class(d[[input$x]])
                yType <- class(d[[input$y]])
                selected = ''
                if(xType %in% c('integer', 'numeric') && yType %in% c('integer', 'numeric'))
                        selected = 'numericals'
                items <- c('NumVsNum' = 'numericals','CatVsCat' = 'categoricals', 'NumVsCat' = 'numvscat', 'CatVsNum' = 'catvsnum')
                radioButtons('relation', '', items, selected= selected)
        })
        
        
        # renders scatter chart
        output$scatter <- renderScatterChart({
                input$goButton
                data <- numericals(values$data)
                names <- names(data)
                isolate({
                        localX <- input$x
                        feature <- input$y
                        ID <-  input$id
                        indN <- ID %in% names
                })
                if(localX != feature){
                        d <- data[, names(data) %in% c(localX,feature)]
                        d <- subset(d, !is.na(d[[localX]]))
                        d <- subset(d, !is.na(d[[feature]]))
                        xCol <- d[[localX]]
                        xCol <- sort(xCol, decreasing = F)
                        yCol <- d[[feature]]
                }else{
                        xCol <- subset(data, !is.na(data[[localX]]))[, c(localX)]
                        xCol <- sort(xCol, decreasing = F)
                        yCol <- subset(data, !is.na(data[[feature]]))[, c(feature)]
                        yCol <- sort(yCol, decreasing = F)
                }
                d <- data.frame(xCol, yCol)
                colnames(d) <- c(localX, feature)
                return(list(xcol=xCol, ycol=yCol, xvar=localX, yvar=feature))
        })
        
        
        #observes when exporting data
        observe({
                if(is.null(input$export))
                        return()
                data <- input$export
                features <- unlist(data$features, use.names = FALSE)
                isolate({
                        d <- values$data
                        ID <- input$id
                })
                wb <- createWorkbook()
                cs3 <- CellStyle(wb) + Font(wb, color="blue") + Border() # header
                switch(data$type, 
                       scatter = {
                               d <- d[, features]
                               for(i in names(d)){
                                       d <- subset(d, !is.na(d[[i]]))
                               }
                               fname <- paste(features, collapse='-')
                       },
                       clean_row = {
                               fname = 'clean_row'
                               NA_RowWise <- apply(d, 1, function(z) sum(is.na(z)))
                               ids <- d[[ID]]
                               d <- data.frame(x=ids,y=NA_RowWise)
                               f <- factor(d$y)
                               s <- split(d,factor(d$y))
                               s <- sapply(s,function(z) length(z$x))
                               d <- data.frame(x = as.numeric(names(s)), y = as.numeric(s))
                       },
                       clean_col = {
                               fname = 'clean_col'
                               NA_ColWise <- apply(d, 2, function(z) sum(is.na(z)))
                               p <- table(NA_ColWise)
                               d <- data.frame(x = as.numeric(names(p)), y = as.numeric(p))
                       },
                       histogram = {
                               fname = paste(features,'hist', sep='-')
                               t <- subset(d, !is.na(d[[features]]))[c(features)]
                               t <- t[[features]]
                               ux <- unique(t)
                               max <- length(ux)
                               slid_name <- paste("slider_",features,sep = '')
                               brks <- seq(min(t), max(t), l=input[[slid_name]] + 1)
                               if (input[[slid_name]] == max)
                                       brks <- ux
                               h <- hist(t, breaks=brks, plot = F)
                               b <- h$breaks
                               breaks <- b[1:length(b)-1]
                               counts <- h$counts
                               d <- data.frame(x = breaks, y = counts)
                       },
                       box = {
                               fname = paste(features,'box', sep='-')
                               t <- subset(d, !is.na(d[[features]]))[c(features)]
                               t <- t[[features]]
                               d <- data.frame(x = t)
                       }
                )
                file <- paste('/home/sayantani/Downloads/', paste(fname,'xlsx', sep='.'), sep='')
                sheet <- createSheet(wb, sheetName=data$type)
                addDataFrame(d, sheet,row.names=FALSE,colnamesStyle=cs3)
                saveWorkbook(wb, file)
        })
        
        
        # The pair-wise correlation matrix
        output$corrPlot_static <- renderPlot({
                val <- correlation()
                if(is.null(val)) return(NULL)
                corrplot(val,method="ellipse")
        })
        
        # The pair-wise correlation matrix
        output$corrPlot <- renderD3Chart({
                d <- values$data
                data <- numericals(d)
                corr <- cor(data,use = "complete",method = "pearson")
                list(data=data,cor=corr)
        })
        
        output$pairwiseCOR <- renderText({
                point <- input[["pair-wise-collinear"]]
                if (is.null(point)) 
                        return(NULL)
                
                METRICS <- c(point$row,point$col)
                METRICS <- as.numeric(METRICS) + 1
                d <- numericals(values$data)
                names <- names(d)[METRICS]
                corr <- round(cor(d[,METRICS[1]],d[,METRICS[2]],use="complete"),2)
                paste("Cor is :", corr, collapse = " ")
        })
        
        output[["compare-features"]] <- renderLineChart({
                
                point <- input[["pair-wise-collinear"]]
                if (is.null(point) || point$cor < 0.90) # 
                        return(NULL)
                
                METRICS <- c(point$row,point$col)
                if (is.null(METRICS)) 
                        return(NULL)
                
                METRICS <- as.numeric(METRICS) + 1
                ID <- input$id
                d <- numericals(values$data)
                names <- names(d)[METRICS]
                
                missing <- sapply(names,function(x) sum(is.na(d[[x]])), simplify = T)
                last <- names(which.min(missing))
                d <- d[order(d[[last]]),]
                return(list(x=as.numeric(d[[ID]]),y=d[,c(METRICS)],xlabel="(# of wells)",tooltip=tooltip_pair,evt="xpairswise",d=missing)) #          ylabel=paste(METRICS,sep = ",")
        })
        
        output[["test-cor"]] <- renderScatterChart({
                point <- input[["pair-wise-collinear"]]
                if (is.null(point)) 
                        return(NULL)
                
                METRICS <- c(point$row,point$col)
                if (is.null(METRICS))
                        return(NULL)
                
                METRICS <- as.numeric(METRICS) + 1
                data <- numericals(values$data)
                names <- names(data)[METRICS]
                
                #     isolate({
                localX <- names[2]
                feature <- names[1]
                ID <- input$id
                #     })
                
                if(!is.null(feature) && !is.null(localX) && feature == localX){
                        temp1 <- data[, (names(data) %in% c(ID,feature))] 
                        temp2 <- data[, (names(data) %in% c(ID,feature))]
                        temp3 <- merge(temp1, temp2, by=ID)
                        data <- temp3[, !names(temp3) %in% c(ID)]
                        colnames(data)[1] <- feature
                        rm(ID, temp1, temp2, temp3)
                } else{
                        data <-  data[, (names(data) %in% names)]
                }
                isna <- unique(is.finite(data[[feature]]))
                if(length(isna) > 1){
                        data <- subset(data, !is.na(data[[feature]]))
                }
                isMetricNA <- unique(is.finite(data[[localX]]))
                if(length(isMetricNA) > 1){
                        data <- data[complete.cases(data[,c(localX)]),]
                }
                data <- data[order(data[[localX]]),]
                return(list(dt=data, metric=localX))
        })
        
        output$downloadData  <- downloadHandler(
                filename = function() { 
                        paste("data", '.csv', sep='') 
                },
                content = function(file) {
                        write.csv(values$data, file, row.names=FALSE)
                }
        )
        
        
        
        output$log <-renderPrint({
                cat(values$logs,sep="\n") #paste("@",Sys.getenv("LOGNAME"),"logged in",sep = " "),
        })
        
        output$downloadLog <- downloadHandler(
                filename = function() { 
                        paste('log.txt', sep='') 
                },
                content = function(logfile) {
                        write.table(values$logs, file=logfile,sep="\n",row.names=FALSE,col.names=FALSE)
                }
        )
        
        # Observe when "Transform" button clicked
        observe({
                # first time it will do nothing.
                if(is.null(input$transformData))
                        return() 
                
                # get the transformation methods that need to apply to data 
                isolate({
                        chooserInputId <- input$transformData$chooser
                        noOfTransformation <- input[[chooserInputId]][["right"]]
                        featureName <- input$transformData$feature
                })
                
                
                isolate(values$copy[[featureName]] <- values$data[[featureName]]) 
                appliedMethod <- c()
                
                tryCatch({
                        # apply each transformation method (i.e selected in the right side of the chhoser input) in data
                        for(method in noOfTransformation){      
                                switch(method,
                                       Log = {
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(data <- values$copy[[featureName]][!is.na(values$copy[[featureName]])])
                                               
                                               # Throw error for negetive or zero value without processing the log             
                                               if(min(data) <= 0){
                                                       throw("Log can not possible")
                                               }
                                               data <- log(data)               
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- data)
                                               
                                       },Scale ={             
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(scaleData <- values$copy)
                                               transf <- preProcess(data.frame(scaleData[[featureName]]),method="scale",na.remove=TRUE,k=5)
                                               temp <- predict(transf, newdata=data.frame(scaleData[[featureName]]))
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- temp[[names(temp)]])                
                                       },
                                       BoxCox ={ 
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(scaleData <- values$copy)
                                               transf <- preProcess(data.frame(scaleData[[featureName]]),method="BoxCox",na.remove=TRUE,k=5)
                                               temp <- predict(transf, newdata=data.frame(scaleData[[featureName]]))
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- temp[[names(temp)]])             
                                       },
                                       Square =  {
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(data <- values$copy[[featureName]][!is.na(values$copy[[featureName]])])             
                                               data <- data^2  
                                               data <- round(data,2)             
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- data)             
                                       },
                                       Sqrt = {
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(data <- values$copy[[featureName]][!is.na(values$copy[[featureName]])])
                                               data <- sqrt(data)
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- data)            
                                       },
                                       Sin = {
                                               appliedMethod[(length(appliedMethod)+1)] <- method
                                               isolate(data <- values$copy[[featureName]][!is.na(values$copy[[featureName]])])
                                               data <- sin(data)
                                               isolate(values$copy[[featureName]][!is.na(values$copy[[featureName]])] <- data)             
                                       }
                                )     
                        }
                        # Display the transformations applied in data in.
                        output[[paste("txt_",featureName,sep="")]] <- renderText({
                                print(paste(appliedMethod,collapse=" -> "))
                        })
                        
                        
                },error=function(e){
                        mesg <- paste(appliedMethod,collapse=" -> ")
                        output[[paste("txt_",featureName,sep="")]] <- renderText({       
                                print(paste(mesg, "not posible for",featureName,"\nTry other methods",sep=" "))
                        })
                        return(NULL)
                },warning=function(cond) {
                        mesg <- paste(appliedMethod,collapse=" -> ")
                        output[[paste("txt_",featureName,sep="")]] <- renderText({       
                                print(paste(mesg,"not posible for",featureName,"\nTry other methods",sep=" "))
                        })
                        return(NULL)
                })
                
                
        })
        
        # Observe when "Save" button clicked
        observe({  
                if(is.null(input$saveTransformedData))
                        return() 
                
                isolate({    
                        featureName <- input$saveTransformedData$feature
                        tempData <- values$copy[[featureName]]
                        values$data[[featureName]] <- tempData
                }) 
                
                # after save clear the text that displayed the transformation methods
                output[[paste("txt_",featureName,sep="")]] <- renderText({
                        print("")
                })
                
        })
        
        #observe when we change chart type either line-chart or bar-chart
        observe({
                input$chartType
        })
        
        #observe when we click the "Reset" button.
        observe({  
                if(is.null(input$resetData))
                        return() 
                isolate({
                        data <- input$resetData   
                        featureName <- data$feature
                        tempData <- values$data[[featureName]]
                        values$copy[[featureName]] <- tempData
                }) 
                
                # after save clear the text that displayed the transformation methods
                output[[paste("txt_",featureName,sep="")]] <- renderText({
                        print("")
                })
        })
        
        
        # no of bars to be shown in barchart for actual and transformed chart
        noOfbars <- 10
        
        # Render Ui for "Numerical" subtab of "Transformation" tab 
        # Here we are showing the bar-chart and line-chart of actual data 
        # as well as charts of transformed data by using some methods like 
        # "Log","BoxCox" etc.
        output$numericalUpdate <- renderUI({   
                # we are showing first six feature due to performance issue
                
                uiplotoutput <- lapply(input$numericFeatures, function(name) {    
                        # id used for different html elements
                        feature <- gsub('([[:punct:]])|\\s+','_',name)
                        chooser_input <- paste("choserInput-",feature,sep="")
                        trans_actionButton <- paste("btn-trans-",feature,sep="")
                        save_actionButton <- paste("btn-save-",feature,sep="")
                        transform_chart_id <- paste("chart-trans-",feature,sep="")
                        actual_chart_id <- paste("chart-actual-",feature,sep="")
                        methods_display <- paste("txt_",name,sep="")
                        actual_slider <- paste("slider-actual-",feature,sep="")
                        trans_slider <- paste("slider-trans-",feature,sep="")
                        actual_bar_chart_id <- paste("chart-bar-actual-",feature,sep="")
                        transform_bar_chart_id <- paste("chart-bar-trans-",feature,sep="")
                        reset_btn <- paste("btn-reset-",feature,sep="")
                        
                        
                        # Plot linechart 
                        if(is.null(input$chartType) || input$chartType == "Linechart"){    
                                output[[transform_chart_id]] <- renderLineChart_1({              
                                        transformedData <- values$copy[!is.na(values$copy[[name]]),]
                                        transformedData <- transformedData[order(transformedData[[name]],decreasing=FALSE),]
                                        return(list(data=transformedData,xLabel="Observation",yLabel=name))   
                                })
                                
                                output[[actual_chart_id]] <- renderLineChart_1({           
                                        actualData <- values$data[!is.na(values$data[[name]]),]
                                        actualData <- actualData[order(actualData[[name]],decreasing=FALSE),]        
                                        return(list(data=actualData,xLabel="Observation",yLabel=name))   
                                })
                        }
                        # Plot barchart
                        else if(input$chartType == "Barchart"){ 
                                output[[actual_bar_chart_id]] <- renderBarChart({
                                        x <- values$data[[name]]
                                        t <- x[!is.na(x)]
                                        ux <- unique(t)
                                        max <- length(ux)
                                        
                                        brks <- seq(min(t),max(t),l=noOfbars)
                                        if (noOfbars == max)
                                                brks <- ux          
                                        
                                        h <- hist(t,plot = F,breaks=brks) # ux
                                        b <- h$breaks       
                                        breaks <- b[1:length(b)-1]       
                                        counts <- h$counts        
                                        return(list(x=breaks,y=h$counts,tooltip=tooltip_trans,ylabel="(frequency)",d=b,max=max(t)))
                                })
                                
                                output[[transform_bar_chart_id]] <- renderBarChart({
                                        x <- values$copy[[name]]
                                        t <- x[!is.na(x)]
                                        ux <- unique(t)
                                        max <- length(ux)        
                                        brks <- seq(min(t),max(t),l=noOfbars)
                                        if (noOfbars == max)
                                                brks <- ux          
                                        
                                        h <- hist(t,plot = F,breaks=brks) # ux
                                        b <- h$breaks       
                                        breaks <- b[1:length(b)-1]     
                                        counts <- h$counts        
                                        return(list(x=breaks,y=counts,tooltip=tooltip_trans,ylabel="(frequency)",d=b,max=max(t))) #  xlabel=feature,
                                })
                                
                                
                        }
                        
                        fluidRow(
                                column(4,wellPanel(
                                        conditionalPanel(
                                                condition = "input.chartType === 'Linechart'",
                                                nvd3ChartOutput(actual_chart_id,"nvd3-linechart",height = 200)
                                        ),
                                        conditionalPanel(
                                                condition = "input.chartType === 'Barchart'",         
                                                nvd3ChartOutput(actual_bar_chart_id,"nvd3-histogram",height = 200)
                                        )
                                        
                                        
                                )),      
                                column(4,wellPanel(h3(name),                 
                                                   chooserInput(chooser_input, "Available method", "Selected method",
                                                                c("Log","Scale","BoxCox","Square","Sqrt","Sin"), c(), size = 5, multiple = TRUE
                                                   ),      
                                                   verbatimTextOutput(methods_display),
                                                   tags$input(id=trans_actionButton,type="button", 
                                                              class="btn transform action-button btn-small btn-primary", value="Transform",
                                                              onClick = HTML(paste("transformHandler(\"",name,"\",\"",chooser_input,"\")",sep=""))             
                                                   ),
                                                   tags$input(id=save_actionButton,type="button", 
                                                              class="btn save action-button btn-small btn-primary", value="Save",
                                                              onClick = HTML(paste("saveTransformHandler(\"",name,"\")",sep=""))
                                                              
                                                   ),
                                                   tags$input(id=reset_btn,type="button", 
                                                              class="btn reset action-button btn-small btn-primary", value="Reset",
                                                              onClick = HTML(paste("resetHandler(\"",name,"\",\"",chooser_input,"\")",sep=""))   
                                                   ))),
                                column(4,wellPanel(conditionalPanel(
                                        condition = "input.chartType === 'Linechart'",
                                        nvd3ChartOutput(transform_chart_id,"nvd3-linechart",height = 200)
                                ),
                                conditionalPanel(
                                        condition = "input.chartType === 'Barchart'",            
                                        nvd3ChartOutput(transform_bar_chart_id,"nvd3-histogram",height = 200)
                                )
                                ))
                        )
                })
                do.call(tagList, uiplotoutput)
        })
        
#         #################################################################################################
#         #########################              Numerical Binning           ##############################
#         #################################################################################################
#         
#         #  Observe when changes occur in the feature name or no of bin
#         observe({
#                 if(input$numericbin_feature == "Select" || input$noOfBin < 2)
#                         return (NULL);   
#                 
#                 # get the no of bin
#                 bins <- as.numeric(input$noOfBin)
#                 # get the no of feature
#                 featureName <- input$numericbin_feature
#                 
#                 # get the feature's data
#                 isolate ({ tempData <- values$copy[[featureName]] })
#                 
#                 # remove na from data
#                 tempData <-  tempData[!is.na(tempData)]  
#                 min_tempData <- min(tempData)
#                 max_tempData <- max(tempData)
#                 
#                 # calculate the class interval based on no of bins
#                 classInterval <- (max_tempData - min_tempData)/bins
#                 
#                 # get the lower bound and upper bound for 1st bin
#                 lbound <- min_tempData
#                 ubound <- (min_tempData +classInterval)  
#                 
#                 
#                 catnames <- c()
#                 lBounds <- c()
#                 uBounds <- c()  
#                 
#                 # create data for bin table with category name, lowerbound and upperbound
#                 for(i in 1:bins){
#                         catnames[i] <- paste("cat_",i,sep="")
#                         lBounds[i] <- lbound
#                         uBounds[i] <- ubound     
#                         lbound <- ubound
#                         ubound <- lbound + classInterval
#                 }
#                 
#                 # create data.frame having column "CatName","Lower Bound",""
#                 table <- data.frame(catnames,lBounds,uBounds)
#                 names(table) <- c("Category","Lower Bound","Upper Bound")
#                 
#                 # render table with data
#                 output$hotable1 <- renderHotable({
#                         table
#                 },readOnly = FALSE) 
#         })
#         
#         # Observe change to be happen in bin table(while edit the value of lower or upper bound or categorical names)
#         observe({
#                 if(is.null(input$hotable1))
#                         return (NULL)  
#                 
#                 # get data from bin table
#                 binsdata <- hot.to.df(input$hotable1)  
#                 if(!is.null(binsdata)){ 
#                         
#                         isolate({ 
#                                 featureName <- input$numericbin_feature 
#                                 tempData <- values$copy[[featureName]] 
#                         })
#                         tempData <-  tempData[!is.na(tempData)]
#                         catnames <- binsdata[,1] # 1st column is "Category"
#                         counts<-c()
#                         nrow <- nrow(binsdata)    
#                         
#                         # prepairing data for bar chart
#                         for(i in 1:nrow){    
#                                 lbound <- binsdata[i,2] # 2nd column is "Lower Bound"
#                                 ubound <- binsdata[i,3] # 3rd column is "Upper Bound"
#                                 if(i != nrow)
#                                         counts[i] <- length(tempData[tempData < ubound & tempData >= lbound])
#                                 else
#                                         counts[i] <- length(tempData[tempData <= ubound & tempData >= lbound])  
#                         }
#                         
#                         # Render numerical bin chart
#                         output$numericalBinChart <- renderBarChart({     
#                                 return(list(x=catnames,y=counts,tooltip='',ylabel="(frequency)",d=catnames,max=max(counts)))
#                         })
#                 }
#         })
#         
#         # observe click event of "Apply" button in "Numerical" Bin tab
#         observe({  
#                 
#                 if(input$btnApplyBins>0){
#                         isolate({ featureName <- input$numericbin_feature })
#                         newColumnName <- paste(featureName,"_categorical",sep="")
#                         isolate({ binTableData <- hot.to.df(input$hotable1)})
#                         title <- paste("Do you want to apply binning of numerical feature ","\"",featureName,"\"",sep="")
#                         datapoint <- list(info=title,featureID="binning",featureName=featureName,newColumnName=newColumnName,randomNo=runif(1, 1, 100),binData=binTableData)
#                         toggleModal(session, "confirm_applyBinning",datapoint)
#                 }
#                 
#         })
        
        # Near Zero variance Module Unique Percentage
        # This output fuction will be called everytime the user wants to 
        # analyse the data for near zero variance
        output$nearZeroVariancePercent <- renderBarChart({
                Data <- values$data 
                featureNames <- names(Data)   
                variancePercent <- nearZeroVar(Data,saveMetrics = TRUE)
                featurePercentage = variancePercent$percentUnique
                return(list(x=featureNames,y=featurePercentage,evt="clickdatapoint",d=featureNames,showVerticalXaLabel=TRUE,ylabel=Y_LABEL_NZV,xlabel=X_LABEL_NZV))
        })
        
        
        
        # Near Zero variance Module Frequency Ratio
        # This output fuction will be called everytime the user wants to 
        # analyse the data for near zero variance
        output$nearZeroVarianceFreq <- renderBarChart({
                Data <- values$data 
                featureNames <- names(Data)   
                variancePercent <- nearZeroVar(Data,saveMetrics = TRUE)
                featurePercentage = variancePercent$freqRatio
                return(list(x=featureNames,y=featurePercentage,evt="clickdatapoint",d=featureNames,showVerticalXaLabel=TRUE,ylabel=Y_LABEL_FR_NZV,xlabel=X_LABEL_NZV))
        })
        
        # Ranking & Importance  Module 
        # This output fuction will be called everytime the user wants to 
        # analyse the data for ranking
        
        output$rankingAndImportance <- renderBarChart({
                data = values$data
                inputVariable = input$rankingFeature
                rankingMethod = input$rankingMethod
                responseVariable = values$data[[inputVariable]]
                featureNames <- colnames(numericals(values$data))
                numericalData = numericals(values$data)
                filteredNames <- which(featureNames %in% c(inputVariable))
                numericalData = numericalData[-c(filteredNames)]  
                
                if(rankingMethod == "Information Gain"){
                        ranks= information.gain(responseVariable~.,numericalData)
                }else if(rankingMethod == "Gain Ratio"){
                        ranks= gain.ratio(responseVariable~.,numericalData)
                }else{
                        ranks= symmetrical.uncertainty(responseVariable~.,numericalData)
                }
                
                return(list(x=names(numericalData),y=round(ranks,3),evt="clickdatapoint",tooltip=tooltip_rank, d=featureNames,showVerticalXaLabel=TRUE,showDecimal=TRUE, ylabel=Y_LABEL_Ranking,xlabel=X_LABEL_NZV))
                
        })
        
        
        
        
        
        #------------------Keep this code for later use-----------------
        
        # Given a vector or list, drop all the NULL items in it
        # dropNulls <- function(x) {
        #   x[!vapply(x, is.null, FUN.VALUE=logical(1))]
        # }
})