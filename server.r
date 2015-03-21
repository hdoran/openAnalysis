load('men.Rdata')
load('women.Rdata')
load('men4549.Rdata')
load('women4549.Rdata')
load('men4044.Rdata')
load('women4044.Rdata')

shinyServer(function(input, output, session) {
  
    ###################################################################
	#							server.r code for tab 1
    ###################################################################
  
  ### This function can be used to read in data in real time
  ### but downloading hits the server a lot and can be very slow
  #filedata <- reactive({
   # infile <- input$Grab_CrossFit
    #if (is.null(infile)) return(NULL)    
    #xfit(gender = input$gender, numPages = input$pages) 
    #})
    
    ### Instead, just load a datafile already prepared
    filedata <- reactive({
    switch(input$dataset,
           "Men Overall" = men,
           "Women Overall" = women,
           "Men 40-44" = men4044,
           "Women 40-44" = women4044,
           "Men 45-49" = men4549,
           "Women 45-49" = women4549)
  })
  

  output$dependent <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #items=names(df)
	items=names(df)[c(3:9)] ### So that only numeric variables appear in drop down
    names(items)=items
    selectInput("dependent","Select dependent variable from:",items, selectize = FALSE)
  })


  output$independents <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #items=names(df)
	items=names(df)[2:9] ### So that only numeric variables appear in drop down
    names(items)=items
    selectInput("independents","Select independent variable(s) from:",items,multiple=TRUE, selectize = FALSE)
  })

  
	output$percVar <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    #items=names(df)
	items=names(df)[5:9] ### So that only numeric variables appear in drop down
    names(items)=items
    selectInput("percVar","Choose a WOD to compute your percentile:",items, selectize = FALSE)
  })
  
  
    output$indRegion <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)[c(5:9)] ### So that only numeric variables appear in drop down
    names(items)=items
    selectInput("indRegion","Choose WOD to analyze:",items, selectize = FALSE)
  })
  
    
	output$view <- renderTable({
    subset(filedata(), regional_rank <= input$obs)
  })
    
    
	observe(updateSelectizeInput(session, "name", choices = filedata()$Competitor, server = TRUE))

    
    output$myScores <- renderPrint({
	df <- filedata()
    subset(df, Competitor==input$name)
 })  
 

  output$regression <- renderTable({
    input$regression
    isolate({   
      df <- filedata()
      if (is.null(df)) return(NULL)
      fmla <- as.formula(paste(input$dependent," ~ ",paste(input$independents,collapse="+")))
      summary(lm(fmla,data=df))
    })   
    })
    

  output$correlation <- renderTable({
    input$correlation
    isolate({   
      df <- filedata()
      if (is.null(df)) return(NULL)
	  vars <- c(input$dependent, input$independents)
      cor(df[,vars], use = 'complete')
    })   
    })
	

  
    ###################################################################
	#							server.r code for Percentile Tab
    ###################################################################

	
	### Conditional percentile
	output$percentile <- renderTable({
    #input$percentile
	#isolate({   
      df <- filedata()
      if (is.null(df)) return(NULL)	  
      fn <- ecdf(df[,input$percVar]) 
      worldRank <- fn(input$myScore)
	  
	  condPercentile <- numeric(17)
	  for(i in 1:17){
		tmp <- subset(df, region == levels(df$region)[i])
		fn <- ecdf(tmp[,input$percVar]) 
		condPercentile[i] <- fn(input$myScore)		
		}
	  
	 pResult <- round(c(worldRank, condPercentile) * 100)
	 pResult <- ifelse(pResult > 99, 99,
		ifelse(pResult < 1, 1, pResult))
	 data.frame(Region = c('World', levels(df$region)), Percentile = pResult)
	  
    #})   
    })
	
	output$histogram <- renderPlot({
	df <- filedata()
    if (is.null(df)) return(NULL)
	x    <- df[,input$percVar]  
    bins <- seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out = input$bins + 1)
	title <- paste('Histogram of', input$percVar, sep =' ')
	hist(x, breaks = bins, col = 'darkgray', border = 'white', main = title, xlab = input$dependent)
  })
  
  
	output$percentilePlot <- renderPlot({	
		#input$percentile
		#isolate({   
			df <- filedata()
			if (is.null(df)) return(NULL)	  
			fn <- ecdf(df[,input$percVar]) 
			yourRank <- round(fn(input$myScore) * 100)
			yourRank <- ifelse(yourRank > 99, 99,
			ifelse(yourRank < 1, 1, yourRank))
			remainder <- 99 - yourRank
			pRank <- c(yourRank, remainder)
			spacer <- c(NA,NA)
			plotDat <- cbind(pRank, spacer)
			barplot(plotDat, names = c('', ''), col=c("green", "red"), width=c(.2, 1),axis.lty=0, ylim = c(0,100))
			#points(.6, yourRank, pch=19)
			claim <- paste('You are scoring at the ', '\n', yourRank, 'th percentile', sep ='')
			if(yourRank < 50){ 
				text(.6, yourRank + 5, claim)
				} else { 
				text(.6, yourRank - 5, claim)
			}
		#})
	}) 	
  
  
    
  
    
###################################################################
#							Regional Analysis tab
################################################################### 
 

 	output$dotplot <- renderPlot({
	df <- filedata()
    if (is.null(df)) return(NULL)
	title <- paste('Average Score by Region of ', input$indRegion, sep =' ')
	xlab <- paste('Score by Region of ', input$indRegion, sep =' ')
	tmp <- subset(df, regional_rank <= input$numRegion)
	x    <- tmp[,input$indRegion]
	r1 <- with(tmp, tapply(x, region, mean, na.rm = TRUE))
	dotplot(~ sort(r1), col = 'red', main = title, xlab = xlab)
  })
 
 
  	output$bwplot <- renderPlot({
	df <- filedata()
    if (is.null(df)) return(NULL)
	title <- paste('Score Distribution by Region of ', input$indRegion, sep =' ')
	xlab <- paste('Score by Region of ', input$indRegion, sep =' ')
	tmp <- subset(df, regional_rank <= input$numRegion)
	x    <- tmp[,input$indRegion]
	bwplot(reorder(region, x, median) ~ x, data = tmp, main = title, xlab = xlab)
  })
  
  
  
###################################################################
#							Strength and Weakness tab
################################################################### 
  
    output$indCompare <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)[c(5:9)] ### So that only numeric variables appear in drop down
    names(items)=items
    selectInput("indCompare","Find athletes who scored similar to me on this WOD:",items, selectize = FALSE)
  })

	output$depCompare <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    items=names(df)[c(5:9)] ### So that only numeric variables appear in drop down
    drop <- which(items== input$indCompare)
    items=items[-drop] ### I do this so that the WOD chosen as indCompare is dropped from this list
    names(items)=items
    selectInput("depCompare","How did athletes like me on the WOD chosen above score on this WOD:",items, selectize = FALSE)
  })  
  
    output$comparison <- renderTable({
    input$showCompare
    isolate({   
		df <- filedata()
		if (is.null(df)) return(NULL)
		num <- nrow(df)
		tmp <- df[sample(1:nrow(df), num, replace = FALSE),]
		aa <- abs(input$swScore - tmp[,input$indCompare])
		tmp2 <- tmp[order(aa) , ]
		result <- round(mean(tmp2[1:input$numCompare, input$depCompare], na.rm = TRUE))
		data.frame(Your.Score = input$swScore, 'Average' = result)
	  })   
    })

    output$compareGroup <- renderTable({
    input$showCompare
    isolate({   
		df <- filedata()
		if (is.null(df)) return(NULL)
		num <- nrow(df)
		tmp <- df[sample(1:nrow(df), num, replace = FALSE),]
		aa <- abs(input$swScore - tmp[,input$indCompare])
		tmp2 <- tmp[order(aa) , ]
		tmp2[1:10,]
	  })   
    })
    
    
###################################################################
#							Report Card tab
################################################################### 
 
 
  output$region <- renderUI({
    df <- filedata()
    if (is.null(df)) return(NULL)
    region=levels(df$region)
    names(region)=region
    selectInput("region","Choose Region:",region, selectize = FALSE)
  })
  
  observe(updateSelectizeInput(session, "RCname", choices = filedata()$Competitor, server = TRUE))
  
	reportCard <- reactive({
    	input$RC
    	 isolate({   
      		df <- filedata()
      		vars <- c('score15.1', 'score15.1A', 'score15.2', 'score15.3', 'score15.4')
      		if (is.null(df)) return(NULL)
      		tmp <- subset(df, Competitor == input$RCname & region == input$region) 
			r1 <- apply(df[, vars], 2, mean, na.rm = TRUE)
			tmp2 <- subset(df, region == input$region)
			rr <- apply(tmp2[, vars], 2, mean, na.rm = TRUE)
			r2 <- tmp[, vars]
			r3 <- rbind(r1, rr, r2)
			names(r3) <- c('15.1', '15.1A', '15.2', '15.3', '15.4')
			rownames(r3) <- c('World Average', paste(input$region, 'Average'), tmp$Competitor)
			r3
      	})
    })
    
      
	#output$worldPrPlot <- renderPlot({
   	#input$RC 
   	#isolate({
	#df <- filedata()
	#if (is.null(df)) return(NULL)
	#fn <- ecdf(df[,'world_rank']) 
	#tmp <- subset(df, Competitor == input$RCname & region == input$region) 
	#yourRank <- round((1 - fn(tmp$world_rank)) * 100) # here it is 1-fn because lower ranks are better
	#yourRank <- ifelse(yourRank > 99, 99,
	#	ifelse(yourRank < 1, 1, yourRank))
#		remainder <- 99 - yourRank
#		pRank <- c(yourRank, remainder)
#		spacer <- c(NA,NA)
#		plotDat <- cbind(pRank, spacer)
#		barplot(plotDat, names = c(tmp$Competitor, ''), col=c("green", "red"), main = 'Your Overall World Percentile', width=c(.2, 1),axis.lty=0, ylim = c(0,100))
#		claim <- paste('You are scoring at the ', '\n', yourRank, 'th percentile in the world overall', sep ='')
#		if(yourRank < 50){ 
#			text(.6, yourRank + 5, claim)
#			} else { 
#			text(.6, yourRank - 5, claim)
#		}
#	})
#})

 
   	output$barplot1 <- renderPlot({
   	input$RC
	r3 <- reportCard()
	aa <- barplot(t(r3)[1,], beside = TRUE, col = c('blue', 'green', 'red'), ylim = c(0, max(r3[,1], na.rm=TRUE)+100), main = 'Your Performance on Open 15.1') 
	xx <- r3[,1]
	text(cex= 1.5, x= aa, y= xx + par("cxy")[2]/2 + 10, round(xx,2), xpd=TRUE)
  })
  
    output$barplot2 <- renderPlot({
	input$RC
	r3 <- reportCard()
	aa <- barplot(t(r3)[2,], beside = TRUE, col = c('blue', 'green', 'red'), ylim = c(0, max(r3[,2], na.rm=TRUE)+100), main = 'Your Performance on Open 15.1A') 
	xx <- r3[,2]
	text(cex= 1.5, x= aa, y= xx + par("cxy")[2]/2 + 10, round(xx,2), xpd=TRUE)
  })
  
    output$barplot3 <- renderPlot({
	input$RC
	r3 <- reportCard()
	aa <- barplot(t(r3)[3,], beside = TRUE, col = c('blue', 'green', 'red'), ylim = c(0, max(r3[,3], na.rm=TRUE)+100), main = 'Your Performance on Open 15.2') 
	xx <- r3[,3]
	text(cex= 1.5, x= aa, y= xx + par("cxy")[2]/2 + 10, round(xx,2), xpd=TRUE)
  })

   output$barplot4 <- renderPlot({
	input$RC
	r3 <- reportCard()
	aa <- barplot(t(r3)[4,], beside = TRUE, col = c('blue', 'green', 'red'), ylim = c(0, max(r3[,4], na.rm=TRUE)+100), main = 'Your Performance on Open 15.3') 
	xx <- r3[,4]
	text(cex= 1.5, x= aa, y= xx + par("cxy")[2]/2 + 10, round(xx,2), xpd=TRUE)
  })
  
    output$barplot5 <- renderPlot({
	input$RC
	r3 <- reportCard()
	aa <- barplot(t(r3)[5,], beside = TRUE, col = c('blue', 'green', 'red'), ylim = c(0, max(r3[,5], na.rm=TRUE)+100), main = 'Your Performance on Open 15.4') 
	xx <- r3[,5]
	text(cex= 1.5, x= aa, y= xx + par("cxy")[2]/2 + 10, round(xx,2), xpd=TRUE)
  })
  

}) # end Program