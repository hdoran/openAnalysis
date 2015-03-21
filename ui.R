library(markdown)
library(lattice)

shinyUI(navbarPage("Toolbar",

tabPanel("Overview",
    sidebarLayout(
      sidebarPanel(      
    tags$head(includeScript("FB.js")),
    tags$head(includeScript("google-analytics.js")),
	includeHTML("overview.html")
			
    ), # end sidebar
		mainPanel(
		
 #includeHTML("methods.html")
   includeHTML("testing.html")
    
      )
    )
  ), # end tabpanel
  
  
tabPanel(span("Choose Data", style = "color:blue"),
    sidebarLayout(
      sidebarPanel(       
      selectInput("dataset", "Choose a CrossFit Open dataset:", 
                  choices = c("Men Overall", "Women Overall", "Men 40-44", 
                  "Women 40-44", "Men 45-49", "Women 45-49")),
                  
                  
	  numericInput("obs", "Number of Top Ranked Athletes by Region to Show:", 3),
	  
	    #textInput("name", label = 'Enter an athlete name and find your scores', 
        #value = "Enter Name Here")
        
        selectizeInput("name", choices = NULL, label = 'Athlete Name', multiple = FALSE)    

    	#h6('Grab Open Data'),
      	#actionButton("Grab_CrossFit", "Grab Data"),
    	#br(),
    	#actionButton("structure", "Structure")

		#numericInput("pages", "Number of Pages per Region", 1),
		#numericInput("gender", "Enter 1 to download men or 2 for women", 1)
		
		
    ), # end sidebar
		mainPanel(
        h4("Your Scores"),
        verbatimTextOutput("myScores"),
    	#tableOutput("myScores"),
		h4("Top Ranked Athletes by Region"),
      tableOutput("view")
      )
    )
  ), # end tabpanel


tabPanel(span("My Report Card", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
    h4('Enter your name in the box below'),
     # textInput("RCname", label = 'Athlete Name', 
     #   value = "Your name ..."),
         	 
	selectizeInput("RCname", choices = NULL, label = 'Athlete Name', multiple = FALSE),     
  
		 uiOutput("region"), 
         actionButton("RC", "Get My Report Card")
	 ),
	  mainPanel(
	h1('Athlete Report Card', align = "center"),
	includeHTML("reportCard.html"),
	br(),
	br(),
	br(),
	br(),
	#plotOutput('worldPrPlot'),
	plotOutput('barplot1'),
	plotOutput('barplot2'),
	plotOutput('barplot3'),
	plotOutput('barplot4'),
	plotOutput('barplot5')
	)
 ) 
), # end tabpanel for basic statistics 


tabPanel(span("Get My Percentile Rank", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
      h4("Percentile Rank"),
	  
	  helpText("What is a percentile? The percentile expresses the percentage of scores in the data that are the same or lower than your score. For example, a percentile of 85 means you have a score that is better than 85% of all other scores in the data."),
       
	  uiOutput("percVar"),
	  numericInput("myScore", "Enter your score", 100),
	  #actionButton("percentile", "Get My Percentile Rank"),
	       
	h4("Histogram of Dependent Variable"),
	  ### This is for the histogram
	  sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 25)	  
	 ),
	mainPanel(
		h3('Your percentile rank in the world and each region is:'),
		
		fluidRow(
		column(4,
			tableOutput('percentile')
			),
		column(8,
			h4('Your score is higher than the percentage of athletes', span("shaded green", style = "color:green"),	"but lower than the percentage of athletes", span("shaded red", style = "color:red")),
			plotOutput('percentilePlot')
		)
	),
		plotOutput('histogram') 
	  
	)
 ) 
), # end tabpanel for basic statistics 

tabPanel(span("My Strengths and Weaknesses", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
      h4("Use the results here to see how you perform relative to other athletes who performed like you on a baseline WOD."),
	  
	  helpText("For example, enter how you scored on 15.1 and see how other athletes with a score like yours performed on WOD 15.1A. You can use this information see if you have a relative strength or weakness."),
	  
      uiOutput("indCompare"), 
	  numericInput("swScore", "Enter your score on this WOD", 100),
	  uiOutput("depCompare"), 
	  numericInput("numCompare", "Enter the number of athletes to include in this analysis", 50),
	  actionButton("showCompare", "Show How I Compare")
	 ),
	mainPanel(
		h3('How to interpret the results below'),
		h5('The value "your score" is the score you entered for the first WOD.') ,
		h5('The value "average" is how other athletes with the same score as you on the first WOD scored on the second WOD.'),
		tableOutput('comparison'),
		h3('Some of the athletes similar to you on this WOD are:'),
		tableOutput('compareGroup')  
	)
 ) 
), # end tabpanel for strength and weakness


tabPanel(span("Basic Statistics", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
      h4("Basic Statistics"),
	  
	  helpText("These statistics can help us examine the relationship between certain Open WODs. You can compute a linear regression by choosing one dependent variable and then one (or many) independent variables. You can also compute a correlation matrix between variables by choosing one dependent and then one (or many) independent variables."),
	  uiOutput("dependent"),
	  uiOutput("independents"),
      actionButton("regression", "Linear regression"),
      actionButton("correlation", "Correlation")
	 ),
	  mainPanel(
		h2('Regression Model Results'),	
	  	tableOutput('regression'),
	  	h2('Correlation Results'),
	  	tableOutput('correlation')
	  )
 ) 
), # end tabpanel for basic statistics 

tabPanel(span("Regional Analysis", style = "color:blue"),
  sidebarLayout(
    sidebarPanel(
      h4("Analyze the performance of the top athletes by region"),
	  
	  helpText("Use the results provided here to view how the top athletes are performing on the different open WODs"),
	  
      uiOutput("indRegion"), 
	  numericInput("numRegion", "Enter the number of athletes to include in this analysis", 20)
	 ),
	  mainPanel(
	h4('This dot plot shows the average score by region for the selected WOD. We can see which region has the highest scores by WOD. Note the regions are ordered automatically.'),
	plotOutput('dotplot'),
	h4('This box and whisker plot shows the median score (represented by the block dot) and how varied the scores are by region'),
	plotOutput('bwplot')
	  
	)
 ) 
) # end tabpanel for regional analysis





)) # end program