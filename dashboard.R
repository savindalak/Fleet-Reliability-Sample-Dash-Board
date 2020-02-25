#Aitken machine Fleet reliability analysis for 2015-2018 period
library(ggplot2)
my_machine_data <-read.csv('C:\\Users\\savinda\\Documents\\Savinda Docs\\job hunt\\machine_availability_analysis_aitken.csv')

my_machine_data$Date <-as.Date(my_machine_data$Date,"%m/%d/%Y")

my_machine_data['Year']<-  strftime(my_machine_data$Date,'%Y')
#my_machine_data['Month']<-  strftime(my_machine_data$Date,'%B')


# define function to render box plot of fleet reliability of each machine category for any year between 2015-2018
Fleet_reliability_func <- function(year){
  
  machines <-my_machine_data$Machine_type[my_machine_data$Year== year]
  fleet_reliability <- my_machine_data$Availability..[my_machine_data$Year== year]
  boxplot(fleet_reliability~machines,col = 'orange',ylab='Fleet reliability',main = paste(year , ' fleet reliability for each machine category'))
  
}

Fleet_reliability_func('2015')


# define function to render box hist of fleet reliability of any machine category for any year between 2015-2018

Fleet_reliability_dist <- function(year,machine){
  
  
  ggplot(subset(my_machine_data,Machine_type==machine& Year==year),aes(Availability..))+
  geom_histogram(binwidth =3,aes(y = ..density..,fill=..density..))+
  ggtitle(paste(machine,'Fleet Reliability in',year))+geom_density()
}
Fleet_reliability_dist(2015,'ECH')

#define a function for density plot comparison for each machine category
Fleet_reliability_density <- function(machine){
  
  #ggplot(my_machine_data,aes(Availability..)+geom_density(color='blue',alpha=0.2)+ggtitle(paste(machine,'Fleet Reliability in',year))
  ggplot(subset((my_machine_data),Machine_type==machine),aes(x = Availability.. ,fill=Year))+
  geom_density(alpha=0.4)+
  ggtitle(paste(machine,'Fleet Reliability density plots'))
}

#define a function for box plot comparison for each machine category
box_plot_comparison<-function(machine){
  
  ggplot(subset((my_machine_data),Machine_type==machine),aes(y = Availability.. ,x=Year,fill=Year))+
  geom_boxplot()+
  ggtitle(paste(machine,'Fleet Reliability box plot comparison'))
  
}


# Create a dash board with 'shiny'
library(shinydashboard)
library(shiny)

ui<-dashboardPage(
  dashboardHeader(title = 'Machine Fleet Availability',titleWidth = 750),
  dashboardSidebar(),
  dashboardBody(
    
    fluidRow(
      
      box(title = 'controls',sliderInput('slider1','Year',width = 300,2015,2018,2016)),
      box(title = 'controls',selectInput('selector','Machine type',width = 300,c('ECH','Laden','Forklift'))),
      
    ),
    
    
    fluidRow(
      box(plotOutput('plot1',height = 350, width = 500)),
      box(plotOutput('plot2',height = 350, width = 500)),
      
      
    ),
    
    fluidRow(
      box(plotOutput('plot3',height = 350, width = 500)),
      box(plotOutput('plot4',height = 350, width = 500)),
      
      
    ),
  )
)

server<- function(input,output){
  output$plot1<- renderPlot({
    year<-input$slider1
    
    Fleet_reliability_func(year)
    
    
  })
  output$plot2<- renderPlot({
    year<-input$slider1
    machine<-input$selector
    
    Fleet_reliability_dist(year = year,machine = machine)
    
  })
  
  output$plot3<- renderPlot({
    
    machine<-input$selector
    
      Fleet_reliability_density(machine = machine)
    
  })
  
  output$plot4<- renderPlot({
    
    machine<-input$selector
    
    box_plot_comparison(machine = machine)
    
  })
}

shinyApp(ui,server)



