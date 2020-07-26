# Fundamental Theorem of Algebra demonstation. This application shows how 
# upcrossings of C_r move to the right, and downcrossings move to the left,
# so that either an upcrossing or downcrossing will pass the origin and produce
# a root. The domain is shown on the right, the image on the left.
#
# Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Author:  Jordan T. Barry

library(shiny)

# Define UI
ui <- fluidPage(
   
   # Application title
   titlePanel("Circle Images of Complex Polynomials (interactive) "),
   
   # Sidebar with a slider input for r Values
    sidebarLayout(
      sidebarPanel(position="left",
                   
                   #textInput will be to input equations for use in function later. 
                   #textInput is reactive
                   textInput("fun","f(z)=",
   "(z-(1+1i))*(z - 0.8*(-1+1i)) * (z - 0.9*(-1-1i)) / ((1+1i) * 0.8 * (-1+1i) * 0.9 * (-1-1i))"),
                   #can set window size for the plots here. 
                   numericInput("size", "Window Size:", 1.5, min=0, max=100),
                   #slider to change radius value
                   sliderInput("r",
                               "r Value",
                               min = 0,
                               max = 5,
                               value = 0.25,
                               step= 0.025)),  
      # Show a plot of the complex image for some function
      mainPanel(
        fluidRow(splitLayout(cellWidths=c("50%", "50%"), plotOutput("jPlot"), plotOutput("iPlot"))),
        plotOutput("cPlot")
   )
   
))

# Define server logic required to draw
server <- function(input, output) {
  #define function from user input data. Will handle real and omplex coefficients.
  f<-function(z){eval(parse(text=input$fun))}
  #define the values of theta from 0 to 2pi.
  theta=seq(0,2*pi,by=0.005)
  
   output$cPlot <- renderPlot({
     
     z=complex(modulus=input$r, argument = theta)
     y=f(z)
     plot(y, type="l", xlim=c(-input$size,input$size), ylim=c(-input$size,input$size), asp=1)
     title(main="Image Plot")
     #plot axis lines
     lines(c(-100,100),c(0,0))
     lines(c(0,0),c(-100,100))
     #plot points on real axis get imaginary part and check whether it is ver close to zero
     iPart=Im(y)
     rPart=Re(y)
     for (j in 1:(length(iPart)-1)){
       
       if(iPart[j]>=0 && iPart[j+1]<=0){
         #interpolate the average point if one does not exist
         p<-(rPart[j]+rPart[j+1])/2 + 0i
         #plot points on existing graph
         if (rPart[j]*rPart[j+1]<=0.003){
            points(p,pch=19,col='black', cex=3)
         } else {
            points(p,pch=1,col='red', cex=2)
         }
         #points(q,pch=1,col='red')
       } else if(iPart[j]<=0 && iPart[j+1]>=0){
         p<-(rPart[j]+rPart[j+1])/2 + 0i
         if (rPart[j]*rPart[j+1]<0.003){
            points(p,pch=19,col='black', cex=3)
         } else {
            points(p,pch=19,col='blue',cex=2)
         }
       }
     }
     
   })
   
   output$iPlot <- renderPlot({
      
     #get user input function
     z=complex(modulus=input$r, argument = theta)
     y=f(z)
     #plot points on real axis
     iPart=Im(y)
     rPart=Re(y)
     plot(theta,iPart, type="l", xlim=c(0,2*pi), ylim=c(-4,4))
     title(main="Pre-image Plot (Polar) ")#, xlab="Arg[z]", ylab="Im[f(z)]")
     #plot axis lines
     lines(c(-100,100),c(0,0))
     lines(c(0,0),c(-100,100))
     #plot the pre-images of upcrossing and downcrossing points.
     for (j in 1:(length(iPart)-1)){
       
       if(iPart[j]>=0 && iPart[j+1]<=0){
         q<-0.5*(Re(z[j])+Re(z[j+1]))+(0.5*(Im(z[j])+Im(z[j+1])))*1i
         if (rPart[j]*rPart[j+1]<0.003){
            points(theta[j],0,pch=19,col='black', cex=3)
         } else {
            points(theta[j],0,pch=1,col='red',cex=2)
         }
       } else if(iPart[j]<=0 && iPart[j+1]>=0){
         q<-0.5*(Re(z[j])+Re(z[j+1]))+(0.5*(Im(z[j])+Im(z[j+1])))*1i
         if (rPart[j]*rPart[j+1]<0.003){
            points(theta[j],0,pch=19,col='black', cex=3)
         } else {
            points(theta[j],0,pch=19,col='blue',cex=2)
         }
       }
     }

   })
   output$jPlot <- renderPlot({
     
     #get user input function
     z=complex(modulus=input$r, argument = theta)
     y=f(z)
     plot(z, type="l", xlim=c(-input$size,input$size), ylim=c(-input$size,input$size), asp=1)
     title(main="Pre-image Plot (Cartesian)")
     #plot axis lines
     lines(c(-100,100),c(0,0))
     lines(c(0,0),c(-100,100))
     #plot points on real axis
     iPart=Im(y)
     rPart=Re(y)
     #plot the pre-images of upcrossing and downcrossing points.
     for (j in 1:(length(iPart)-1)){
       
       if(iPart[j]>=0 && iPart[j+1]<=0){
         k1<-abs(iPart[j+1])/(abs(iPart[j])+abs(iPart[j+1]))
         q<-k1*z[j]+(1-k1)*z[j+1]
         if (rPart[j]*rPart[j+1]<0.003){
           points(q,pch=19,col='black', cex=3)
           text(q+.1,labels=round(q,digits=2),cex=9,pos=4)
         } else {
           points(q,pch=1,col='red',cex=2)
         }
       } else if(iPart[j]<=0 && iPart[j+1]>=0){
          k1<-abs(iPart[j+1])/(abs(iPart[j])+abs(iPart[j+1]))
          q<-k1*z[j]+(1-k1)*z[j+1]
          if (rPart[j]*rPart[j+1]<0.003){
           points(q,pch=19,col='black', cex=3)
           text(q+.1,labels=round(q,digits=2),cex=0.9,pos=4)
         } else {
           points(q,pch=19,col='blue',cex=2)
         }
       }
     }
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)
