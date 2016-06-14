library(shiny)
library(ggplot2)
library(tidyr)
library(deSolve)

#------ Function for si system ------------------------------------
si_model = function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    #rate of change
    dS = -r * S * I
    dI = r * S * I
    list(c(dS, dI))
  })
}
#----- Function to calculate solution to si system ----------------
si_disease = function(S_in, I_in, r_in, time_in){
  times = seq(0, time_in, by = 0.01)
  parameters = c(r = r_in)
  state = c(S = S_in, I = I_in)
  results = data.frame(ode(y = state, times = times, func = si_model, 
                           parms = parameters))
  return(results)
}
#------ Function for sis system ------------------------------------
sis_model = function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    #rate of change
    dS = -r * S * I + a * I
    dI = r * S * I - a * I
    list(c(dS, dI))
  })
}
#----- Function to calculate solution to sis system ----------------
sis_disease = function(S_in, I_in, r_in, a_in, time_in){
  times = seq(0, time_in, by = 0.05)
  parameters = c(r = r_in, a = a_in)
  state = c(S = S_in, I = I_in)
  results = data.frame(ode(y = state, times = times, func = sis_model, 
                           parms = parameters))
  return(results)
}
#------ Function for sir system ------------------------------------
sir_model = function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    #rate of change
    dS = -r * S * I 
    dI = r * S * I - a * I
    dR = a * I
    list(c(dS, dI, dR))
  })
}
#----- Function to calculate solution to sir system ----------------
sir_disease = function(S_in, I_in, R_in, r_in, a_in, time_in){
  times = seq(0, time_in, by = 0.05)
  parameters = c(r = r_in, a = a_in)
  state = c(S = S_in, I = I_in, R = R_in)
  results = data.frame(ode(y = state, times = times, func = sir_model, 
                           parms = parameters))
  return(results)
}

shinyServer(function(input, output, session) {
  # ---------- Create dynamic ui for model inputs -----------------------------
  output$slider = renderUI({
    if (is.null(input$model)) {
      return()
    }
    
    switch(input$model,
           "SI" = NULL,
           "SIS" = sliderInput("a", "Rate of Return to Susceptible",
                               value = 0.01, min = 0, max = 1, step = 0.01),
           "SIR" = sliderInput("a", "Rate of Resistance",
                               value = 0.01, min = 0, max = 1, step = 0.01)
    )
  }) 
  
  
  # -------- Create dynamic output dependent on model choice ------------------
  output$model_text = renderUI({
    if (is.null(input$model)) {
      return()
    }
    
    switch(input$model,
           "SI" = withMathJax(tags$div(
              tags$p("The plot below shows the number of susceptible and
                infected individuals as predicted by the \\(SI\\) model
                $$\\begin{align} 
                \\frac{\\textrm{dS}}{\\textrm{dt}} &= -rSI \\\\
                \\frac{\\textrm{dI}}{\\textrm{dt}} &= rSI 
                \\end{align}$$
                where \\(r\\) is the rate of infection of the susceptibles."),
              tags$p("Change the variables and see how the numbers
                change. Why does the population become infected quicker
                when it is larger?")
              )),
            "SIS" = withMathJax(tags$div(
              tags$p("The plot below shows the number of susceptible and
                infected individuals as predicted by the \\(SIS\\) model
                $$\\begin{align} 
                \\frac{\\textrm{dS}}{\\textrm{dt}} &= -rSI + aI\\\\
                \\frac{\\textrm{dI}}{\\textrm{dt}} &= rSI - aI
                \\end{align}$$
                where \\(r\\) is the rate of infection of the susceptibles 
                and \\(a\\) is the rate of recovery."),
              tags$p("Change the variables and see how the numbers
                change. Can you arrange it so the population has a constant
                number of infected individuals?")
              )), 
            "SIR" = withMathJax(tags$div(
              tags$p("The plot below shows the number of susceptible and
                infected individuals as predicted by the \\(SIR\\) model
                $$\\begin{align} 
                \\frac{\\textrm{dS}}{\\textrm{dt}} &= -rSI \\\\
                \\frac{\\textrm{dI}}{\\textrm{dt}} &= rSI - aI \\\\
                \\frac{\\textrm{dR}}{\\textrm{dt}} &= aI \\\\
                \\end{align}$$
                where \\(r\\) is the rate of infection of the susceptibles
                and \\(a\\) is the rate of resistance."),
              tags$p("Change the variables and see how the numbers
                change. What initial conditions do you need for an epidemic?")
              ))
          )
  })
  
  # -------- Generate plot of results from model ------------------------------
  output$plot <- renderPlot({
    if (input$model == "SI") {
        data = si_disease(S_in = input$S, I_in = input$I, r_in = input$r, 
                 time_in = input$time)
        out_long = gather(data, subpop, number, S:I)
        ggplot(out_long, aes(x = time, y = number, colour = subpop)) + 
          geom_line(size = 1) + 
          labs(x = "Time", y = "Number of Individuals",
               title = "Number of Susceptible and Infected against Time") +
          scale_colour_discrete(name = "Type of Individual", 
                                labels = c("Susceptible", "Infected")) + 
        theme(text = element_text(size = 18))
    } else if (input$model == "SIS") {
        if (is.null(input$a_sis)) {
          data = sis_disease(S_in = input$S, I_in = input$I, r_in = input$r, 
                             a_in = 0.01, time_in = input$time)  
        } else {
          data = sis_disease(S_in = input$S, I_in = input$I, r_in = input$r, 
                           a_in = input$a_sis, time_in = input$time)
        }
      
        out_long = gather(data, subpop, number, S:I)
        ggplot(out_long, aes(x = time, y = number, colour = subpop)) + 
          geom_line(size = 1) + 
          labs(x = "Time", y = "Number of Individuals",
               title = "Number of Susceptible and Infected against Time") +
          scale_colour_discrete(name = "Type of Individual", 
                                labels = c("Susceptible", "Infected")) + 
        theme(text = element_text(size = 18))
    } else {
        if (is.null(input$a_sir)) {
          data = sir_disease(S_in = input$S, I_in = input$I, R_in = 0, 
                             r_in = input$r, a_in = 0.01, 
                             time_in = input$time)  
        } else {
          data = sir_disease(S_in = input$S, I_in = input$I, R_in = 0, 
                             r_in = input$r, a_in = input$a_sir, 
                             time_in = input$time)
        }
        out_long = gather(data, subpop, number, S:R)
        ggplot(out_long, aes(x = time, y = number, colour = subpop)) + 
          geom_line(size = 1) + 
          labs(x = "Time", y = "Number of Individuals", title = 
                 "Number of Susceptible, Infected and Resistant against Time") +
          scale_colour_discrete(name = "Type of Individual", 
                                labels = c("Susceptible", "Infected", 
                                           "Resistant")) + 
        theme(text = element_text(size = 18))
    }
  })
  


})