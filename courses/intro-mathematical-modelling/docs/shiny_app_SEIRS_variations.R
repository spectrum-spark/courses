# first set your working directory where you want it, 
# using the Session tab
# Library imports
# Library imports
required_packages <- (c("dplyr",
                        "deSolve",
                        "ggplot2",
                        "shiny"))
for (pkg in required_packages) {
  if (!pkg %in% rownames(installed.packages())) {
    install.packages(pkg, character.only = TRUE)
    
  }
  library(pkg, character.only = TRUE)
}  

ui <- fluidPage(
  HTML('SEIR-type models'),
  numericInput(inputId = "R0", "Basic reproduction number", value = 2.5, min=.0, max=10, step = .1),
  sliderInput(inputId = "duration_latency", "Latency duration", min=.01, max=100, value=5),
  sliderInput(inputId = "duration_infection",  "Log_10 duration of infection:",
              min = -1, max = 3, value = 1, step = .1),
  sliderInput(inputId = "duration_immunity", "Log 10 duration of immunity:",
              min = -1, max = 10, value = 10, step = .1),
  br(),
  textOutput("readout1"),
  textOutput("readout2"),
 
         plotOutput(outputId = "plotSEIR_time", width  = "500px",height = "400px"),  
         plotOutput(outputId = "plotSEIR_SE", width  = "500px",height = "400px")
 
)

server <- function(input, output) {
  
 
  
  
  output$plotSEIR_time <- renderPlot({  
    
    # Model parameters
    parameters = c(
      infections_per_time = input$R0/10^(input$duration_infection),
      incubation_period = (input$duration_latency),
      infectious_period = 10^(input$duration_infection),
      rate_of_immunity_waning = 1/(10^(input$duration_immunity))
    )
    
    
    # Initial conditions
    Total_population = 1000
    Initial_exposed = 0
    Initial_infected = 1
    Initial_recovered = 0
    Initial_susceptible = Total_population - Initial_exposed - Initial_infected - Initial_recovered
    
    
    # State variables
    state = c(Susceptible = Initial_susceptible,
              Exposed = Initial_exposed,
              Infected = Initial_infected,
              Recovered_or_dead = Initial_recovered)
    
    
    # Time window
    
    times = seq(1, 1000, by=1)
    
    
    
    # Solve model
    
    # Plot solution
    # plot(out[, 2], out[, 4])
    # plot(out)

    # Model function
    model_base = function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
        # Calculate the total population size
        Total_population = Susceptible + Exposed + Infected + Recovered_or_dead
        R0 = infections_per_time*infectious_period
        # Calculate the average force of infection imposed on each susceptible individual
        force_of_infection = R0 * Infected / (Total_population * infectious_period)
        # Calculate the net (instantaneous) change in each state variable
        Susceptible_change = -force_of_infection * Susceptible + Recovered_or_dead * rate_of_immunity_waning
        Exposed_change = force_of_infection * Susceptible - Exposed / incubation_period
        Infected_change = Exposed / incubation_period - Infected / infectious_period
        Recovered_change = Infected / infectious_period - Recovered_or_dead * rate_of_immunity_waning
        
        # Return net changes as list
        return(list(
          c(
            Susceptible_change,
            Exposed_change,
            Infected_change,
            Recovered_change
          )
        ))
      })
    }
    out = ode(y = state, times = as.numeric(times - times[1]), func = model_base, parms = parameters)
    plot(out,xlim=c(0, 1000), ylim=c(0, max(out[, 2:5])*1.1))

 
  })
  
  output$plotSEIR_SE <- renderPlot({  
    
    # Model parameters
    parameters = c(
      infections_per_time = input$R0/10^(input$duration_infection),
      incubation_period = (input$duration_latency),
      infectious_period = 10^(input$duration_infection),
      rate_of_immunity_waning = 1/(10^(input$duration_immunity))
    )
    
    # Initial conditions
    Total_population = 1000
    Initial_exposed = 0
    Initial_infected = 1
    Initial_recovered = 0
    Initial_susceptible = Total_population - Initial_exposed - Initial_infected - Initial_recovered
    
    
    # State variables
    state = c(Susceptible = Initial_susceptible,
              Exposed = Initial_exposed,
              Infected = Initial_infected,
              Recovered_or_dead = Initial_recovered)
    
    
    # Time window
    
    times = seq(1, 1000, by=1)
    
    
    
    # Solve model
    
    # Plot solution
    # plot(out[, 2], out[, 4])
    # plot(out)
    
    # Model function
    model_base = function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
        # Calculate the total population size
        Total_population = Susceptible + Exposed + Infected + Recovered_or_dead
        R0 = infections_per_time*infectious_period
        # Calculate the average force of infection imposed on each susceptible individual
        force_of_infection = R0 * Infected / (Total_population * infectious_period)
        # Calculate the net (instantaneous) change in each state variable
        Susceptible_change = -force_of_infection * Susceptible + Recovered_or_dead * rate_of_immunity_waning
        Exposed_change = force_of_infection * Susceptible - Exposed / incubation_period
        Infected_change = Exposed / incubation_period - Infected / infectious_period
        Recovered_change = Infected / infectious_period - Recovered_or_dead * rate_of_immunity_waning
        
        # Return net changes as list
        return(list(
          c(
            Susceptible_change,
            Exposed_change,
            Infected_change,
            Recovered_change
          )
        ))
      })
    }
    out = ode(y = state, times = as.numeric(times - times[1]), func = model_base, parms = parameters)
    plot(out[, 2], out[, 4], main="Phase plane of Susceptble versus Infected", 
         xlab="Susceptibles", ylab="Infected",
         xlim=c(0, 1000), ylim=c(0, max(out[,4])*1.1))
    
  })
}




shinyApp(ui = ui, server = server)