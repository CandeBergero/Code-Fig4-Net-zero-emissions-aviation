# Code to generate contour plots for costs of different biofuels for net-zero aviation paper

##1. Load relevant libraries
library(ggplot2)
library(ContourFunctions)
library(plotly)
library(RColorBrewer)
library(processx)

#==========================================================================================================

##2. Create colorsclae 
  #We take colors from Viridis scale and add orange and red
colors_figures <- c("#440154", "#3b528b", "#21918c", "#5FC989","#5ec962", "#fde725", "#FF5733", "#ED1313")

#==========================================================================================================

##3. Create contour plots based on different formulas provided by Dolf Gielen and Seungwoo Kang

##### A. SYNFUELS #####
# Synfuel cost was based on mass balance. Below are the used cost equations.
#CH2 = (Hydrogen unit cost x 0.4 + CO2 unit cost x 3.14 ) / conversion eff (0.8).

#Create x & y arrays
hydrogen_cost <- as.numeric(0:6) #Hydrogen cost in $/kg H2 (x axis)
carbon_cost <- seq(0, 0.6, by = 0.1) #Carbon cost in %/kg CO2 (y axis)

#Define constants from equation
a_synfuel <- 0.4
b_synfuel <- 3.14
c_synfuel <- 0.8

# Create a black matrix
z1 <- matrix(,nrow = length(carbon_cost), ncol = length(hydrogen_cost))
    #NOTE: matrix[row, column] = value 
    #This means that we name first the row (y value), and then the column (x value)

# Iterate through arrays to populate matrix
for (i in 1:length(carbon_cost)){
  for (j in 1:length(hydrogen_cost)){
    z1[i, j] <- (carbon_cost[i] * b_synfuel + hydrogen_cost[j] * a_synfuel) / c_synfuel
  }
  
}

# contour(z)
fig_synfuels <- plot_ly(x = hydrogen_cost, #This is the column in matrix
                       y = carbon_cost, #This is the row in matrix
                       z = z1,
                       type = "contour",
                       colors = colors_figures,
                       colorbar = list(bordercolor = "pink",
                                       title = "Cost (USD/kg)",
                                       len = 0.5),
                       line = list(color = "white"),
                       contours = list(
                         showlabels = TRUE,
                         coloring = "heatmap",
                         size = 0.5,
                         start = 0.5,
                         end = 10
                       )) %>% 
  
  layout(title = "Synthetic Fuels",
         xaxis = list(
           title = "Hydrogen Cost (USD / kg H2)",
           nticks = 8
         ),
         yaxis = list(
           title = "Carbon Cost (USD / kg CO2)",
           nticks = 10
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )
# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_synfuels, "fig4_synthetic_fuels_kg.png")
orca(fig_synfuels, "fig4_synthetic_fuels_kg.svg")

#Print figure
fig_synfuels


## SYNFUELS cost in liters ##
# Synfuel cost was based on the mass balance. Below are the used cost equations.
#CH2 = (Hydrogen unit cost x 0.4 + CO2 unit cost x 3.14 ) / conversion eff (0.8).
  # In order to get liter cost we multiply everything times 0.74 kg/liter,
  # which is the density of gasoline (C8H18), similar to CH2

#Include new constants for conversion
synfuel_liter_multiplier <- 0.74 # kg/liter

# Create a black matrix
z1_liters <- matrix(,nrow = length(carbon_cost), ncol = length(hydrogen_cost))

#NOTE: matrix[row, column] = value 
#This means that we name first the row (y value), and then the column (x value)

# Iterate through arrays to populate matrix
for (i in 1:length(carbon_cost)){
  for (j in 1:length(hydrogen_cost)){
    z1_liters [i, j]<- ((carbon_cost[i] * b_synfuel * synfuel_liter_multiplier) + (hydrogen_cost[j] * a_synfuel * synfuel_liter_multiplier)) / c_synfuel
  }
  
}

# contour(z)
fig_synfuels_liter <- plot_ly(x = hydrogen_cost, #This is the column in matrix
                        y = carbon_cost, #This is the row in matrix
                        z = z1_liters,
                        type = "contour",
                        colors = colors_figures,
                        colorbar = list(bordercolor = "pink",
                                        title = "Cost (USD/liter)",
                                        len = 0.5),
                        line = list(color = "white"),
                        contours = list(
                          showlabels = TRUE,
                          coloring = "heatmap",
                          size = 0.5,
                          start = 0.5,
                          end = 7.5
                        )) %>% 
  
  layout(title = "Synthetic Fuels",
         xaxis = list(
           title = "Hydrogen Cost (USD / kg H2)",
           nticks = 8
         ),
         yaxis = list(
           title = "Carbon Cost (USD / kg CO2)",
           nticks = 10
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )
# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_synfuels_liter, "fig4_synthetic_fuels_liter.png")
orca(fig_synfuels_liter, "fig4_synthetic_fuels_liter.svg")



#====================================================================================================

##### B. HEFA #####
#Equation -> CAPEX – 4.3 USD/GJ biofuel (0.17 USD/kg biomass), OPEX - 8.6 USD/GJ biofuel (0.34 USD/kg biomass)
#Cost = (0.17 + 0.34 + Feedstock cost ) * Y (Inversed efficiency)

#Create x & y arrays
biomass_cost <- seq(0, 0.9, by = 0.1) #Delivered biomass cost in $/kg biomass
conv_eff <-  as.numeric(0:7) #Conversion efficiency (kg biomass / kg fuel)

#Define constants from equation
a_HEFA <- 0.17 + 0.34

# Create a blank matrix
  # rows will be conversion efficiency (0 to 7)
  # columns will be biomass cost (0 to 0.9)
z2 <- matrix(,nrow = length(biomass_cost), ncol = length(conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(biomass_cost)){
  for (j in 1:length(conv_eff)){
    z2[i, j] <- ((biomass_cost[i] + a_HEFA) * conv_eff[j])
  }
  
}

# contour(z)
fig_HEFA <- plot_ly(x = conv_eff, # This represents columns
                    y = biomass_cost, # This represents rows
                        z = z2,
                        type = "contour",
                        colorbar = list(bordercolor = "black",
                                        title = "Cost (USD/kg)"),
                        line = list(color = "white"),
                    #colorscale = "Viridis",
                    colors = colors_figures,
                    contours = list(
                      showlabels = TRUE,
                      coloring = "heatmap",
                      size = 0.5,
                      start = 0.5,
                      end = 10
                    )) %>% #"fill" is default, which is no gradient

  layout(title = "HEFA",
         xaxis = list(
           title = "Conversion Efficiency (kg biomass / kg fuel)",
           nticks = 8
         ),
         yaxis = list(
           title = "Cost of Delivered Biomass (USD / kg biomass)",
           nticks = 10
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  ) 

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_HEFA, "fig4_HEFA_kg.png")
orca(fig_HEFA, "fig4_HEFA_kg.svg")

#Print figure
fig_HEFA


## HEFA in liters ##
#Equation -> CAPEX – 4.3 USD/GJ biofuel (0.17 USD/kg biomass), OPEX - 8.6 USD/GJ biofuel (0.34 USD/kg biomass)
#Cost = (0.17 + 0.34 + Feedstock cost ) * Y (Inversed efficiency)
  # In order to get liter cost we multiply everything times  0.8265 kg/liter,
  # which is the average density of ethanol (0.783 kg/liter) and biodiesel (0.87 kg/liter)

#Define constants from equation
biofuels_liter_multiplier <- 0.8265 #kg per liters

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z2_liter<- matrix(,nrow = length(biomass_cost), ncol = length(conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(biomass_cost)){
  for (j in 1:length(conv_eff)){
    z2_liter[i, j] <- ((biomass_cost[i] + a_HEFA) * biofuels_liter_multiplier * conv_eff[j])
  }
  
}

# contour(z)
fig_HEFA_liter <- plot_ly(x = conv_eff, # This represents columns
                    y = biomass_cost, # This represents rows
                    z = z2_liter,
                    type = "contour",
                    colorbar = list(bordercolor = "black",
                                    title = "Cost (USD/liter)"),
                    line = list(color = "white"),
                    #colorscale = "Viridis",
                    colors = colors_figures,
                    contours = list(
                      showlabels = TRUE,
                      coloring = "heatmap",
                      size = 0.5,
                      start = 0.5,
                      end = 7.5
                    )) %>% #"fill" is default, which is no gradient

  
  layout(title = "HEFA",
         xaxis = list(
           title = "Conversion Efficiency (kg biomass / kg fuel)",
           nticks = 8
         ),
         yaxis = list(
           title = "Cost of Delivered Biomass (USD / kg biomass)",
           nticks = 10
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  ) 

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_HEFA_liter, "fig4_HEFA_liter.png")
orca(fig_HEFA_liter, "fig4_HEFA_liter.svg")



#==============================================================================================================

##### C. FT Biofuels #####
# CAPEX – 7.6 USD/GJ biofuel (0.07 USD/kg biomass), OPEX – 13.5 USD/GJ biofuel (0.12 USD/kg biomass)
# Cost = (0.07 + 0.12 + Feedstock cost ) * Y (Inversed efficiency)

#We use same x & y arrays as in HEFA

#Define constants from equation
a_FT <- 0.07 + 0.12

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z3<- matrix(,nrow = length(biomass_cost), ncol = length(conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(biomass_cost)){
  for (j in 1:length(conv_eff)){
    z3[i, j] <- ((biomass_cost[i] + a_FT) * conv_eff[j])
  }
  
}

# contour(z)
fig_FT <- plot_ly(x = conv_eff, # This represents columns
                  y = biomass_cost, # This represents rows
                  z = z3,
                  type = "contour",
                  colors = colors_figures,
                  colorbar = list(
                    bordercolor = "back",
                    title = "Cost (USD/kg"
                  ),
                  line = list(color = "white"), #we could add width = N for thicker lines
                  contours = list(
                    showlabels = TRUE,
                    coloring = "heatmap",
                    size = 0.5,
                    start = 0.5,
                    end = 10
                  )) %>% #"fill" is default, which is no gradient
          
    layout(title = "FT Biofuels",
                    xaxis = list(
                      title = "Conversion Efficiency (kg biomass / kg fuel)",
                      nticks = 8
                    ),
                    yaxis = list(
                      title = "Cost of Delivered Biomass (USD / kg biomass)",
                      nticks = 10
                    ),
                   margin = list (
                     b = 60,
                     t = 60,
                     l = 60,
                     r = 60
                   )
                  )

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_FT, "fig4_FT_kg.png")
orca(fig_FT, "fig4_FT_kg.svg")

#Print figure
fig_FT


## FT Biofuels in liters ##
# CAPEX – 7.6 USD/GJ biofuel (0.07 USD/kg biomass), OPEX – 13.5 USD/GJ biofuel (0.12 USD/kg biomass)
# Cost = (0.07 + 0.12 + Feedstock cost ) * Y (Inversed efficiency)
  # In order to get liter cost we multiply everything times  0.8265 kg/liter,
  # which is the average density of ethanol (0.783 kg/liter) and biodiesel (0.87 kg/liter)

#We use same x & y arrays as in HEFA, and multiplier

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z3_liters <- matrix(,nrow = length(biomass_cost), ncol = length(conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(biomass_cost)){
  for (j in 1:length(conv_eff)){
    z3_liters[i, j] <- ((biomass_cost[i] + a_FT) * biofuels_liter_multiplier * conv_eff[j])
  }
  
}

# contour(z)
fig_FT_liter <- plot_ly(x = conv_eff, # This represents columns
                  y = biomass_cost, # This represents rows
                  z = z3_liters,
                  type = "contour",
                  colors = colors_figures,
                  colorbar = list(
                    bordercolor = "back",
                    title = "Cost (USD/liter)"
                  ),
                  line = list(color = "white"), #we could add width = N for thicker lines
                  contours = list(
                    showlabels = TRUE,
                    coloring = "heatmap",
                    size = 0.5,
                    start = 0.5,
                    end = 7.5
                  )) %>% #"fill" is default, which is no gradient
  
  layout(title = "FT Biofuels",
         xaxis = list(
           title = "Conversion Efficiency (kg biomass / kg fuel)",
           nticks = 8
         ),
         yaxis = list(
           title = "Cost of Delivered Biomass (USD / kg biomass)",
           nticks = 10
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_FT_liter, "fig4_FT_liter.png")
orca(fig_FT_liter, "fig4_FT_liter.svg")
