# Code to generate contour plots for costs of different biofuels for net-zero aviation paper

##1. Load relevant libraries
#install.packages("ggplot2")
library(ggplot2)

library(ContourFunctions)
library(plotly)

#install.packages("RColorBrewer")
library(RColorBrewer)

library(processx)
library(magrittr)
library(reticulate)
library(readr)

setwd("~/Documents/R_projects/Aviaiton_paper")

#==========================================================================================================

##2. Create colorsclae
  #We take colors from Viridis scale and add orange and red
colors_figures <- c("#440154", "#3b528b", "#21918c", "#5FC989","#5ec962", "#fde725", "#FF5733", "#ED1313")

#We take colors from Viridis scale and add orange and red
colors_figures_2 <- c("#440154", "#3b528b", "#21918c", "#5FC989", "#f0df4a", "#fc846a" ,"#FF5733", "#ED1313")

#We take colors from Viridis scale and modify for plot on CDR vs SAF
colors_figures_3 <- c("#440154", "#3b528b", "#21918c", "#F5F5DC", "#fc846a" ,"#FF5733", "#ED1313")
#==========================================================================================================

##3. Create contour plots FIGURE 4

##### A. SYNFUELS #####
# Synfuel cost was based on mass balance. Below are the used cost equations.
#CH2 = (Hydrogen unit cost x 0.4 + CO2 unit cost x 3.14 ) / conversion eff (0.8).

#Create x & y arrays
hydrogen_cost <- as.numeric(0:7) #Hydrogen cost in $/kg H2 (x axis)
carbon_cost <- seq(0, 0.7, by = 0.1) #Carbon cost in %/kg CO2 (y axis)

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
                       colors = colors_figures_2,
                       colorbar = list(bordercolor = "pink",
                                       title = "Cost ($/kg)",
                                       len = 0.5),
                       line = list(color = "white"),
                       contours = list(
                         showlabels = TRUE,
                         coloring = "heatmap",
                         size = 0.5,
                         start = 0,
                         end = 6
                       )) %>%
  
  layout(title = "Synthetic Fuels",
         xaxis = list(
           title = "Hydrogen Cost ($ / kg H2)",
           nticks = 8
         ),
         yaxis = list(
           title = "Carbon Cost ($ / kg CO2)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_synfuels

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_synfuels, "fig4_synthetic_fuels_kg.png")
orca(fig_synfuels, "fig4_synthetic_fuels_kg.svg")



## SYNFUELS cost in liters (Figure 4a)##
# Synfuel cost was based on the mass balance. Below are the used cost equations.
#CH2 = (Hydrogen unit cost x 0.4 + CO2 unit cost x 3.14 ) / conversion eff (0.8).
  # In order to get liter cost we multiply everything times 0.8 kg/liter,
  # which is the density of jet fuel

#Include new constants for conversion
synfuel_liter_multiplier <- 0.8 # kg/liter

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
                        colors = colors_figures_2,
                        colorbar = list(bordercolor = "pink",
                                        title = "Cost ($/liter)", #COST IS IN LITERS
                                        len = 0.5),
                        line = list(color = "white"),
                        contours = list(
                          showlabels = TRUE,
                          coloring = "heatmap",
                          size = 0.5,
                          start = 0,
                          end = 6
                        )) %>%

  layout(title = "Synthetic Fuels",
         xaxis = list(
           title = "Hydrogen Cost ($ / kg H2)",
           nticks = 8
         ),
         yaxis = list(
           title = "Carbon Cost ($ / kg CO2)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print
fig_synfuels_liter

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_synfuels_liter, "fig4_synthetic_fuels_liter.png")
orca(fig_synfuels_liter, "fig4_synthetic_fuels_liter.svg")


##### Add the cost of sequestering the non-CO2s embodied in each liter of synfuel (Figure 4b) #####
#We keep the two axis the same (hydrogen cost in $/kgH2 and carbon cost in $/kgCO2)
#but to each data point we add the cost of removing the non-CO2 equivalent warming with offsets
# assuming a multiplier of 1.7 (GWP100) and a carbon cost of $350/tCO2, which yields about $0.62 per liter

multiplier_GWP100 <- 1.7 #This is the multiplier from Lee et al 2021 for GWP100
CO2_amount <- 3.16 #This is the amount in kg of CO2 per kg of fuel burned
density_synfuel <- 1 / synfuel_liter_multiplier #This is density in L/kg
CDR_cost_assumed <- 350 # We assume a CDR cost fo $350/tCO2

#Calculating the total amount of non-CO2 per liter of fuel burned
total_GHG_kg = multiplier_GWP100 * CO2_amount #This is the total GHG in kg from burning 1 kg of fossil jet fuel
total_nonCO2_kg = total_GHG_kg - CO2_amount #This is the total kg for non-CO2s
total_nonCO2_l = total_nonCO2_kg / density_synfuel #This is the total amount of non-CO2s in kg per L of SAF burned

synfuel_offset_cost = total_nonCO2_l * (CDR_cost_assumed/1000)

# Create a black matrix
z1_liters_with_CDR <- matrix(,nrow = length(carbon_cost), ncol = length(hydrogen_cost))

#NOTE: matrix[row, column] = value
#This means that we name first the row (y value), and then the column (x value)

# Iterate through arrays to populate matrix
for (i in 1:length(carbon_cost)){
  for (j in 1:length(hydrogen_cost)){
    z1_liters_with_CDR [i, j]<- (((carbon_cost[i] * b_synfuel * synfuel_liter_multiplier) + (hydrogen_cost[j] * a_synfuel * synfuel_liter_multiplier)) / c_synfuel) + (synfuel_offset_cost) 
  }
  
}

# contour(z)
fig_synfuels_liter_with_CDR <- plot_ly(x = hydrogen_cost, #This is the column in matrix
                              y = carbon_cost, #This is the row in matrix
                              z = z1_liters_with_CDR,
                              type = "contour",
                              colors = colors_figures_2,
                              colorbar = list(bordercolor = "pink",
                                              title = "Cost ($/liter)", #COST IS IN LITERS
                                              len = 0.5),
                              line = list(color = "white"),
                              contours = list(
                                showlabels = TRUE,
                                coloring = "heatmap",
                                size = 0.5,
                                start = 0,
                                end = 6
                              )) %>%
  
  layout(title = "Synthetic Fuels & CDR",
         xaxis = list(
           title = "Hydrogen Cost ($ / kg H2)",
           nticks = 8
         ),
         yaxis = list(
           title = "Carbon Cost ($ / kg CO2)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print
fig_synfuels_liter_with_CDR

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_synfuels_liter_with_CDR, "fig4_synthetic_fuels_liter_with_CDR.png")
orca(fig_synfuels_liter_with_CDR, "fig4_synthetic_fuels_liter_with_CDR.svg")


#====================================================================================================

##### B. HEFA #####
#Equation -> CAPEX – 4.3 USD/GJ biofuel (0.17 USD/kg biomass), OPEX - 8.6 USD/GJ biofuel (0.34 USD/kg biomass)
#Cost = (0.17 + 0.34 + Feedstock cost ) * Y (Inversed efficiency)

#Create x & y arrays
HEFA_cost <- seq(0, 1, by = 0.2) #Delivered biomass cost in $/kg biomass
HEFA_conv_eff <-  as.numeric(0:2) #Conversion efficiency (kg biomass / kg fuel)

#Define constants from equation
a_HEFA <- 0.17 + 0.34

# Create a blank matrix
  # rows will be conversion efficiency (0 to 4)
  # columns will be biomass cost (0 to 0.9)
z2 <- matrix(,nrow = length(HEFA_cost), ncol = length(HEFA_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(HEFA_cost)){
  for (j in 1:length(HEFA_conv_eff)){
    z2[i, j] <- ((HEFA_cost[i] + a_HEFA) * HEFA_conv_eff[j])
  }

}

# contour(z)
fig_HEFA <- plot_ly(x = HEFA_conv_eff, # This represents columns
                    y = HEFA_cost, # This represents rows
                        z = z2,
                        type = "contour",
                        colorbar = list(bordercolor = "black",
                                        title = "Cost (USD/kg)"),
                        line = list(color = "white"),
                    #colorscale = "Viridis",
                    colors = colors_figures_2,
                    contours = list(
                      showlabels = TRUE,
                      coloring = "heatmap",
                      size = 0.5,
                      start = 0,
                      end = 6
                    )) %>% #"fill" is default, which is no gradient

  layout(title = "HEFA",
         xaxis = list(
           title = "Conversion Efficiency (kg biomass / kg fuel)",
           nticks = 3
         ),
         yaxis = list(
           title = "Cost of Delivered Biomass (USD / kg biomass)",
           nticks = 6
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_HEFA

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_HEFA, "fig4_HEFA_kg.png")
orca(fig_HEFA, "fig4_HEFA_kg.svg")



## HEFA in liters (Figure 4e)##
#Equation -> CAPEX – 4.3 USD/GJ biofuel (0.17 USD/kg biomass), OPEX - 8.6 USD/GJ biofuel (0.34 USD/kg biomass)
#Cost = (0.17 + 0.34 + Feedstock cost ) * Y (Inversed efficiency)
  # In order to get liter cost we multiply everything times  0.88 kg/liter,
  # which is the density of biodiesel (0.88 kg/liter)

#Define constants from equation
biofuels_liter_multiplier <- 0.88 #kg per liters

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z2_liter<- matrix(,nrow = length(HEFA_cost), ncol = length(HEFA_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(HEFA_cost)){
  for (j in 1:length(HEFA_conv_eff)){
    z2_liter[i, j] <- ((HEFA_cost[i] + a_HEFA) * biofuels_liter_multiplier * HEFA_conv_eff[j])
  }

}

# contour(z)
fig_HEFA_liter <- plot_ly(x = HEFA_conv_eff, # This represents columns
                    y = HEFA_cost, # This represents rows
                    z = z2_liter,
                    type = "contour",
                    colorbar = list(bordercolor = "black",
                                    title = "Cost (USD/liter)"),
                    line = list(color = "white"),
                    #colorscale = "Viridis",
                    colors = colors_figures_2,
                    contours = list(
                      showlabels = TRUE,
                      coloring = "heatmap",
                      size = 0.5,
                      start = 0,
                      end = 6
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

print(fig_HEFA_liter)

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_HEFA_liter, "fig4_HEFA_liter.png")
orca(fig_HEFA_liter, "fig4_HEFA_liter.svg")



##### Add the cost of sequestering the non-CO2s embodied in each liter of synfuel (Figure 4f) #####
#We keep the two axis the same (hydrogen cost in $/kgH2 and carbon cost in $/kgCO2)
#but to each data point we add the cost of removing the non-CO2 equivalent warming with offsets
# assuming a multiplier of 1.7 (GWP100) and a carbon cost of $350/tCO2, which yields about $0.68 per liter (this is different form synthetic fuels because the density is a little lower for bio-diesel)

density_biodiesel <- 1 / biofuels_liter_multiplier #This is density in L/kg

#Calculating the total amount of non-CO2 per liter of fuel burned
total_nonCO2_l_biodiesel = total_nonCO2_kg / density_biodiesel #This is the total amount of non-CO2s in kg per L of SAF burned

biofuel_offset_cost = total_nonCO2_l_biodiesel * (CDR_cost_assumed/1000)

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z2_liter_with_CDR <- matrix(,nrow = length(HEFA_cost), ncol = length(HEFA_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(HEFA_cost)){
  for (j in 1:length(HEFA_conv_eff)){
    z2_liter_with_CDR[i, j] <- ((HEFA_cost[i] + a_HEFA) * biofuels_liter_multiplier * HEFA_conv_eff[j]) + biofuel_offset_cost
  }
  
}

# contour(z)
fig_HEFA_liter_with_CDR <- plot_ly(x = HEFA_conv_eff, # This represents columns
                          y = HEFA_cost, # This represents rows
                          z = z2_liter_with_CDR,
                          type = "contour",
                          colorbar = list(bordercolor = "black",
                                          title = "Cost (USD/liter)"),
                          line = list(color = "white"),
                          #colorscale = "Viridis",
                          colors = colors_figures_2,
                          contours = list(
                            showlabels = TRUE,
                            coloring = "heatmap",
                            size = 0.5,
                            start = 0,
                            end = 6
                          )) %>% #"fill" is default, which is no gradient
  
  
  layout(title = "HEFA with CDR",
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

print(fig_HEFA_liter_with_CDR)

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_HEFA_liter_with_CDR, "fig4_HEFA_liter_with_CDR.png")
orca(fig_HEFA_liter_with_CDR, "fig4_HEFA_liter_with_CDR.svg")

#==============================================================================================================

##### C. FT Biofuels #####
# CAPEX – 7.6 USD/GJ biofuel (0.07 USD/kg biomass), OPEX – 13.5 USD/GJ biofuel (0.12 USD/kg biomass)
# Cost = (0.07 + 0.12 + Feedstock cost ) * Y (Inverse efficiency)

#Create x & y arrays
FT_cost <- seq(0, 0.7, by = 0.1) #Delivered biomass cost in $/kg biomass
FT_conv_eff <-  as.numeric(0:4) #Conversion efficiency (kg biomass / kg fuel)

#Define constants from equation
a_FT <- 0.07 + 0.12

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z3<- matrix(,nrow = length(FT_cost), ncol = length(FT_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(FT_cost)){
  for (j in 1:length(FT_conv_eff)){
    z3[i, j] <- ((FT_cost[i] + a_FT) * FT_conv_eff[j])
  }

}

# contour(z)
fig_FT <- plot_ly(x = FT_conv_eff, # This represents columns
                  y = FT_cost, # This represents rows
                  z = z3,
                  type = "contour",
                  colors = colors_figures_2,
                  colorbar = list(
                    bordercolor = "back",
                    title = "Cost (USD/kg"
                  ),
                  line = list(color = "white"), #we could add width = N for thicker lines
                  contours = list(
                    showlabels = TRUE,
                    coloring = "heatmap",
                    size = 0.5,
                    start = 0,
                    end = 6
                  )) %>% #"fill" is default, which is no gradient

    layout(title = "FT Biofuels",
                    xaxis = list(
                      title = "Conversion Efficiency (kg biomass / kg fuel)",
                      nticks = 5
                    ),
                    yaxis = list(
                      title = "Cost of Delivered Biomass (USD / kg biomass)",
                      nticks = 8
                    ),
                   margin = list (
                     b = 60,
                     t = 60,
                     l = 60,
                     r = 60
                   )
                  )

#Print figure
fig_FT

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_FT, "fig4_FT_kg.png")
orca(fig_FT, "fig4_FT_kg.svg")



## FT Biofuels in liters (Figure 4c)##
# CAPEX – 7.6 USD/GJ biofuel (0.07 USD/kg biomass), OPEX – 13.5 USD/GJ biofuel (0.12 USD/kg biomass)
# Cost = (0.07 + 0.12 + Feedstock cost ) * Y (Inversed efficiency)
  # In order to get liter cost we multiply everything times  0.8265 kg/liter,
  # which is the average density of ethanol (0.783 kg/liter) and biodiesel (0.87 kg/liter)

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z3_liters <- matrix(,nrow = length(FT_cost), ncol = length(FT_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(FT_cost)){
  for (j in 1:length(FT_conv_eff)){
    z3_liters[i, j] <- ((FT_cost[i] + a_FT) * biofuels_liter_multiplier * FT_conv_eff[j])
  }

}

# contour(z)
fig_FT_liter <- plot_ly(x = FT_conv_eff, # This represents columns
                  y = FT_cost, # This represents rows
                  z = z3_liters,
                  type = "contour",
                  colors = colors_figures_2,
                  colorbar = list(
                    bordercolor = "back",
                    title = "Cost (USD/liter)"
                  ),
                  line = list(color = "white"), #we could add width = N for thicker lines
                  contours = list(
                    showlabels = TRUE,
                    coloring = "heatmap",
                    size = 0.5,
                    start = 0,
                    end = 6
                  )) %>% #"fill" is default, which is no gradient

  layout(title = "FT Biofuels",
         xaxis = list(
           title = "Conversion Efficiency (kg biomass / kg fuel)",
           nticks = 5
         ),
         yaxis = list(
           title = "Cost of Delivered Biomass (USD / kg biomass)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print
fig_FT_liter

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_FT_liter, "fig4_FT_liter.png")
orca(fig_FT_liter, "fig4_FT_liter.svg")



##### Add the cost of sequestering the non-CO2s embodied in each liter of synfuel (Figure 4d) #####
#We keep the two axis the same (hydrogen cost in $/kgH2 and carbon cost in $/kgCO2)
#but to each data point we add the cost of removing the non-CO2 equivalent warming with offsets
# assuming a multiplier of 1.7 (GWP100) and a carbon cost of $350/tCO2, which yields about $0.68 per liter (this is different form synthetic fuels because the density is a little lower for bio-diesel)

# Create a blank matrix
# rows will be conversion efficiency (0 to 7)
# columns will be biomass cost (0 to 0.9)
z3_liters_with_CDR <- matrix(,nrow = length(FT_cost), ncol = length(FT_conv_eff))

# Iterate through arrays to populate matrix
for (i in 1:length(FT_cost)){
  for (j in 1:length(FT_conv_eff)){
    z3_liters_with_CDR[i, j] <- ((FT_cost[i] + a_FT) * biofuels_liter_multiplier * FT_conv_eff[j]) + biofuel_offset_cost
  }
}

# contour(z)
fig_FT_liter_with_CDR <- plot_ly(x = FT_conv_eff, # This represents columns
                        y = FT_cost, # This represents rows
                        z = z3_liters_with_CDR,
                        type = "contour",
                        colors = colors_figures_2,
                        colorbar = list(
                          bordercolor = "back",
                          title = "Cost (USD/liter)"
                        ),
                        line = list(color = "white"), #we could add width = N for thicker lines
                        contours = list(
                          showlabels = TRUE,
                          coloring = "heatmap",
                          size = 0.5,
                          start = 0,
                          end = 6
                        )) %>% #"fill" is default, which is no gradient
  
  layout(title = "FT Biofuels with CDR",
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

#Print
fig_FT_liter_with_CDR

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_FT_liter_with_CDR, "fig4_FT_liter_with_CDR.png")
orca(fig_FT_liter_with_CDR, "fig4_FT_liter_with_CDR.svg")

#==========================================================================================================

# SUPPLEMENTARY FIGURE 7

##### Sup. Figure 7a #####
#Now we want to try to add more figures about the cost of sequestering carbon

#One option is to plot how much carbon would need to be offset vs. how much it costs to sequester the carbon

#Create x & y arrays
carbon_seq_quant <- seq(0, 4, by = 0.5) #Carbon sequestration amounts from our scenarios and GWP100 in Gt CO2 (10^9)
carbon_seq_cost <-  seq(0, 700, by = 100) #Carbon sequestration cost in USD per ton of CO2

# Create a blank matrix
z4_cost <- matrix(,nrow = length(carbon_seq_cost), ncol = length(carbon_seq_quant))

# Iterate through arrays to populate matrix
for (i in 1:length(carbon_seq_cost)){
  for (j in 1:length(carbon_seq_quant)){
    z4_cost[i, j] <- ((carbon_seq_quant[j] * carbon_seq_cost[i])/10^3) #result will be in trillion USD
  }

}

# contour(z)
fig_co2_seq_cost <- plot_ly(x = carbon_seq_quant, # This represents columns
                        y = carbon_seq_cost, # This represents rows
                        z = z4_cost,
                        type = "contour",
                        colors = colors_figures_2,
                        colorbar = list(
                          bordercolor = "back",
                          title = "Cost (trillion $)"
                        ),
                        line = list(color = "white"), #we could add width = N for thicker lines
                        contours = list(
                          showlabels = TRUE,
                          coloring = "heatmap",
                          size = .2,
                          start = .2,
                          end = 2.8
                        )) %>% #"fill" is default, which is no gradient

  layout(title = "Total Sequestration Cost by 2050",
         xaxis = list(
           title = "Carbon Dioxide Removal (GtCO2)",
           nticks = 5
         ),
         yaxis = list(
           title = "Carbon Sequestration Cost ($/tCO2)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_co2_seq_cost

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_co2_seq_cost, "fig4_carbon_seq_cost.png")
orca(fig_co2_seq_cost, "fig4_carbon_seq_cost.svg")


##### Sup. Figure 7b#####
# In supplementary figure 7, we add a plot that shows how the cost to offset non-CO2s changes based on what multiplier we use and what carbon cost we assume. 
  #GWP100 multiplier 1.7
  #GWP50 multiplier 2.3
  #GWP20 multiplier 4.0

sup_fig7_CDR_multipliers <- read_csv("sup_fig7_CDR_multipliers.csv") #Here we load the table with the costs of offsetting non-CO2s based on different CDR cost and different multipliers

sup_fig7_CDR_multipliers <- data.matrix(sup_fig7_CDR_multipliers)

#Create x & y arrays
CDR_cost <- seq(0, 700, by = 100) 
Multiplier_syn <- seq(1, 4, by = 0.5) 


figure_sup7_CDR_multipliers <- plot_ly(x = Multiplier_syn, #This is the column in matrix
                                  y = CDR_cost, #This is the row in matrix
                                  z = sup_fig7_CDR_multipliers,
                                  type = "contour",
                                  colors = colors_figures_2,
                                  colorbar = list(bordercolor = "pink",
                                                  title = "Cost (USD/liter)",
                                                  len = 0.5),
                                  line = list(color = "white"),
                                  contours = list(
                                    showlabels = TRUE,
                                    coloring = "heatmap",
                                    size = 0.5,
                                    start = 0,
                                    end = 5
                                  )) %>%
  
  layout(title = "Synthetic Fuels CDR Cost",
         xaxis = list(
           title = "Multiplier",
           nticks = 7
         ),
         yaxis = list(
           title = "CDR Cost (USD / ton CO2)",
           nticks = 8
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

print(figure_sup7_CDR_multipliers)


# Save the figure as both PNG and vector file to modify in Illustrator
orca(figure_sup7_CDR_multipliers, "fig4_CDR_multiplier.png")
orca(figure_sup7_CDR_multipliers, "fig4_CDR_multiplier.svg")

#==========================================================================================================
# Figure out total cost of using 100% SAF to be net-zero vs 100% DAC

##### We first create the total cost of using 100% SAF
#Create x & y arrays
SAF_quant <- seq(0, 1, by = 0.1) # This is the amount of SAF in trillion litera by 2050 from our 3 demand + energy intensity scenarios, assuming all energy comes form SAF

SAF_cost <-  seq(0, 7, by = 1) #This is the SAF cost in $/L from literature and our paper

# Create a blank matrix
z5_cost <- matrix(,nrow = length(SAF_quant), ncol = length(SAF_cost))

# Iterate through arrays to populate matrix
for (i in 1:length(SAF_quant)){
  for (j in 1:length(SAF_cost)){
    z5_cost[i, j] <- ((SAF_quant[i] * SAF_cost[j])) #result will be in trillion dollars
  }

}

# contour(z)
fig_100_SAF_cost <- plot_ly(x = SAF_cost, # This represents columns
                            y = SAF_quant, # This represents rows
                            z = z5_cost,
                            type = "contour",
                            colors = colors_figures_2,
                            colorbar = list(
                              bordercolor = "back",
                              title = "Cost (trillion $)"
                            ),
                            line = list(color = "white"), #we could add width = N for thicker lines
                            contours = list(
                              showlabels = TRUE,
                              coloring = "heatmap",
                              size = 0.5,
                              start = 0.5,
                              end = 7
                            )) %>% #"fill" is default, which is no gradient

  layout(title = "2050 Net-zero Cost Assuming 100% SAF",
         xaxis = list(
           title = "SAF Cost ($/L)",
           nticks = 8
         ),
         yaxis = list(
           title = "SAF Quantity (trillion liters)",
           nticks = 11
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_100_SAF_cost

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_100_SAF_cost, "fig_100_SAF_cost.png")
orca(fig_100_SAF_cost, "fig_100_SAF_cost.svg")



##### We now create the total cost of using 100% DAC
#Create x & y arrays
DAC_quant <- seq(0, 2, by = 0.2) # This is the amount of CO2 in Gt from our scenarios by 2050

DAC_cost <-  seq(0, 350, by = 50) #This is the DAC cost in $/tCO2 from literature (Fuss et al 2018, https://doi.org/10.1088/1748-9326/aabf9f) and IEA (https://www.iea.org/commentaries/is-carbon-capture-too-expensive)

# Create a blank matrix
z6_cost <- matrix(,nrow = length(DAC_quant), ncol = length(DAC_cost))

# Iterate through arrays to populate matrix
for (i in 1:length(DAC_quant)){
  for (j in 1:length(DAC_cost)){
    z6_cost[i, j] <- (((DAC_quant[i] * 10^9) * DAC_cost[j]))/10^12 #result will be in trillion dollars
  }

}

# contour(z)
fig_100_DAC_cost <- plot_ly(x = DAC_cost, # This represents columns
                            y = DAC_quant, # This represents rows
                            z = z6_cost,
                            type = "contour",
                            colors = colors_figures_2,
                            colorbar = list(
                              bordercolor = "back",
                              title = "Cost (trillion $)"
                            ),
                            line = list(color = "white"), #we could add width = N for thicker lines
                            contours = list(
                              showlabels = TRUE,
                              coloring = "heatmap",
                              size = 0.1,
                              start = 0.1,
                              end = 0.7
                            )) %>% #"fill" is default, which is no gradient

  layout(title = "2050 Net-zero Cost Assuming 100% CDR",
         xaxis = list(
           title = "CDR Cost ($/tCO2)",
           nticks = 8
         ),
         yaxis = list(
           title = "CDR Quantity (GtCO2)",
           nticks = 11
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_100_DAC_cost

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_100_DAC_cost, "fig_100_DAC_cost.png")
orca(fig_100_DAC_cost, "fig_100_DAC_cost.svg")

#==============================================================================================================
# Now we want to estimate what is cheaper? To mitigate emissions by passenger-kilometer-equivalent / ton-kilometer-equivalent using SAF or using CDR?
  # This means: we calculate how much GHG (CO2 + non-CO2) are emitted per pass-km-eq and per ton-km-eq
  # Then we can calculate how much it costs to do CDR to offset those emissions
  # And how much would it cost to produce SAF to not have those emissions to begin with

#Create x & y arrays
SAF_cost_Liter <- seq(0, 2.8, by = 0.2) #SAF cost in $/L (x axis)
CDR_cost_ton <- seq(0, 700, by = 50) #CDR cost in $/ton  (y axis)

#Define constants from equation
pass_SAF <- 0.04 #This is the amount of liters of biofuel per pass-km-eq
pass_CDR <- 0.16 #This are the emissions in kg that need to be offset if we burn the fuel needed for 1 pass-km-eq
ton_SAF <- 0.42 #This is the amount of liters of biofuel per pass-km-eq
ton_CDR <- 1.73 #This are the emissions in kg that need to be offset if we burn the fuel needed for 1 pass-km-eq


### FOR PASSENGER-KILOMETER-EQUIVALENT
# Create a black matrix
diff_cost_pass <- matrix(,nrow = length(CDR_cost_ton), ncol = length(SAF_cost_Liter))
#NOTE: matrix[row, column] = value
#This means that we name first the row (y value), and then the column (x value)

# Iterate through arrays to populate matrix
for (i in 1:length(CDR_cost_ton)){
  for (j in 1:length(SAF_cost_Liter)){
    diff_cost_pass[i, j] <- (((CDR_cost_ton[i]/1000) * pass_CDR) - (SAF_cost_Liter[j] * pass_SAF))
  }
}
    # CDR - SAF means
    # When the value is POSITIVE, mitigating with SAF is cheaper
    # When the value is NEGATIVE, mitigating with CDR is cheaper

# contour(z)
fig_pass_km <- plot_ly(x = SAF_cost_Liter, #This is the column in matrix
                        y = CDR_cost_ton, #This is the row in matrix
                        z = diff_cost_pass,
                        type = "contour",
                        colors = colors_figures_3,
                        colorbar = list(bordercolor = "pink",
                                        title = "Cost Difference ($/pass-km-eq)",
                                        len = 0.5),
                        line = list(color = "white"),
                        contours = list(
                          showlabels = TRUE,
                          coloring = "heatmap",
                          size = 0.03,
                          start = -0.12,
                          end = 0.12
                        )) %>%

  layout(title = "CDR vs. SAF Cost per passenger-kilometer-equivalent (CDR - SAF)",
         xaxis = list(
           title = "SAF Cost ($ / L)",
           nticks = 15
         ),
         yaxis = list(
           title = "Carbon Removal Cost ($ / ton CO2)",
           nticks = 15
         ),
         margin = list (
           b = 60,
           t = 60,
           l = 60,
           r = 60
         )
  )

#Print figure
fig_pass_km

# Save the figure as both PNG and vector file to modify in Illustrator
orca(fig_pass_km, "fig_pass_km.png")
orca(fig_pass_km, "fig_pass_km.svg")


### TON-KILOMETER-EQUIVALENT
# Create a black matrix
diff_cost_ton <- matrix(,nrow = length(CDR_cost_ton), ncol = length(SAF_cost_Liter))
#NOTE: matrix[row, column] = value
#This means that we name first the row (y value), and then the column (x value)

# Iterate through arrays to populate matrix
for (i in 1:length(CDR_cost_ton)){
  for (j in 1:length(SAF_cost_Liter)){
    diff_cost_ton[i, j] <- (((CDR_cost_ton[i]/1000) * ton_CDR) - (SAF_cost_Liter[j] * ton_SAF))
  }
}
  # CDR - SAF means
  # When the value is POSITIVE, mitigating with SAF is cheaper
  # When the value is NEGATIVE, mitigating with CDR is cheaper

  # contour(z)
  fig_ton_km <- plot_ly(x = SAF_cost_Liter, #This is the column in matrix
                         y = CDR_cost_ton, #This is the row in matrix
                         z = diff_cost_ton,
                         type = "contour",
                         colors = colors_figures_3,
                         colorbar = list(bordercolor = "pink",
                                         title = "Cost Difference ($/ton-km-eq)",
                                         len = 0.5),
                         line = list(color = "white"),
                         contours = list(
                           showlabels = TRUE,
                           coloring = "heatmap",
                           size = 0.3,
                           start = -1.2,
                           end = 1.2
                         )) %>%

    layout(title = "CDR vs. SAF Cost per ton-kilometer-equivalent (CDR - SAF)",
           xaxis = list(
             title = "SAF Cost ($ / L)",
             nticks = 15
           ),
           yaxis = list(
             title = "Carbon Removal Cost ($ / ton CO2)",
             nticks = 15
           ),
           margin = list (
             b = 60,
             t = 60,
             l = 60,
             r = 60
           )
    )

  #Print figure
  fig_ton_km

  # Save the figure as both PNG and vector file to modify in Illustrator
  orca(fig_ton_km, "fig_ton_km.png")
  orca(fig_ton_km, "fig_ton_km.svg")
  
  
  
