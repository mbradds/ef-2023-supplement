library(pacman)
p_load(pacman,tidyverse,readxl,janitor,stringr,fs,lubridate,writexl,
       here,multiplex,scales,patchwork,viridis,ggrepel,gganimate,gifski,scales,ggpubr,usethis,rvest,
       unheadr,lattice,usethis,curl,DBI,RSQLite,sourcetools,purrr,odbc,treemap,treemapify,svglite)

# import CER palette approved by UX
my_pallet <- read_excel(here ("F:/bucom/Energy Futures 2/charts2023","palette_cer.xlsx"),sheet = "master")
cer_colours <- my_pallet$colour
# To run the french charts, change the my_pallet variable to: my_pallet$varible_french
names(cer_colours) <- my_pallet$varible_english
# generic palette
cer_palette <- c("#054169","#FFBE4B","#5FBEE6","#559B37","#FF821E","#871455","#8c8c96","#5c78ad","#42464B","#CC37B0","#FA86AC",
                 "#054169","#FFBE4B","#5FBEE6","#330000","#333300","#666633","#990033")
# line size for all the line charts made with geom_line
line_size = 0.55

cer_theme <- function(){
  theme_bw()+
    theme(
      legend.position = "bottom",
      legend.margin=margin(c(0,0,0,0),unit="cm"),
      legend.text = element_text(colour="black", size = 5),
      legend.title = element_blank(),
      legend.key.size = unit(2, 'mm'),
      legend.spacing.x = unit(0.40, "mm"),
      legend.spacing.y = unit(0.10, "mm"),
      panel.border = element_blank(), axis.line.y = element_line(),
      plot.subtitle = element_blank(),
      plot.margin = margin(.25, .75, .25, .75, "cm"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour = "grey", size = 0.5),
      axis.title.x = element_blank(),
      axis.text.x = element_text(margin = margin(t = .1, unit = "cm")),
      axis.ticks.length = unit(0.1,"cm"),
      axis.line.x = element_line(color = "black"),
      axis.title.y = element_text(size = 5, colour="black",margin = margin(r = 10)),
      axis.text = element_text(size = 5, colour="black",margin = margin(t = 10, b = 10)),
      strip.background = element_blank()
    )
}

save_charts <- function(chart_name, chart, lang="en", c_height=2.5, c_width=6) {
  # formats <- list(".png", ".svg")
  formats <- list(".png")
  for (format in formats) {
    ggsave(here("ef_2023_supplement", "charts", lang, paste(chart_name, format, sep="")),
           plot=chart,
           width = c_width,
           height = c_height,
           units = "in",
           limitsize = TRUE)
  }
}

delete_files_with_extensions <- function(folder_path) {
  file_list <- list.files(folder_path, pattern = "\\.png$|\\.svg$", full.names = TRUE)
  if (length(file_list) > 0) {
    for (file in file_list) {
      unlink(file)
    }
    print("Files with .png or .svg extension deleted successfully.")
  } else {
    print("No files with .png or .svg extension found in the folder.")
  }
}

for (lang in c("en", "fr")) {
  delete_files_with_extensions(here("ef_2023_supplement", "charts", lang))
}

get_color_list <- function(data, column) {
  unique_values <- length(unique(data[[column]]))
  colors = cer_palette[1:unique_values]
  return(colors)
}

filter_dataframe <- function(data, column_filters) {
  filtered_data <- data
  for (col_filter in column_filters) {
    col_name <- col_filter$column
    if (col_filter$type == "equal") {
      filtered_data <- filtered_data[filtered_data[[col_name]] %in% col_filter$values, ]
    } else if (col_filter$type == "not_equal") {
      filtered_data <- filtered_data[!filtered_data[[col_name]] %in% col_filter$values, ]
    }
  }
  return(filtered_data)
}

create_line_with_1_dash <- function(df, dashed_category, chart_name, legend_pos, y_min, y_max, lang="en") {
  df$Value <- round(df$Value, 0)
  chart <- df |> 
    ggplot(aes(Year,Value,colour=Item,linetype = ifelse(Item == dashed_category, "solid", "dashed")))+
    geom_line(size = line_size, key_glyph = "rect")+
    geom_text(data = subset(df, Item == dashed_category), aes(label = ifelse(Year == min(Year), Value, "")), hjust = 0, vjust = -0.5, size = 2) +
    geom_text(data = subset(df, Item != dashed_category), aes(label = ifelse(Year == max(Year), Value, "")), hjust = -0.5, size = 2) +
    scale_y_continuous(breaks = seq(y_min, y_max, by = 20), expand = c(0, 0), limit = c(y_min , y_max)) +
    cer_theme()+
    scale_color_manual(values = get_color_list(df, "Item"), limits = force)+
    theme(legend.position = legend_pos)+
    guides(linetype = "none")+
    labs(y="2021 = 100")
  save_charts(chart_name, chart, lang)
}

create_double_x_axis <- function(df, y_axis_title, plot_name, y_min, y_max, scale_jump, custom_order=NULL, color_list=NULL, lang="en") {
  if (is.null(color_list)) {
    color_list <- cer_palette 
  }
  if (!is.null(custom_order)) {
    df$x2 <- factor(df$x2, levels = custom_order)
  }
  
  chart <- ggplot(df, aes(x1, Value, fill=name, label = x1)) +
    geom_bar(position="stack", stat="identity") +
    facet_wrap(~x2, strip.position = "bottom", scales = "free_x", nrow=1) +
    scale_fill_manual(values = cer_palette, limits = force) +
    labs(x="", y=y_axis_title) +
    cer_theme() +
    theme(panel.spacing = unit(0, "lines"), 
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 5)) +
    scale_y_continuous(breaks = seq(y_min, y_max, by = scale_jump), expand = c(0, 0), limit = c(y_min , y_max), labels = scales::number_format(accuracy = 1))
  
  save_charts(plot_name, chart, lang)
}

create_gnz_plot_2 <- function(df, item_order, type_order, y_label="", chart_name="3_option_2", lang="en"){
  df <- pivot_longer(df, cols = c(r, n),
                     names_to = "Emission_Type", values_to = "Value")
  
  if(lang=="en"){
    df$Emission_Type[df$Emission_Type == "r"] <- "Remaining_emissions"
    df$Emission_Type[df$Emission_Type == "n"] <- "Negative_emissions"
  } else {
    df$Emission_Type[df$Emission_Type == "r"] <- "Émissions restantes de GES"
    df$Emission_Type[df$Emission_Type == "n"] <- "Émissions négatives de GES"
  }
  
  df$Emission_Type <- factor(df$Emission_Type, levels = type_order)
  df = filter_dataframe(df, list(list(column = "Item", type = "not_equal", values = c("Total", "Émissions négatives totales de GES"))))
  df$Item <- factor(df$Item, levels = item_order)
  df$Emission_Type <- gsub("_", " ", df$Emission_Type)
  
  chart <- ggplot(df, aes(x = Emission_Type, y = Value, fill = Item)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = 0, color = "black", size = 0.5) +
    labs(y = y_label, x = "Item") +
    scale_y_continuous(breaks = seq(-175, 175, by = 25), expand = c(0, 0), labels = number, limit=c(-175, 175)) +
    scale_fill_manual(values = get_color_list(df_gnz2, "Item"), limits = force) +
    cer_theme() +
    theme(legend.position = "right")
  save_charts(chart_name, chart, lang)
}

create_line_chart <- function(df, chart_name, y_min, y_max, scale_jump, y_axis_title="", color_list=NULL, custom_order=NULL, c_width=6, lang="en") {
  if (!is.null(custom_order)) {
    df$Type <- factor(df$Type, levels = custom_order)
  }
  # color/category column must be named "Type"
  if (is.null(color_list)) {
    color_list <- get_color_list(df, "Type")  
  }
  chart <- ggplot(df, aes(x = Year, y = Value, group = Type, color = Type)) +
    geom_line(size = line_size) +
    scale_color_manual(values = color_list, limits = force) +
    scale_y_continuous(breaks = seq(y_min, y_max, by = scale_jump), expand = c(0, 0), limit = c(y_min , y_max),) +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    labs(y=y_axis_title, color = "Type") +
    cer_theme()
  
  save_charts(chart_name, chart, lang=lang, c_width=c_width)
}

create_area_chart <- function(df, chart_name, y_min, y_max, y_axis_title="", custom_order=NULL, color_list=NULL, lang="en", c_width=6) {
  if (is.null(color_list)) {
    color_list <- get_color_list(df, "Type")  
  }
  if (!is.null(custom_order)) {
    df$Type <- factor(df$Type, levels = custom_order)
  }
  # color/category column must be named "Type"
  chart <- ggplot(df, aes(x = Year, y = Value, fill = Type)) +
    geom_area() +
    scale_fill_manual(values = color_list, limits = force) +
    scale_x_continuous(breaks = pretty_breaks(n = 10),expand = c(0, 0)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10),expand = c(0, 0),labels = scales::number_format(accuracy = 1), limit = c(y_min, y_max))+
    labs(x = "", y = y_axis_title, fill = "Type") +
    cer_theme()
  
  if (grepl("12_gas_production_vintage_", chart_name) && lang == "fr"){
    chart <- chart + guides(fill = guide_legend(nrow = 2))
  }
  if (chart_name == "16_propane" && lang == "fr"){
    chart <- chart + guides(fill = guide_legend(ncol = 2))
  }
  if (chart_name == "16_propane" && lang == "en"){
    chart <- chart + guides(fill = guide_legend(nrow = 3))
  }
  
  save_charts(chart_name, chart, lang, c_width=c_width)
}

create_electricity_chart <- function(df, scenarios, chart_name, y_max, lang="en", y_text="Gigawatt (GW)") {
  names(df)[names(df) == "Fuel/Technology"] <- "FuelTechnology"
  chart <- ggplot(df, aes(x= factor(Year), y = y_col, fill = FuelTechnology))+
    geom_col()+
    theme(legend.spacing.y = unit(0.1, "cm")) +
    scale_fill_manual(values = cer_colours, limits = force)+
    scale_y_continuous(breaks = pretty_breaks(n = 10),expand= c(0,0), limits=c(0, y_max), labels = number_format(accuracy = 1)) +
    cer_theme()+
    labs(x="Year", y=y_text)+
    facet_wrap(~factor(Scenario, scenarios))+
    theme(strip.background = element_blank())+
    theme(strip.text.x = element_text(size = 5))
  save_charts(chart_name, chart, lang=lang)
}

create_oil_sands_emission_chart <- function(df,
                                            solid_line,
                                            dashed_line,
                                            chart_name,
                                            y_min,
                                            y_max,
                                            y_axis_title = "",
                                            custom_order=NULL,
                                            color_list=NULL,
                                            lang="en",
                                            c_width=6) {

  if (!is.null(custom_order)) {
    df$TypeSector <- factor(df$TypeSector, levels = custom_order)
  }
  if (is.null(color_list)) {
    color_list <- get_color_list(df, "TypeSector")  
  }
  # Create the chart with stacked areas and lines
  line_list <- c(dashed_line, solid_line)
  linetype_map <- setNames(c(1, 3), c(solid_line, dashed_line))
  # Separate dataset for stacked area with reduced opacity
  area_df <- subset(df, !TypeSector %in% line_list)
  
  chart <- ggplot() +
    # Stacked area with modified opacity
    geom_area(data = area_df, aes(x = Year, y = Value, fill = TypeSector),
              position = "stack") +
    geom_line(data = subset(df, TypeSector %in% line_list),
              aes(x = Year, y = Value, group = TypeSector, linetype = TypeSector),
              size = line_size) +
    scale_x_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0), labels = number, limits=c(y_min, y_max)) +
    scale_fill_manual(values = color_list, limits = force) +
    scale_linetype_manual(values = linetype_map) +  # Use the named vector for different linetypes
    labs(x = "Year", y = y_axis_title, fill = "TypeSector", linetype = "TypeSector") +
    guides(linetype = guide_legend(nrow = 2, label.position = "right", title.position = "top")) +
    cer_theme()
  
  if (grepl("10_oil_sands_emissions_", chart_name) && lang == "fr") {
    chart <- chart + guides(fill = guide_legend(nrow = 3))
  }
  save_charts(chart_name, chart, lang, 2.5, c_width)
}

create_gas_prod_chart <- function(df, df_price, chart_name, y_axis_label="", lang="en", c_width=6){
  chart <- ggplot() +
    geom_area(data = df, aes(Year, Value, fill = Variable)) +
    geom_line(data = df_price, aes(y = Value*4, x = Year, colour = "black"), size = line_size, linetype="solid") + 
    scale_fill_manual(values = cer_colours , limits = force) +
    scale_color_manual(values = c('black'), labels = c('AB Reference Gas Price, 2022 C$ per mmBtu')) +
    scale_x_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(~./4, name = "AB Reference Gas Price, 2022 C$ per mmBtu"),
                       labels = function(x) x,
                       breaks = c(0, 5, 10, 15, 20, 25)) +
    labs(y = y_axis_label) +
    cer_theme()
  if (lang == "fr") {
    chart <- chart + guides(fill = guide_legend(nrow = 5))
  }
  chart <- chart + theme(legend.box = "vertical")
  save_charts(chart_name, chart, lang, c_width=c_width)
}

create_plots <- function(df) {
  # Get unique scenarios from the "Scenario" column
  scenarios <- unique(df$Sector)
  # Create a list of plots using lapply()
  plots <- lapply(scenarios, function(scenario) {
    plot_data <- df %>%
      filter(Sector == scenario)
    list(data = plot_data, title = scenario)
  })
  return(plots)
}

create_multiple_area <- function(df, y_max, y_axis_title, chart_name, stack_order, lang="en") {
  plots <- create_plots(df)
  plot_list <- map(plots, function(plot) {
    ggplot(plot$data, aes(Year, Value, fill=factor(Fuel, levels = stack_order))) +
      geom_area() +
      ggtitle(plot$title) +
      scale_fill_manual(values = cer_colours, limits = force) +
      scale_x_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0), labels = number, limit = c(0, y_max)) +
      cer_theme() +
      theme(legend.text = element_text(margin = margin(r = 2, unit = "pt"))) +
      theme(legend.position = "bottom") +
      theme(plot.title = element_text(size=5))+
      theme(plot.title = element_text(hjust = 0.5))  +
      labs(x = "Year", y = y_axis_title)
  })
  
  apply_theme_to_plots <- function(plot_list) {
    n_plots <- length(plot_list)
    # Loop through each plot in the plot_list
    for (i in 1:n_plots) {
      if (i == 1) {
        plot_list[[i]] <- plot_list[[i]] +
          theme(axis.title.y = element_text(size = 5, colour = "black", margin = margin(r = 0.5)))
      }
      if (i > 1) {
        plot_list[[i]] <- plot_list[[i]] +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank())
      }
    }
    # Combine the plots using plot_layout
    combined_plot <- plot_list[[1]]
    for (i in 2:n_plots) {
      combined_plot <- combined_plot + plot_list[[i]]
    }
    # Apply the final modifications and return the plot
    combined_plot <- combined_plot +
      plot_layout(guides = "collect") & theme(legend.position = 'bottom', plot.margin = margin(0.50, 0.25, 0.25, 0.10, "cm"))
    return(combined_plot)
  }
  plot <- apply_theme_to_plots(plot_list)
  save_charts(chart_name, plot, lang)
}

# TODO reuse some of these inner functions between create_multiple_area and create_multiple_line
create_multiple_line <- function(df, y_max, y_axis_title, chart_name) {
  plots <- create_plots(df)
  # create a list of plots
  plot_list <- map(plots, function(plot) {
    ggplot(plot$data, aes(Year, Value, fill=Type, color = Type)) +
      geom_line(size = line_size) +
      scale_color_manual(values = get_color_list(plot$data, "Type")) +
      ggtitle(plot$title) +
      scale_x_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0)) +
      scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0), labels = number, limit = c(0, y_max)) +
      cer_theme() +
      theme(legend.position = "bottom") +
      theme(plot.title = element_text(size=5))+
      theme(plot.title = element_text(hjust = 0.5))  +
      labs(x = "Year", y = y_axis_title) +
      guides(color = guide_legend(ncol = 1))
  })
  
  apply_theme_to_plots <- function(plot_list) {
    n_plots <- length(plot_list)
    # Loop through each plot in the plot_list
    for (i in 1:n_plots) {
      plot_list[[i]] <- plot_list[[i]] + guides(color = guide_legend(ncol = 1))
      if (i == 1) {
        plot_list[[i]] <- plot_list[[i]] +
          theme(axis.title.y = element_text(size = 5, colour = "black", margin = margin(r = 0.5)))
      }
      if (i > 1) {
        plot_list[[i]] <- plot_list[[i]] +
          theme(axis.text.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.title.y = element_blank())
      }
    }
    # Combine the plots using plot_layout
    combined_plot <- plot_list[[1]]
    for (i in 2:n_plots) {
      combined_plot <- combined_plot + plot_list[[i]]
    }
    # this makes sure each legend is below its chart!
    combined_plot <- combined_plot & plot_layout(guides = "keep") & 
      theme(legend.position = 'bottom', plot.margin = margin(0.50, 0.25, 0.25, 0.10, "cm"))
    return(combined_plot)
  }
  
  plot <- apply_theme_to_plots(plot_list)
  save_charts(chart_name, plot)
}

get_data <- function(sheet, range) {
  df <- read_excel(here("EF2023_charts", "Chart Data for Fact Sheet.xlsx"), sheet = sheet, range=range)
  if ("Year" %in% colnames(df)) {
    df$Year <- as.numeric(df$Year)
  }
  if ("Value" %in% colnames(df)) {
    df$Value <- as.numeric(df$Value)
  }
  return(df)
}

translate_df <- function(df, translations) {
  for (pair in translations) {
    # Check if both columns are in the dataframe
    if (pair[[1]] %in% colnames(df) && pair[[2]] %in% colnames(df)) {
      # Rename the second column to the name of the first column
      colnames(df)[colnames(df) == pair[[2]]] <- paste0(pair[[1]], "_temp")
      # Remove the first column
      df <- df[, !(colnames(df) == pair[[1]])]
      # Rename the temporarily renamed second column to the name of the first column
      colnames(df)[colnames(df) == paste0(pair[[1]], "_temp")] <- pair[[1]]
    }
  }
  return(df)
}


# start charts
# 1) End Use Energy Intensity Trends by end-use subsector, Global Net-zero Scenario
df_euei <- get_data(sheet = "End-use energy intensities", range="B3:G59")
create_line_with_1_dash(df_euei, "Baseline", "1_end_use_energy_intensities", "bottom", 0, 120)
# French
df_euei <- translate_df(df_euei, list(list("Scenario", "Scenario (FR)"), list("Item", "Item (FR)")))
df_euei$Item <- factor(df_euei$Item, levels=c("Données de base", "Bâtiments commerciaux", "Transport de marchandises", "Industries lourdes", "Secteur pétrolier et gazier", "Autres industries", "Transport de passagers", "Bâtiments résidentiels")) 
create_line_with_1_dash(df_euei, "Baseline", "1_end_use_energy_intensities", "bottom", 0, 120, "fr")


# 2) Kaya Identity or Trends in key drivers of GHG emissions levels, Global Net-zero Scenario
df_ki <- get_data(sheet = "Kaya identity", range="B3:G45")
create_line_with_1_dash(df_ki, "Baseline", "2_kaya_identity", "bottom", -20, 140)
# French
df_ki <- translate_df(df_ki, list(list("Item", "FR"), list("Scenario", "Scenario (FR)")))
df_ki$Item <- factor(df_ki$Item, levels=c("Données de base","Intensité des émissions de carbone", "Intensité énergétique", "PIB par habitant", "Émissions nettes de GES", "Population")) 
create_line_with_1_dash(df_ki, "Baseline", "2_kaya_identity", "bottom", -20, 140, "fr")


# 3) 2050 GHG emissions in Global Net-zero (second option)
df_gnz2 <- get_data(sheet = "2050 GHG emissions in GNZ", range="K2:N13")
names(df_gnz2)[names(df_gnz2) == "Remaining_emissions"] <- "r"
names(df_gnz2)[names(df_gnz2) == "Negative_emissions"] <- "n"
gnz_order <- c("Agriculture", "Buildings", "Heavy Industry", "Oil and Gas", "Transportation", "Waste and Others", "Low or Non-emitting Hydrogen Production", "Direct Air Capture", "Electricity", "Land Use, Land Use Change and Forestry")
item_order <- c("Remaining_emissions", "Negative_emissions")
create_gnz_plot_2(df_gnz2, gnz_order, item_order, "Megatonnes of CO2e")
# French
df_gnz2 <- translate_df(df_gnz2, list(c("Item", "Item (FR)")))
gnz_order <- c("Agriculture", "Bâtiments", "Industrie lourde", "Pétrole et gaz", "Transport", "Déchets et autres", "Production d’hydrogène à émissions faibles ou nulles", "Captage direct dans l’air", "Électricité", "Utilisation des sols, changement d’affectation des sols et foresterie")
item_order <- c("Émissions restantes de GES", "Émissions négatives de GES")
create_gnz_plot_2(df_gnz2, gnz_order, item_order, "Mégatonnes d’éq. CO2", lang="fr")


# 4) Global Net-zero Bioenergy Demand by Type and Sector
df_bio <- get_data(sheet = "Bioenergy Demand by Type", range="S2:X50")
names(df_bio)[names(df_bio) == "Year"] <- "x1"
names(df_bio)[names(df_bio) == "Sector"] <- "x2"
names(df_bio)[names(df_bio) == "Fuel"] <- "name"
bio_order = c("Commercial", "Residential", "Transportation", "Industrial")
create_double_x_axis(df_bio, "Petajoules (PJ)", "4_bioenergy_demand", 0, 800, 100, bio_order, cer_colours)
# French
df_bio <- translate_df(df_bio, list(list("x2", "Sector (French)"), list("name", "Fuel (French)")))
bio_order = c("Commercial", "Résidentiel", "Transport", "Industriel")
create_double_x_axis(df_bio, "Pétajoules", "4_bioenergy_demand", 0, 800, 100, bio_order, cer_colours, lang="fr")


# 5) Total Carbon Captured and Removed Excluding Nature-Based Solutions
df_ccs <- read_excel(here("EF2023_charts", "Chart Data for Fact Sheet.xlsx"), sheet = "Total CCS", range="K2:P52")
names(df_ccs)[names(df_ccs) == "Scenario"] <- "x1"
names(df_ccs)[names(df_ccs) == "Year"] <- "x2"
names(df_ccs)[names(df_ccs) == "Sector"] <- "name"
df_ccs$x1 <- gsub("Current Measures", "CM", df_ccs$x1)
df_ccs$x1 <- gsub("Canada Net-zero", "CNZ", df_ccs$x1)
df_ccs$x1 <- gsub("Global Net-zero", "GNZ", df_ccs$x1)
df_ccs$x1 <- factor(df_ccs$x1, levels = c("History", "GNZ", "CNZ", "CM"))
create_double_x_axis(df_ccs, "Megatonnes (MT)", "5_total_ccs", 0, 250, 50)
# French
df_ccs <- translate_df(df_ccs, list(list("x1", "Scenario (French)"), list("name", "Sector (French)")))
df_ccs$x1 <- gsub("Mesures actuelles", "MA", df_ccs$x1)
df_ccs$x1 <- gsub("Carboneutralité au Canada", "CEM", df_ccs$x1)
df_ccs$x1 <- gsub("Carboneutralité à l'échelle mondiale", "CC", df_ccs$x1)
create_double_x_axis(df_ccs, "Mégatonnes", "5_total_ccs", 0, 250, 50, NULL, NULL, "fr")


# 6) Global Net-zero Passenger vs Freight Transport
df_transport <- get_data(sheet = "Passenger vs Freight Transport", range="R23:W391")
create_multiple_area(df_transport, 1800, "Petajoules (PJ)", "6_passenger_freight_transportation", c("RPPs", "Hydrogen", "Electricity", "Biofuels"))
# French
df_transport <- translate_df(df_transport, list(list("Sector", "Sector (French)"), list("Fuel", "Fuel (French)")))
create_multiple_area(df_transport, 1800, "Pétajoules", "6_passenger_freight_transportation", c("PPRs", "Hydrogène", "Électricité", "Biocarburant"), "fr")


# 7) Global Net-zero Indexed Oil Sands production and Emissions Intensity
df_osp <- get_data(sheet = "Oil Sands Prod and Emi Int", range="A1:G211")
df_osp_gnz = filter_dataframe(df_osp, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
df_osp_cnz = filter_dataframe(df_osp, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
df_osp_cm = filter_dataframe(df_osp, list(list(column = "Scenario", type = "equal", values = c("Current Measures"))))
create_line_chart(df_osp_gnz, "7_oil_sands_production_gnz", 0, 160, 20, "2016 = 100", c_width = 4)
create_line_chart(df_osp_cnz, "7_oil_sands_production_cnz", 0, 160, 20, "2016 = 100", c_width = 4)
create_line_chart(df_osp_cm, "7_oil_sands_production_cm", 0, 160, 20, "2016 = 100", c_width = 4)
# French
df_osp_gnz <- translate_df(df_osp_gnz, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
df_osp_cnz <- translate_df(df_osp_cnz, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
df_osp_cm <- translate_df(df_osp_cm, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
create_line_chart(df_osp_gnz, "7_oil_sands_production_gnz", 0, 160, 20, "2016 = 100", c_width = 4, lang="fr")
create_line_chart(df_osp_cnz, "7_oil_sands_production_cnz", 0, 160, 20, "2016 = 100", c_width = 4, lang="fr")
create_line_chart(df_osp_cm, "7_oil_sands_production_cm", 0, 160, 20, "2016 = 100", c_width = 4, lang="fr")


# 8) Global Net-zero Oil Sands Production with CCUS
df_osp_ccus <- get_data(sheet = "Oil Sands Prod with CCUS", range="A1:G211")
df_osp_ccus_gnz = filter_dataframe(df_osp_ccus, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
df_osp_ccus_cnz = filter_dataframe(df_osp_ccus, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
create_area_chart(df_osp_ccus_gnz, "8_oil_sands_production_ccus_gnz", 0, 4000, "Thousand barrels per day (mb/d)", c("Production without CCUS", "Production with CCUS"), c_width = 4)
create_area_chart(df_osp_ccus_cnz, "8_oil_sands_production_ccus_cnz", 0, 4000, "Thousand barrels per day (mb/d)", c("Production without CCUS", "Production with CCUS"), c_width = 4)
# French
df_osp_ccus_gnz <- translate_df(df_osp_ccus_gnz, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
df_osp_ccus_cnz <- translate_df(df_osp_ccus_cnz, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
create_area_chart(df_osp_ccus_gnz, "8_oil_sands_production_ccus_gnz", 0, 4000, "Milliers de barils par jour (kb/j)", c("Production sans CSC", "Production avec CSC"), c_width = 4, lang="fr")
create_area_chart(df_osp_ccus_cnz, "8_oil_sands_production_ccus_cnz", 0, 4000, "Milliers de barils par jour (kb/j)", c("Production sans CSC", "Production avec CSC"), c_width = 4, lang="fr")


# 9) Western Canada Conventional Oil Production for Global Net-zero from 2000-2050
df_conv_oil = get_data("WC Conv Oil Prod FLAT", "A1:H511")
df_conv_oil$TypeSector <- paste(df_conv_oil$Province, df_conv_oil$Class)
df_conv_oil$Value <- df_conv_oil$Value / 1000
df_conv_oil <- df_conv_oil[df_conv_oil$Year > 2009, ]
create_oil_sands_emission_chart(df_conv_oil, "Canada Current Measures Total", "Canada Canada Net-zero", "9_wc_conv_oil_prod", 0, 1800, "Thousand barrels per day (mb/d)")
# French
df_conv_oil <- translate_df(df_conv_oil, list(c("Scenario", "Scenario_french"), c("Province", "Province_french"), c("Class", "Class_french")))
df_conv_oil$TypeSector <- paste(df_conv_oil$Province, df_conv_oil$Class)
df_conv_oil$TypeSector <- trimws(df_conv_oil$TypeSector)
df_conv_oil$TypeSector <- factor(df_conv_oil$TypeSector, levels=c("AB Lourd", "AB Léger", "BC Lourd", "BC Léger", "MB Lourd", "MB Léger", "SK Lourd", "SK Léger", "Canada Carboneutralité au Canada", "Canada Mesures actuelles totale"))
create_oil_sands_emission_chart(df_conv_oil, "Canada Mesures actuelles totale", "Canada Carboneutralité au Canada", "9_wc_conv_oil_prod", 0, 1800, "Milliers de barils par jour (kb/j)", lang="fr")


# 10) Carbon Dioxide Captured from Oil & Gas Facilities for all Scenarios
df_os_ccs <- get_data("CCS captured from Oil Sands", "A1:H1243")
df_os_ccs$TypeSector <- paste(df_os_ccs$Type, df_os_ccs$Sector)
df_os_ccs = filter_dataframe(df_os_ccs, list(list(column = "TypeSector", type = "not_equal", values = c("Abbated Emissions TOTAL"))))
df_os_ccs_gnz <- filter_dataframe(df_os_ccs, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
df_os_ccs_cnz <- filter_dataframe(df_os_ccs, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
df_os_ccs_cm <- filter_dataframe(df_os_ccs, list(list(column = "Scenario", type = "equal", values = c("Current Measures"))))
# create the charts
create_oil_sands_emission_chart(df_os_ccs_gnz, "Emissions TOTAL", "Emitted Emissions TOTAL", "10_oil_sands_emissions_gnz", 0, 200000, "Megatonnes", NULL, cer_colours, c_width = 4)
create_oil_sands_emission_chart(df_os_ccs_cnz, "Emissions TOTAL", "Emitted Emissions TOTAL", "10_oil_sands_emissions_cnz", 0, 200000, "Megatonnes", NULL, cer_colours, c_width = 4)
create_oil_sands_emission_chart(df_os_ccs_cm, "Emissions TOTAL", "Emitted Emissions TOTAL", "10_oil_sands_emissions_cm", 0, 200000, "Megatonnes", NULL, cer_colours, c_width = 4)
# French
df_os_ccs_gnz <- translate_df(df_os_ccs_gnz, list(c("Type", "Type (FR)"), c("Scenario", "Scenario (FR)"), c("Sector", "Sector FR")))
df_os_ccs_gnz$TypeSector <- paste(df_os_ccs_gnz$Type, df_os_ccs_gnz$Sector)
df_os_ccs_cnz <- translate_df(df_os_ccs_cnz, list(c("Type", "Type (FR)"), c("Scenario", "Scenario (FR)"), c("Sector", "Sector FR")))
df_os_ccs_cnz$TypeSector <- paste(df_os_ccs_cnz$Type, df_os_ccs_cnz$Sector)
df_os_ccs_cm <- translate_df(df_os_ccs_cm, list(c("Type", "Type (FR)"), c("Scenario", "Scenario (FR)"), c("Sector", "Sector FR")))
df_os_ccs_cm$TypeSector <- paste(df_os_ccs_cm$Type, df_os_ccs_cm$Sector)
create_oil_sands_emission_chart(df_os_ccs_gnz, "Émissions Totale", "Émissions dégagées Totale", "10_oil_sands_emissions_gnz", 0, 200000, "Mégatonnes", NULL, cer_colours, lang="fr", c_width = 4)
create_oil_sands_emission_chart(df_os_ccs_cnz, "Émissions Totale", "Émissions dégagées Totale", "10_oil_sands_emissions_cnz", 0, 200000, "Mégatonnes", NULL, cer_colours, lang="fr", c_width = 4)
create_oil_sands_emission_chart(df_os_ccs_cm, "Émissions Totale", "Émissions dégagées Totale", "10_oil_sands_emissions_cm", 0, 200000, "Mégatonnes", NULL, cer_colours, lang="fr", c_width = 4)


# 11) Gas Production by well area and type of gas, and gas prices, for all scenarios
df_gas_prod <- get_data("Gas Production FLAT", "A1:F1837")
df_gas_price_cm = filter_dataframe(df_gas_prod, list(
  list(column = "Scenario", type = "equal", values = c("Current Measures")),
  list(column = "Variable", type = "equal", values = c("AB Reference Gas Price, 2022 C$ per mmBtu"))
))
df_gas_price_cnz = filter_dataframe(df_gas_prod, list(
  list(column = "Scenario", type = "equal", values = c("Canada Net-zero")),
  list(column = "Variable", type = "equal", values = c("AB Reference Gas Price, 2022 C$ per mmBtu"))
))
df_gas_price_gnz = filter_dataframe(df_gas_prod, list(
  list(column = "Scenario", type = "equal", values = c("Global Net-zero")),
  list(column = "Variable", type = "equal", values = c("AB Reference Gas Price, 2022 C$ per mmBtu"))
))
# filter out the gas price
df_gas_prod <- filter_dataframe(df_gas_prod, list(list(column = "Variable", type = "not_equal", values = c("AB Reference Gas Price, 2022 C$ per mmBtu"))))
# filter for each scenario
df_gas_prod_cm <- filter_dataframe(df_gas_prod, list(list(column = "Scenario", type = "equal", values = c("Current Measures"))))
df_gas_prod_cnz <- filter_dataframe(df_gas_prod, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
df_gas_prod_gnz <- filter_dataframe(df_gas_prod, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
# create the charts for each scenario
create_gas_prod_chart(df_gas_prod_cm, df_gas_price_cm, "11_gas_prod_cm", "Billion cubic feet per day (BCF/d)", c_width = 4)
create_gas_prod_chart(df_gas_prod_cnz, df_gas_price_cnz, "11_gas_prod_cnz", "Billion cubic feet per day (BCF/d)", c_width = 4)
create_gas_prod_chart(df_gas_prod_gnz, df_gas_price_gnz, "11_gas_prod_gnz", "Billion cubic feet per day (BCF/d)", c_width = 4)
# French
translate_gp <- function(df){
  df$Scenario <- NULL
  names(df)[names(df) == "scenario_french"] <- "Scenario"
  df$Variable <- NULL
  names(df)[names(df) == "Variable_french"] <- "Variable"
  return(df)
}
y_label_fr = "Milliard de pieds cubes par jour (« Gpi³/j »)"
french_order <- c("Prix de référence en Alberta ($ CA 2022/MBTU)","Réservoirs étanches, Deep Basin, AB","Réservoirs étanches, Montney, AB","Réservoirs étanches, Montney, BC","Schistes, Duvernay","Schistes, Horn River","Autres schistes, Ouest canadien","Autres réservoirs étanches, Ouest canadien","Classique - Reste du Canada","Méthane de houille - Ouest canadien","Classique - Ouest canadien","Gaz dissous - Ouest canadien")
df_gas_prod_cm$Variable_french <- factor(df_gas_prod_cm$Variable_french, levels=french_order)
df_gas_prod_cnz$Variable_french <- factor(df_gas_prod_cnz$Variable_french, levels=french_order)
df_gas_prod_gnz$Variable_french <- factor(df_gas_prod_gnz$Variable_french, levels=french_order)
create_gas_prod_chart(translate_gp(df_gas_prod_cm), translate_gp(df_gas_price_cm), "11_gas_prod_cm", y_label_fr, "fr", c_width = 4)
create_gas_prod_chart(translate_gp(df_gas_prod_cnz), translate_gp(df_gas_price_cnz), "11_gas_prod_cnz", y_label_fr, "fr", c_width = 4)
create_gas_prod_chart(translate_gp(df_gas_prod_gnz), translate_gp(df_gas_price_gnz), "11_gas_prod_gnz", y_label_fr, "fr", c_width = 4)


# 12) Gas Production by well vintage for all scenarios
df_gas_prod_vint = get_data("Gas Prod Vintage FLAT", "A1:F433")
df_gas_prod_vint$Value <- round(df_gas_prod_vint$Value, 2)
names(df_gas_prod_vint)[names(df_gas_prod_vint) == "Variable"] <- "Type"
df_gas_prod_vint_cm <- filter_dataframe(df_gas_prod_vint, list(list(column = "Scenario", type = "equal", values = c("Current Measures"))))
df_gas_prod_vint_cnz <- filter_dataframe(df_gas_prod_vint, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
df_gas_prod_vint_gnz <- filter_dataframe(df_gas_prod_vint, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
custom_order = c("Additional Production for LNG Export","Projected Wells Given Gas Price","Existing Wells", "Solution Gas")
create_area_chart(df_gas_prod_vint_cm, "12_gas_production_vintage_cm", 0, 25, "Billion cubic feet per day (Bcf/d)", custom_order, cer_colours, c_width = 4)
create_area_chart(df_gas_prod_vint_cnz, "12_gas_production_vintage_cnz", 0, 25, "Billion cubic feet per day (Bcf/d)", custom_order, cer_colours, c_width = 4)
create_area_chart(df_gas_prod_vint_gnz, "12_gas_production_vintage_gnz", 0, 25, "Billion cubic feet per day (Bcf/d)", custom_order, cer_colours, c_width = 4)
# French
df_gas_prod_vint_cm <- translate_df(df_gas_prod_vint_cm, list(list("Scenario", "Scenario_french"), list("Type", "Variable_french")))
df_gas_prod_vint_cnz <- translate_df(df_gas_prod_vint_cnz, list(list("Scenario", "Scenario_french"), list("Type", "Variable_french")))
df_gas_prod_vint_gnz <- translate_df(df_gas_prod_vint_gnz, list(list("Scenario", "Scenario_french"), list("Type", "Variable_french")))
custom_order = c("Production supplémentaire pour GNL","Puits projetés compte tenu du prix du gaz","Puits existants", "Gaz dissous")
create_area_chart(df_gas_prod_vint_cm, "12_gas_production_vintage_cm", 0, 25, "Milliard de pieds cubes par jour (« Gpi³/j »)", custom_order, cer_colours, "fr", c_width = 4)
create_area_chart(df_gas_prod_vint_cnz, "12_gas_production_vintage_cnz", 0, 25, "Milliard de pieds cubes par jour (« Gpi³/j »)", custom_order, cer_colours, "fr", c_width = 4)
create_area_chart(df_gas_prod_vint_gnz, "12_gas_production_vintage_gnz", 0, 25, "Milliard de pieds cubes par jour (« Gpi³/j »)", custom_order, cer_colours, "fr", c_width = 4)


# 13 Canada gas remaining marketable resources by region at year-end 2021
# Function to add line breaks every three words (excluding one-character words)
add_line_breaks <- function(text) {
  gsub("((\\S+\\s+\\S+\\s+\\S+\\s+)|(\\S+\\s+\\S+\\s+\\S+\\s*$))", "\\1\n", text)
}
df_gas_res = get_data("Gas Resources and Cum Prod FLAT", "A1:E25")
# Create stacked bar chart
create_gas_resource <- function(df, lang="en", y_label_text="Trillion cubic feet (Tcf)") {
  df$Variable <- sapply(df$Variable, add_line_breaks)
  custom_order <- as.list(head(df$Variable, 4))
  df$Variable <- factor(df$Variable, levels = custom_order)
  plot <- ggplot(df, aes(x = Variable, y = Value, fill = Type)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = get_color_list(df_gas_res, "Type")) +
    scale_y_continuous(breaks = pretty_breaks(n = 10),expand = c(0, 0),labels = scales::number_format(accuracy = 1), limit = c(0, 1600))+
    labs(x = "", y = y_label_text, fill = "Type") +
    cer_theme()
  if (lang == "fr"){
    c_width = 7
  } else {
    c_width = 6
  }
  save_charts("13_gas_resources", plot, lang, c_width=c_width)
}
create_gas_resource(df_gas_res)
# French
df_gas_res <- translate_df(df_gas_res, list(list("Variable", "Variable_French"), list("Type", "Type_French")))
df_gas_res$Type <- factor(df_gas_res$Type, levels=c("Méthane de houille","Gaz de réservoirs étanches de la formation de Montney","Gaz d’autres réservoirs étanches","Gaz classique ailleurs au Canada","Gaz d’autres formations schisteuses","Gaz classique dans l’Ouest canadien") )
create_gas_resource(df_gas_res, "fr", "Trillion cubic feet (Tcf)")


# 14) Canada Natural Gas Exports 2010-2050 all scenarios
df_gas_exp = get_data("Gas Net Exports All FLAT", "A1:F61")
create_gas_export <- function(df, y_axis_title, custom_order, lang="en") {
  if (!is.null(custom_order)) {
    df$Scenario <- factor(df$Scenario, levels = custom_order)
  }
  plot <- ggplot(df, aes(Year, Value, label = Year)) +
    geom_bar(data = subset(df, !(Variable %in% c("Production"))), aes(fill = Variable), position = "stack", stat = "identity") +
    geom_point(data = subset(df, Variable == "Production"), aes(color = Variable), size = 4, shape = 45) +
    facet_wrap(~ Scenario, strip.position = "bottom", scales = "free_x", nrow = 1) +
    scale_fill_manual(values = c("#5FBEE6","#FFBE4B","#054169")) +
    scale_color_manual(values = "black") +
    labs(x = "", y = y_axis_title) +
    cer_theme() +
    theme(panel.spacing = unit(0, "lines"),
          strip.background = element_blank(),
          strip.placement = "outside",
          strip.text = element_text(size = 5),
          legend.spacing.y = unit(0.1, "cm")) +
    scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0), limits=c(-2, 25), labels = scales::number_format(accuracy = 1)) +
    guides(fill = guide_legend(title = "Variable",
                               override.aes = list(color = NA)),
           color = guide_legend(title = "Production",
                                override.aes = list(fill = NA, size = 2)))

  save_charts("14_gas_net_exports", plot, lang)
}
create_gas_export(df_gas_exp, "Billion cubic feet per day (Bcf/d)", c("Global Net-zero", "Canada Net-zero", "Current Measures"))
# French
df_gas_exp <- translate_df(df_gas_exp, list(list("Scenario", "Scenario_french"), list("Variable", "Variable_french")))
df_gas_exp$Variable <- factor(df_gas_exp$Variable, levels=c("Production" ,"Exportations de GNL", "Demande", "Exportations nettes vers les États-Unis"))
create_gas_export(df_gas_exp, "Milliard de pieds cubes par jour (« Gpi³/j »)", c("Carboneutralité à l’échelle mondiale", "Carboneutralité au Canada", "Mesures actuelles"), "fr")


# 15) Canadian Ethane production from 2010-2050, all scenarios
df_ethane = get_data("Ethane FLAT", "A1:F493")
names(df_ethane)[names(df_ethane) == "Variable"] <- "TypeSector"
df_ethane_cm <- filter_dataframe(df_ethane, list(list(column = "Scenario", type = "equal", values = c("Current Measures"))))
df_ethane_cnz <- filter_dataframe(df_ethane, list(list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))))
df_ethane_gnz <- filter_dataframe(df_ethane, list(list(column = "Scenario", type = "equal", values = c("Global Net-zero"))))
create_oil_sands_emission_chart(df_ethane_cm, "Total Demand", "", "15_ethane_cm", 0 , 800, "Thousand barrels per day (Mb/d)", c_width = 4)
create_oil_sands_emission_chart(df_ethane_cnz, "Total Demand", "", "15_ethane_cnz", 0, 800, "Thousand barrels per day (Mb/d)", c_width = 4)
create_oil_sands_emission_chart(df_ethane_gnz, "Total Demand", "", "15_ethane_gnz", 0, 800, "Thousand barrels per day (Mb/d)", c_width = 4)
# French
df_ethane_cm <- translate_df(df_ethane_cm, list(list("Scenario", "Scenario_french"), list("TypeSector", "Varialbe_french")))
df_ethane_cnz <- translate_df(df_ethane_cnz, list(list("Scenario", "Scenario_french"), list("TypeSector", "Varialbe_french")))
df_ethane_gnz <- translate_df(df_ethane_gnz, list(list("Scenario", "Scenario_french"), list("TypeSector", "Varialbe_french")))
create_oil_sands_emission_chart(df_ethane_cm, "Demande totale", "", "15_ethane_cm", 0 , 800, "Milliers de barils par jour (kb/j)", lang = "fr", c_width = 4)
create_oil_sands_emission_chart(df_ethane_cnz, "Demande totale", "", "15_ethane_cnz", 0, 800, "Milliers de barils par jour (kb/j)", lang = "fr", c_width = 4)
create_oil_sands_emission_chart(df_ethane_gnz, "Demande totale", "", "15_ethane_gnz", 0, 800, "Milliers de barils par jour (kb/j)", lang = "fr", c_width = 4)


# 16) Canada Propane Global Net-zero Disposition 2010-2050 - Mb/d
df_propane = get_data("Propane FLAT", "A1:F286")
new_row <- data.frame(Scenario=c("","","","",""),
                      Scenario_french=c("","","","",""),
                      Year=c(2022,2023,2023,2023,2023),
                      Variable=c("Projected Exports", "Exports from Westcoast BC to International","Exports from AB to U.S.","Exports from ON to U.S.","Exports from Rest of Canada to U.S."),
                      Variable_french=c("Projection des exportations","Exportations à partir de la côte Ouest de la Colombie-Britannique vers l'international","Exportations de l’Alberta vers les États-Unis","Exportations de l’Ontario vers les États-Unis","Exportations du reste du Canada vers les États-Unis"),
                      Value=c(0,0,0,0,0))
df_propane <- rbind(df_propane, new_row)
names(df_propane)[names(df_propane) == "Variable"] <- "Type"
custom_order <- c("Projected Exports", "Exports from AB to U.S.", "Exports from ON to U.S.", "Exports from Rest of Canada to U.S.", "Exports from Westcoast BC to International","AB Petrochemical Demand", "Other Canada Other Demand", "AB Other Demand", "Solvent in Oil Sands AB Demand", "ON Petrochemical Demand")
df_propane <- df_propane[order(df_propane$Type, -df_propane$Value), ]
create_area_chart(df_propane, "16_propane", 0, 400, "Thousand barrels per day (Mb/d)", custom_order)
# French
df_propane <- translate_df(df_propane, list(list("Scenario", "Scenario_french"), list("Type", "Variable_french")))
custom_order <- c("Projection des exportations", "Exportations de l’Alberta vers les États-Unis", "Exportations de l’Ontario vers les États-Unis", "Exportations du reste du Canada vers les États-Unis", "Exportations à partir de la côte Ouest de la Colombie-Britannique vers l'international","Demande du secteur pétrochimique de l’Alberta", "Demande ailleurs au Canada", "Demande d’autres secteurs de l’Alberta", "Solvant pour les sables bitumineux en Alberta", "Demande du secteur pétrochimique de l’Ontario")
create_area_chart(df_propane, "16_propane", 0, 400, "Milliers de barils par jour (kb/j)", custom_order, NULL, "fr")


# 17) Net Imports of Pentanes Plus + Condensate 2010-2025 all scenarios - Mb/d
df_condy = get_data("Net Imports PP+Condy FLAT", "A1:F124")
names(df_condy)[names(df_condy) == "Scenario"] <- "Type"
create_line_chart(df_condy, "17_net_imports_pp_condy", -200, 600, 100, "Thousand barrels per day (Mb/d)", cer_colours, c("Global Net-zero", "Canada Net-zero", "Current Measures"))
# French
df_condy <- translate_df(df_condy, list(c("Type", "Scenario_french"), c("Variable", "Variable_french")))
create_line_chart(df_condy, "17_net_imports_pp_condy", -200, 600, 100, "Milliers de barils par jour (kb/j)", cer_colours, c("Carboneutralité à l’échelle mondiale", "Carboneutralité au Canada", "Mesures actuelles"), lang="fr")


# 18) Electricity Generation Capacity 2050 compared to 2021 all scenarios
df_elec_cap <- get_data("Electrcity Capacity", "A1:G81")
names(df_elec_cap)[names(df_elec_cap) == "capacity_GW"] <- "y_col"
create_electricity_chart(df_elec_cap, c('Current Measures', 'Global Net-zero','Canada Net-zero'), "18_electricity_capacity", 350, y_text="Gigawatt")
# French
df_elec_cap <- translate_df(df_elec_cap, list(c("Scenario", "Scenario (FR)"), c("Fuel/Technology", "Fuel/Technology FR")))
df_elec_cap$`Fuel/Technology` <- factor(df_elec_cap$`Fuel/Technology`, levels=c("Stockage dans des batteries","Bioénergie","Bioénergie avec CUSC","Charbon et coke","Hydroélectricité","Hydrogène","Gaz naturel","Gaz naturel avec CUSC","Énergie éolienne, production extracôtière","Pétrole","Énergie éolienne, production sur terre","Solaire (distribué)","Solaire (échelle des services publics)","Uranium","PRM à l’uranium"))
create_electricity_chart(df_elec_cap, c('Mesures actuelles', 'Carboneutralité à l’échelle mondiale','Carboneutralité au Canada'), "18_electricity_capacity", 350, lang="fr", y_text="Gigawatt")


# 19) Electricity Generation by Technology 2050 compared to 2021 all scenarios
df_elec_gen <- get_data("Electricity Generation", "A1:G75")
names(df_elec_gen)[names(df_elec_gen) == "generation_TWh"] <- "y_col"
create_electricity_chart(df_elec_gen, c('Current Measures', 'Global Net-zero','Canada Net-zero'), "19_electricity_generation", 1400, y_text="Terrawatt hours (TWh)")
# French
df_elec_gen <- translate_df(df_elec_gen, list(c("Scenario", "Scenario (FR)"), c("Fuel/Technology", "Fuel/Technology (FR)")))
create_electricity_chart(df_elec_gen, c('Mesures actuelles', 'Carboneutralité à l’échelle mondiale','Carboneutralité au Canada'), "19_electricity_generation", 1400, lang="fr", y_text="Térawattheures")


# 20) Electricity Peak Demand by province 2050 compared to 2021 all scenarios
df_elec_peak <- get_data("Electricity Peak demand", "A1:E40")
create_elec_peak <- function(df, y_axis_title, lang="en") {
  elec_peak_chart <- ggplot(df, aes(Region,peak_demand_ratio, fill=as.factor(Scenario)))+
    geom_col(width = 0.9, position = position_dodge(0.9))+
    scale_fill_manual(values = cer_colours, limits = force)+
    cer_theme()+
    theme(legend.spacing.y = unit(0.1, "cm")) +
    scale_y_continuous(breaks = pretty_breaks(n = 10), expand = c(0, 0), limits=c(0, 4), labels = scales::number_format(accuracy = 0.1)) +
    scale_x_discrete(expand = c(0, 0))+
    theme(
      legend.position = "bottom",
      legend.key.size = unit(2, 'mm'),
      legend.spacing.x = unit(0.40, "mm"),
      legend.spacing.y = unit(0.10, "mm"),
      legend.text = element_text(margin = margin(r = 4, unit = "pt")),
      axis.ticks.x = element_blank(),
      axis.text = element_text(colour="black",margin = margin(t = 10, b = 10)))+
    labs(y=y_axis_title)+
    guides(fill = guide_legend(ncol = 3))
  save_charts("20_electricity_peak_demand", elec_peak_chart, lang)
}
create_elec_peak(df_elec_peak, "Peak demand in 2050 compared to 2021")
# French
df_elec_peak <- translate_df(df_elec_peak, list(c("Scenario", "Scenario (FR)"), c("Region", "Region (FR)")))
df_elec_peak$Scenario <- factor(df_elec_peak$Scenario, levels=c('Carboneutralité au Canada', 'Mesures actuelles', 'Carboneutralité à l’échelle mondiale'))
create_elec_peak(df_elec_peak, "Demande de pointe en 2050 par rapport à 2021", "fr")


# 21) Global Net-zero Renewables + Electricity Share 2050 compared to 2021
dmd_vs_gen <- get_data("gnz renewables+elec share", "A1:E27") %>%
  rename("share of electricity in end-use demand %" = electricity_share, "share of renewables in electricity generation %"=renewables_share) %>%
  pivot_longer(cols = c('share of electricity in end-use demand %':'share of renewables in electricity generation %'), names_to = 'Type', values_to = 'Value') %>%
  mutate(Year=as.character(Year))

c <- ggplot(dmd_vs_gen, aes(Region,Value,fill=Type))+
  geom_col( position = "dodge")+
  theme(legend.spacing.y = unit(0.1, "cm")) +
  scale_fill_manual(values = cer_palette, limits = force)+
  scale_y_continuous(breaks = pretty_breaks(n = 10),expand= c(0,0),limit = c(0, 1),labels = label_percent(accuracy = 1)) +
  cer_theme()+
  labs( y="Percentage (%)")+
  facet_wrap(~Year, nrow=2, scales = "free_x")+
  theme(strip.text.x = element_text(size = 5), axis.title.x = element_text(size = 5))

save_charts("21_gen_vs_dmd", c)


# 22 Oil Sands Production: New vs Existing
df_os_prod <- get_data(sheet="Oil Sands New vs Existing Prod", range="A1:G211")
column_filters <- list(
  list(column = "Scenario", type = "equal", values = c("Canada Net-zero"))
)
df_os_prod = filter_dataframe(df_os_prod, column_filters)
create_area_chart(df_os_prod, "22_oil_sands_new_vs_existing", 0, 4000, "Thousand barrels per day (mb/d)", c("New Projects", "Existing Projects"))
# French
df_os_prod <- translate_df(df_os_prod, list(c("Scenario", "Scenario FR"), c("Type", "Type FR")))
create_area_chart(df_os_prod, "22_oil_sands_new_vs_existing", 0, 4000, "Milliers de barils par jour (kb/j)", c("Nouveaux projets", "Projets actuels"), lang="fr")


# 23 Indexed Oil Production
df_index_1 <- get_data(sheet="Index Light +Heavy Production", range="M2:P32")
df_index_2 <- get_data(sheet="Index Mined + Insitu Production", range="M2:P32")
unstack_dataframe <- function(df) {
  df_long <- df %>%
    pivot_longer(cols = -Year, names_to = "Type", values_to = "Value")
  return(df_long)
}
df_index_1 <- unstack_dataframe(df_index_1)
df_index_2 <- unstack_dataframe(df_index_2)
df_index_1$Sector <- "Light + Heavy Production"
df_index_2$Sector <- "Mined + Insitu Production"
df_index <- rbind(df_index_1, df_index_2)
create_multiple_line(df_index, 1.6, "", "23_indexed_production")
# French
# df_index$Type[df_index$Type == 5] <- 10


# 24 Carbon Pricing
df_carbon <- get_data(sheet="Carbon pricing", range="A3:F15")
df_carbon$Year <- as.character(df_carbon$Year)
names(df_carbon)[names(df_carbon) == "Year"] <- "x1"
names(df_carbon)[names(df_carbon) == "Scenario"] <- "x2"
names(df_carbon)[names(df_carbon) == "Type"] <- "name"
names(df_carbon)[names(df_carbon) == "Value ($2022 Canadian)"] <- "Value"
create_double_x_axis(df_carbon, "$2022 CDN per tonne", "24_carbon_pricing", 0, 500, 100, c("Global Net-zero", "Canada Net-zero", "Current Measures"))
# French
df_carbon <- translate_df(df_carbon, list(c("x2", "Scenario (FR)"), c("name", "Type (FR)")))
create_double_x_axis(df_carbon, "$ CA de 2022 par tonne", "24_carbon_pricing", 0, 500, 100, c("Carboneutralité à l’échelle mondiale", "Carboneutralité au Canada", "Mesures actuelles"), lang="fr")


# 25 Oil Sands Emissions Int
df_os_int <- get_data(sheet="Oil Sands Emissions Int", range="A1:G106")
df_os_int$Type <- NULL
names(df_os_int)[names(df_os_int) == "Scenario"] <- "Type"
create_line_chart(df_os_int, "25_oil_sands_emissions_int", 0, 0.07, 0.01, "tonnes CO2 per barrel", cer_colours, c("Global Net-zero", "Canada Net-zero", "Current Measures"))
# French
df_os_int <- translate_df(df_os_int, list(c("Type", "Scenario FR")))
create_line_chart(df_os_int, "25_oil_sands_emissions_int", 0, 0.07, 0.01, "Tonne de CO2 par baril", cer_colours, c("Carboneutralité à l’échelle mondiale", "Carboneutralité au Canada", "Mesures actuelles"), lang="fr")

