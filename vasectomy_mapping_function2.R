# Set working directory
setwd("~/Google Drive/git/vasectomy_mapping")


# Part 1: set up and load data

load_vmaps <- function() {
  
  require(tidyverse)  # Tidy
  require(scales)     # Better scales on maps
  require(rgdal)      # R wrapper around GDAL/OGR
  require(tictoc)     # Timing code runs
  require(readxl)     # Read Excel files
  require(rmapshaper) # Compress shapefily polygons
  
  
  # Define grattan colors
  glightyellow <<- "#FFE07F"
  gyellow <<- "#FFC35A"
  gorange <<- "#F68B33"
  gdark <<- "#D4582A"
  gdarker <<- "#BD3603" 
  gred <<- "#A02226"
  gdarkred <<- "#621214"
  
  # Define Marie Stopes shades
  
  ms <<- "#409DD6"
  
  ms1 <<- "#A0CEEB"
  ms2 <<- "#70B6E0"
  ms4 <<- "#3889BB"
  ms5 <<- "#286286"
  
  # Map poly compression [0,1]
  mapcompression <- 0.2
  
  # Read in STATES shapefile (map) data
  shapefile <- readOGR("data/statesshapefile/", "STE11aAust") %>% 
               ms_simplify(keep = mapcompression, keep_shapes = T)
  state.shapefile.details <- merge(fortify(shapefile), 
                                   as.data.frame(shapefile), 
                                   by.x="id", 
                                   by.y=0)
  
  assign("states.poly", 
         state.shapefile.details %>% 
           rename(state      = "STATE_NAME",
                  state_code = "STATE_CODE"),
         pos = 1)
  
  # Read in SA3 shapefile (map) data
  shapefile <- readOGR("data/sa3shapefile/", "SA3_2016_AUST")
  
  t01 <- ms_simplify(shapefile, keep = 0.01, keep_shapes = T)
  t25 <- ms_simplify(shapefile, keep = 0.20, keep_shapes = T)
  t1  <- ms_simplify(shapefile, keep = 1.00, keep_shapes = T)
  
  td01 <- merge(fortify(t01), as.data.frame(t01), by.x="id", by.y=0)
  td25 <- merge(fortify(t25), as.data.frame(t25), by.x="id", by.y=0)
  
  object.size(td01)
  object.size(td25)
  
  ggplot() +
  geom_polygon(data = td01,
               aes(x = long, y = lat, group = group), 
               fill = ms)
  ggsave("td01.pdf")
  
  ggplot() +
    geom_polygon(data = td25,
                 aes(x = long, y = lat, group = group), 
                 fill = ms)
  ggsave("td25.pdf")
  
  shapefile.details <- merge(fortify(shapefile), 
                             as.data.frame(shapefile), 
                             by.x="id", 
                             by.y=0)
  
  sa3poly <- shapefile.details %>%
    rename(sa3_code = "SA3_CODE16",
           sa3 = "SA3_NAME16",
           sa4 = "SA4_CODE16",
           sa4_name = "SA4_NAME16",
           gcc = "GCC_CODE16",
           gcc_name = "GCC_NAME16",
           state_code = "STE_CODE16",
           state = "STE_NAME16",
           area = "AREASQKM16"
    ) %>% 
    mutate(city = if_else(grepl('Greater', gcc_name), TRUE, 
                          if_else(state == "Australian Capital Territory", TRUE, 
                                  FALSE))) %>%
    select(lat, long, group, sa3_code, city, state_code)
  
  
  # Read in SA3 to postcode mapping
  postcodes <-  read_excel(path = "data/1270055006_CG_POSTCODE_2011_SA3_2011.xls",
                           sheet = "Table 3",
                           skip = 5) %>% 
    slice(2:n()) %>%  ## remove first empty row 
    select(postcode = POSTCODE, # changing names to be easier to use; and only keeping relevant vars
           sa3_code = SA3_CODE_2011,
           sa3      = SA3_NAME_2011,
           percent  = PERCENTAGE) %>% 
    mutate(percent  = as.numeric(percent)) %>%  # convert to numeric for ease of use
    group_by(postcode) %>% 
    mutate(best_sa3 = if_else(percent == max(percent),
                              TRUE, FALSE),
           morethan1 = sum(best_sa3)) %>% # This identifies ONE perfect split, in Tasmania. Let's DROP South East Coast SA3 for ease; but we should come back and do this better later
    filter(sa3 != "South East Coast") %>% # Drop that one annoying SA3
    filter(best_sa3 == 1) %>% # Now, keep only the "best" SA3s for each postcode
    select(-morethan1, -best_sa3) %>%  # And remove any unnecessary variables
    ungroup() # This might not be necessary, as there is only one obversation per postcode
  
  
  # Read in SA3 age data
  sa3age <- read_csv("data/table.csv",
                     skip = 10) %>% 
    slice(2:351) %>%   # Drop the first observation and remove cruft at bottom
    # select(-X8) %>%    # Drop the empty variable X8
    rename(sa3 = "AGE10P - Age in Ten Year Groups") %>%  # Rename variables to be easier to use
    rename(a2029 = "20-29 years") %>% 
    rename(a3039 = "30-39 years") %>% 
    rename(a4049 = "40-49 years") %>% 
    rename(a5059 = "50-59 years") %>% 
    rename(a6069 = "60-69 years") %>% 
    rename(total = "Total") %>% 
    mutate(a2029 = as.numeric(a2029),  # Convert a2029 variable to numeric (wasn't before due to import process)
           pc = (a2029 + a3039)/total) # Generate a new variable for the proportion of males who are 20-39 years old
  
  
  
  # Now merge the three datasets
  sa3 <-  left_join(sa3poly, postcodes, by = "sa3_code") %>% 
    group_by(sa3) %>% # This is creating a tag for one observation in each sa3 poly set
    mutate(justone = 1,
           one = cumsum(justone)) %>% 
    select(-justone) %>% 
    ungroup()
  
  assign("sa3", left_join(sa3, sa3age, by = "sa3"), pos = 1)
  
  # Create smaller dataset for one == 1
  assign("sa3one", sa3 %>% filter(one == 1), pos = 1)
  
} # end of load_vmaps() function







plot_vmap <- function(code = 60203, plot = FALSE, save = TRUE) {
  # Plotting one area against all others in a state via a loop ("highlighting" an area)
  
  # Get the state of the code (state_code)
  thisstatecode <- as.numeric(sa3one %>% filter(sa3_code == code) %>% select(state_code))
  name <- paste0(code,"_",thisstatecode)
  city <- as.logical(sa3one %>% filter(sa3_code == code) %>% select(city))
  
  if (city) {
    # Generate the chart
    chart <-
      ggplot() + 
      geom_polygon(data = (sa3 %>% filter(state_code == thisstatecode & city == TRUE)),
                   aes(x = long, y = lat, group = group),
                   fill = "grey90") +
      geom_polygon(data = (sa3 %>% filter(sa3_code == code)),
                   aes(x = long, y = lat, group = group), 
                   fill = ms) +
      theme_void() +
      theme(legend.position = "off",
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      NULL 
    
  } else {
    # Generate the chart of GCC area
    chart <-
      ggplot() + 
      geom_polygon(data = (states.poly %>% filter(state_code == thisstatecode)),
                   aes(x = long, y = lat, group = group),
                   fill = "grey90") +
      geom_polygon(data = (sa3 %>% filter(sa3_code == code)),
                   aes(x = long, y = lat, group = group), 
                   fill = ms) +
      theme_void() +
      theme(legend.position = "off",
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      NULL 
    
  }
  
  
  if (plot) {
    plot(chart)
  }
  
  if (save) {
    ggsave(paste0("atlas/", code,"_",thisstatecode,".pdf"), chart)
  }
  
}


load_vmaps()

plot_vmap(code = 20403, plot = FALSE, save = TRUE)


# Maybe instead:
build_vmaps()
load_vmaps()
plot_vmap()


tic()
write_csv(samplesa3, "data/write-test.csv")
toc()
tic()
samplesa3 <- read_csv("data/write-test.csv")
toc()
tic()
fwrite(sa3, "data/fwrite-all.csv")
tic()
sa3_wr <- fread("data/fwrite-all.csv")
toc()




a <- 10
