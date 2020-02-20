library(ggmap)

df_address <- read.csv("only_addresses.csv")
df_address$address <- as.character(df_address$address)

ggmap(get_googlemap())
register_google(key = "Your-Key")


geocode("Robson Street, Vancouver West")

maps_loc <- function(df_address) {
  
  loc <<- list()
  for(i in 1:(nrow(df_address))) {
    loc[[i]] <<- geocode(df_address[i, 1])
    
  }
}

purrr::safely(maps_loc(df_address))
loc %>%
  purrr::map(., "lon") %>%
  unlist(.) -> longitude
loc %>%
  purrr::map(., "lat")  %>%
  unlist(.) -> latitude

sotheby_data <- read.csv("sotheby_data.csv")
sotheby_data$longitude <- longitude
sotheby_data$latitude <- latitude
str(sotheby_data)

write.csv(sotheby_data, "lat_long_sotheby.csv")

p <- ggmap(get_googlemap(center = c(lon = -123.127, lat = 49.28321),
                         zoom = 14, scale = 1, size = c(640, 640),
                         maptype ='terrain',
                         color = 'color'))


### Variance Inflation Factor


housing %>%
  dplyr::select_if(., is.numeric) %>%
  stats::cor(., use = "pairwise.complete.obs")

library(car)
housing %>%
  stats::lm(price ~ age + bed + bath + neighborhood + squares, data = .) %>%
  car::vif(.)


# From the correlation matrix we might expect to see some high variance inflation factors for square feet, bed rooms, or bath rooms. However, using the `vif()` function from the car package, we can see that this is not the case. Therefore, our standard errors are nor going to be inflated and and the variance of our estimated parameters is going to be small. 

### Normality


normality_vars <- housing %>%
  dplyr::select(., price, squares, age, bed, bath)

df_lm_vars <- list()
for(i in 2:(ncol(normality_vars))) {
  df_lm_vars[[i - 1]] <- data.frame(price = normality_vars[, "price"], normality_vars[, i]) %>%
    dplyr::as_tibble(.)
  colnames(df_lm_vars[[i - 1]]) <- c("price", names(normality_vars)[i])
}

df_lm_vars %>%
  purrr::map(~dplyr::filter(., .[, 2] != 999)) %>%
  purrr::map(~ggplot(., aes_string(x = "price", y = colnames(.)[2])) + 
               geom_point() +
               geom_smooth() + 
               theme_minimal())

ggplot(housing, aes_string(x = "price", y = "squares")) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() + 
  xlab("Price") +
  ylab("Square Feet") -> sqft

ggplot(housing %>%
         dplyr::filter(., age < 900), aes(x = price, y = age)) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() + 
  xlab("Price") +
  ylab("Age") -> age

ggplot(housing, aes_string(x = "price", y = "bed")) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() +
  xlab("Price") +
  ylab("Bed") -> bed

ggplot(housing, aes_string(x = "price", y = "bath")) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() + 
  xlab("Price") +
  ylab("Bath") -> bath

ggpubr::ggarrange(sqft, age, bed, bath)



ggplot(housing, aes(x = squares)) + 
  geom_density() + 
  theme_minimal() +
  xlab("Square Feet") -> sqft

ggplot(housing %>%
         dplyr::filter(., age < 900), aes(x = age)) + 
  geom_density() + 
  theme_minimal() +
  xlab("Age") -> age

ggplot(housing, aes_string(x = "price", y = "bed")) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() +
  xlab("Price") +
  ylab("Bed") -> bed

ggplot(housing, aes_string(x = "price", y = "bath")) + 
  geom_point() +
  geom_smooth() + 
  theme_minimal() + 
  xlab("Price") +
  ylab("Bath") -> bath

ggpubr::ggarrange(sqft, age, bed, bath)
