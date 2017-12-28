nomear_paises <- function(country, flag = TRUE){
  if(flag == TRUE){
  custom_match01 <- c(WO = "World Office", EP = "European Union", SU = "Soviet Union",
                      AP = "NA", EA = "NA", OA = "NA",
                      UK = "UK")
  country <- countrycode(country, "iso2c", 
                               "country.name.en", custom_match = custom_match01)}

  for (i in 1:length(country))
  {
    if (country[i] == "United Kingdom of Great Britain and Northern Ireland" )
    {
      country[i] <- "UK"
    }
    else if(country[i] == "United States of America"){
      country[i] <- "USA"
    } else if(country[i] == "Republic of Korea"){
      country[i] <- "South Korea"
    } else if (country[i] == "Russian Federation"){
      country[i] <- "Russia"
    }else if (country[i] =="Taiwan, Province of China"){
      country[i] <- "Taiwan"
    }else if (country[i] =="United Kingdom"){
      country[i] <- "UK"
    }
  }
                               
  return(country)
  
  
}