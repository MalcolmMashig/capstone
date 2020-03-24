library(tidyverse)

# FBv Submodel by Year

fbv_submodel_y <- function(year) {
  fbv1 = paste("FBv_", year, sep="")
  fbv2 = paste("FBv_", year + 1, sep="")
  age1 = paste("Age_", year, sep="")
  data = data_by_year[data_by_year[[fbv1]] != 'NA' & data_by_year[[fbv2]] != 'NA',]
  model = lm(get(fbv2) ~ get(fbv1) + get(age1), data=data)
  return(summary(model))
}

fbv_submodel_y(2008)

# FBv Submodel by Age

fbv_submodel_a <- function(age) {
  fbv1 = paste("FBv_", age, sep="")
  fbv2 = paste("FBv_", age + 1, sep="")
  data = data_by_age[data_by_age[[fbv1]] != 'NA' & data_by_age[[fbv2]] != 'NA',]
  model = lm(get(fbv2) ~ get(fbv1), data=data)
  return(summary(model))
}

fbv_submodel_a(24)

# FB% Submodel by Year

fbp_submodel_y <- function(year) {
  fbp1 = paste("FB%_", year, sep="")
  fbp2 = paste("FB%_", year + 1, sep="")
  age1 = paste("Age_", year, sep="")
  data = data_by_year[data_by_year[[fbp1]] != 'NA' & data_by_year[[fbp2]] != 'NA',]
  model = lm(get(fbp2) ~ get(fbp1) + get(age1), data=data)
  return(summary(model))
}

fbp_submodel_y(2011)

# FB% Submodel by Age

fbp_submodel_a <- function(age) {
  fbp1 = paste("FB%_", age, sep="")
  fbp2 = paste("FB%_", age + 1, sep="")
  data = data_by_age[data_by_age[[fbp1]] != 'NA' & data_by_age[[fbp2]] != 'NA',]
  model = lm(get(fbp2) ~ get(fbp1), data=data)
  return(summary(model))
}

fbp_submodel_a(30)

# Age Submodel

age_submodel <- function(age) {
  return(age+1)
}

age_submodel(21)

# xFIP Model by Year

xfip_model_y <- function(year) {
  xfip1 = paste("xFIP_", year, sep="")
  xfip2 = paste("xFIP_", year + 1, sep="")
  age1 = paste("Age_", year, sep="")
  fbv1 = paste("FBv_", year, sep="")
  fbp1 = paste("FB%_", year, sep="")
  data = data_by_year[data_by_year[[xfip1]] != 'NA' & data_by_year[[xfip2]] != 'NA',]
  model = lm(get(xfip2) ~ get(xfip1) + get(age1) + get(fbv1) + get(fbp1), data=data)
  return(summary(model))
}

xfip_model_y(2016)

# xFIP Model by Age

xfip_model_a <- function(age) {
  xfip1 = paste("xFIP_", age, sep="")
  xfip2 = paste("xFIP_", age + 1, sep="")
  fbv1 = paste("FBv_", age, sep="")
  fbp1 = paste("FB%_", age, sep="")
  data = data_by_age[data_by_age[[xfip1]] != 'NA' & data_by_age[[xfip2]] != 'NA',]
  model = lm(get(xfip2) ~ get(xfip1) + get(fbv1) + get(fbp1), data=data)
  return(summary(model))
}

xfip_model_a(28)

