data <- mtcars
myfun1 <- function(data, group_name, measure_input){
  data %>%
    group_by_(group_name) %>%
    summarise_(measure_input) %>% 
    ungroup() -> data
  return(data)
}
group_name <- "cyl"
measure_input <- "mean(mpg)"
myfun1(data = mtcars, group_name = group_name, measure_input = measure_input)

myfun2 <- function(data, select_name, filter_input){
  data %>%
    filter_(.dots = filter_input) %>%
    select_(.dots = select_name) -> data
  return(data)
}
select_name <- c("mpg","disp","cyl","carb")
filter_input <- c("cyl>4","vs==0","carb %in% c(3,4)")

myfun2(data = mtcars, select_name = select_name, 
       filter_input = filter_input)

myfun3<-function(data,mute_input=NULL){
  data %>% mutate_(.dots=mute_input) %>% return()
}
mmm=c("cc"="cyl*carb","dbm"="disp+mpg")
myfun3(data,mute_input = mmm)
