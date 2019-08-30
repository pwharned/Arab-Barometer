

##pathname_mac="/Users/pharned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx"
library(readxl)
labeling = read_xlsx("C:/Users/Patrick Harned/Google Drive/Coding/R Projects/Captions/Captions and Titles.xlsx", sheet=4) ##import arabic language labels from an excel sheet. this is to ensure 

library(ggrepel)

###Colors
ab_colors <- c(
  `light blue 1`  = "#B1C2CF",
  `light blue 2`  = "#7CBBC7",
  `light blue 3`   = "#43B9CC",
  `light blue 4` = "#2096BA",
  `light blue 5`   = "#327591",
  `medium blue 1` = "#93A1AC",
  `medium blue 2`  = "#60919A",
  `medium blue 3`  = "#2C8F9F",
  `medium blue 4`  = "#21718A",
  `medium blue 5`   = "#305C6E",
  `light orange 1` = "#F7B58B",
  `light orange 2`   = "#FBA950",
  `light orange 3` = "#F28232",
  `light orange 4`  = "#DF6E21",
  `medium orange 1`  = "#CF8B61",
  `medium orange 2`  = "#DA8C37",
  `medium orange 3`   = "#CD671E",
  `medium orange 4` = "#B04F0D",
  `light purple 1`   = "#CE83A3",
  `light purple 2` = "#AC517F",
  `light purple 3`  = "#837FA7",
  `light purple 4`   = "#796391",
  `dark purple 1` = "#903F68",
  `dark purple 2`  = "#7F375B",
  `dark purple 3`   = "#5C5883",
  `dark purple 4` = "#4B3861"
  )


orange1=c("#F7B58B","#60919A","#7F375B","#43B9CC")

ab_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ab_colors)
  
  ab_colors[cols]
}

ab_palettes <- list(
  `light blue`  = ab_cols("light blue 1", "light blue 2", "light blue 3", "light blue 4", "light blue 5"),
  
  `medium blue`  = ab_cols("medium blue 1", "medium blue 2", "medium blue 3", "medium blue 4", "medium blue 5"),
  
  `light orange`  = ab_cols("light orange 1", "light orange 2", "light orange 3", "light orange 4"),
  
  `medium orange`  = ab_cols("medium orange 1", "medium orange 2", "medium orange 3", "medium orange 4"),
  
  `light purple`  = ab_cols("light purple 1", "light purple 2", "light purple 3", "light purple 4"),
  
  `dark purple`  = ab_cols("dark purple 1", "dark purple 2", "dark purple 3", "dark purple 4")
)



ab_pal <- function(palette = "light_orange", reverse = FALSE, ...) {
  pal <- ab_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

scale_color_ab <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ab_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("ab_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

scale_fill_ab <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- ab_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("ab_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


library(extrafont)


title_function= function (variable){
  title = c(labeling[[variable]])
  if(!is.na(title[2])){
    paste(variable, str_wrap(title[1],width = 60),title[2], sep = "\n ")
  }else{
    paste(variable, str_wrap(title[1],width = 60), sep = "\n ")
  }
}

short_title =function (variable){
  title = c(labeling[[variable]])
  paste(title[2], sep = "\n ")
}


long_subtitle_function= function (variable){
  title = c(labeling[[variable]])
  paste(title[3],title[4], sep="\n",collapse = "\n")
}

subtitle_function= function (variable){
  title = c(labeling[[variable]])
  paste(title[3],sep="\n",collapse = "\n")
}

individual_country_plot <- function(data, x, y, country) {
  
  title = title_function(deparse(substitute(y)))
  
  title = str_wrap(paste(country, substitute(y), title, sep = ":"), width = 70)
  subtitle = subtitle_function(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  
  
  ggplot(data,aes(!!x,!!y,fill=!!x)) + geom_col()+geom_bar(stat = "identity")+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+
    scale_fill_manual(values=c('#2096BA','#21718A'))+
    theme(plot.title = element_text(size=12), plot.subtitle = element_text(size = 10),axis.text.y = element_text(angle = 45),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(title)+
    geom_text(aes(label=round(!!y*100)), vjust=1.9,hjust=-.3 , color="black", size=3.5)
}



grouped_country_plot_filled <- function(data, x, y, by=NULL, colour = NULL) {
  
  subtitle = paste(paste("(",deparse(substitute(group)),")", sep =""),subtitle(deparse(substitute(y))),sep="\n")
  
  title = short_title(deparse(substitute(y)))
  
  subtitle = subtitle_function(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  by = enquo(by)
  
  ggplot(data,aes(reorder(!!x, +!!y),!!y, fill = !!by)) +geom_bar(stat = "identity", width = .5, fill=colour)+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+
    theme(legend.position = "none",  plot.title = element_text(size=12, hjust=0.5), plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
    ggtitle(title)+ labs(caption = "Notes: Weighted Estimates.\n Source: Arab Barometer, Wave 5")+
    geom_text(aes(label=round(!!y)), hjust=-.2 , color="black", size=3.5)+xlab("") +ylab("Percent")+theme(text = element_text(family = "Arial"),
panel.border = element_rect(colour = "black", fill=NA, size=.6),plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic")  )
}


grouped_country_plot <- function(data=data, x, y, group = NULL, colour = NULL, title = title_function, subtitle= subtitle_function) {
  

  subtitle = subtitle(deparse(substitute(y)))
  
  title = short_title(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  group <- enquo(group)
    ggplot(data = data,aes(reorder(!!x, +!!y),!!y, fill = !!group)) +geom_col(width = .5, position = position_dodge(width = 1))+
    coord_flip()+ggtitle(title, subtitle = subtitle)+theme_bw()+scale_fill_manual(values = colour)+
    theme(legend.position = "bottom", axis.text.x = element_text(size = 10),plot.caption = element_text(size = 11),  plot.title = element_text(size=16, hjust=0.5), plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45, size = 12),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
     labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer, Wave 5")+
    geom_text(aes(label=round(!!y)),color="black", size=3, hjust = -.2)+xlab("") +ylab("Percent")+theme(text = element_text(family = "Arial"),
                                                                                                     panel.border = element_rect(colour = "black", fill=NA, size=.6),plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic"))+ylim(0,100)
}

trend_graph_function = function (data=NULL, x, y, group=NULL, title= title_function, subtitle=subtitle_function, colour=colour){
  
    subtitle = subtitle(deparse(substitute(y)))

  title = title(deparse(substitute(y)))
  x <- enquo(x)
  y <- enquo(y)
  group = enquo(group)
  
    ggplot(data,aes(x= !!x, y= !!y, group = !!group))+geom_point(aes(colour = !!group))+geom_line(aes(colour = !!group))+
    ggtitle(title, subtitle = subtitle)+theme_bw()+
    theme(plot.title = element_text(size=12, hjust=0.5), plot.caption = element_text(size = 11),
          plot.subtitle = element_text(size = 10, hjust = 0.5),axis.text.y = element_text(angle = 45),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank())+scale_color_manual(values = colour)+
          ggtitle(title)+ labs(caption = "Notes: Weighted Estimates.\nSource: Arab Barometer")+ylim(1,100)+
         geom_text_repel(aes(label=round(!!y)), vjust = -1.2, color="black", size=3.5)+xlab("") +
         ylab("Percent")+
        theme(text = element_text(family = "Arial"),
          panel.border = element_rect(colour = "black", 
                                      fill=NA, size=.6),
          plot.caption = element_text(hjust = 0),plot.subtitle = element_text(face = "italic"))
}




####Colors
women_color = '#796391'
country_color = "#DF6E21"
education_color= c("#FBA950","#F28232")
gender_color = c("#CE83A3","#796391")
gender_color_trend = c("#796391", "#CE83A3",'#DF6E21')
trend_colors = c("#FBA950","#5C5883","#7CBBC7","#CE83A3")
age_colors = c("#21718A","#2096BA")

