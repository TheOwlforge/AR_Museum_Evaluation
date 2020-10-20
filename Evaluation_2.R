# library(forcats) #factors
library(plyr) #revalue
library(dplyr) #mutate, case_when
library(lattice) #plotting
library(ggplot2) #plotting
library(reshape2) #melt
library(corrplot)
library(MASS) #plotr
library(nnet) #multinom
library(RColorBrewer) #more color scales
library(quantreg)
library(mgcv)#gam
library(lmtest)#gam
library(boot)
library(voxel)#gamplot
library(scales)#percent
library(ggpubr)#anotate figure

########################################################
################# reading in data ######################
########################################################

general_data <- read.csv(file ="data/general_data_cleaned.csv", na.strings = "")
app_data <- read.csv(file ="data/app_data_cleaned.csv", na.strings = "")
data_orig <- merge(general_data, app_data, by="ID")

########################################################
################# data wrangling  ######################
########################################################

#delete ID
data_orig$ID <- NULL
#reverse a1
a1_fac <- revalue(as.factor(data_orig$a1), c("1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1"))
data_orig$a1 <- as.numeric(as.character(a1_fac))
#add age level 41-50
levels(data_orig$age) <- c(levels(data_orig$age),"41 - 50")
data_orig$age <- factor(data_orig$age, levels = c("18 - 24", "25 - 30", "31 - 40", "41 - 50", "51 - 60", "60+"))
#reorder gender
data_orig$sex <- factor(data_orig$sex, levels = c("female", "male", "diverse"))
#extract usability levels
dif_app_cut3 <- cut(data_orig$a1, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
dif_app_cut5 <- cut(data_orig$a1, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
dif_tools_cut3 <- cut(data_orig$a2, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
dif_tools_cut5 <- cut(data_orig$a2, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_orig <- mutate(data_orig, difficulty_app_cut3=dif_app_cut3)
data_orig <- mutate(data_orig, difficulty_app_cut5=dif_app_cut5)
data_orig <- mutate(data_orig, difficulty_tools_cut3=dif_tools_cut3)
data_orig <- mutate(data_orig, difficulty_tools_cut5=dif_tools_cut5)

summary(data_orig)

#extract pre-knowledge
k1_points <- case_when(data_orig$k1 %in% c("chronos", "kronos", "saturn", "uranos") ~ 1, TRUE ~ 0)
k2_points <- numeric(length(data_orig$k2))
k2_points[grep("odys|ilia|illias|aeneis|odise", data_orig$k2)] <- 1
k3_points <- case_when(data_orig$k3 %in% c("styx", "styks", "styx") ~ 1, TRUE ~ 0)
k4_points <- numeric(length(data_orig$k4))
k4_points[grep("gift|schierling|schirling|verurteilt|selbstmord", data_orig$k4)] <- 1
k5_points <- numeric(length(data_orig$k5))
k5_points[grep("solon|kleisth|rikles|tim", data_orig$k5)] <- 1
k6_points <- numeric(length(data_orig$k6))
k6_points[grep("epikur|peripatos|sto|akademie", data_orig$k6)] <- 1
sum <- k1_points + k2_points + k3_points + k4_points + k5_points + k6_points
sum_factor <- cut(sum, c(0,1,3,5,7), right = FALSE, labels = c("none", "basic", "educated", "expert"), ordered_result = TRUE)
summary(sum_factor)
sum <- sum/6

#extract knowledge about stele
q1_points <- numeric(length(data_orig$q1))
q1_points[grep("xanth|xhant|pferd", data_orig$q1)] <- 1
q2_points <- numeric(length(data_orig$q2))
q2_points[grep("sakral|name|schutz", data_orig$q2)] <- 1
q3_points <- numeric(length(data_orig$q3))
q3_points[grep("kleid|mantel|tuch|gewand|laken", data_orig$q3)] <- 1
q4_points <- numeric(length(data_orig$q4))
q4_points[grep("stuhl", data_orig$q4)] <- 1
q5_points <- numeric(length(data_orig$q5))
q5_points[grep("schu|handwerk", data_orig$q5)] <- 1
q6_points <- numeric(length(data_orig$q6))
q6_points[grep("nichts", data_orig$q6)] <- 1
q_sum <- q1_points + q2_points + q3_points + q4_points + q5_points + q6_points
q_sum_factor <- cut(q_sum, c(0,1,3,5,7), right = FALSE, labels = c("none", "basic", "educated", "expert"), ordered_result = TRUE)
summary(q_sum_factor)
q_sum <- q_sum/6

#extract relevant data
relevant_data <- data_orig[,c(1,2,9,10,19,20,21,22)]
relevant_data <- mutate(relevant_data, score_general=sum)
relevant_data <- mutate(relevant_data, level_general=sum_factor)
relevant_data <- mutate(relevant_data, score_object=q_sum)
relevant_data <- mutate(relevant_data, level_object=q_sum_factor)

#extract data of participants only at the museum and split by group
data_wo_demoday <- relevant_data[relevant_data$place != "demoday",]
summary(data_wo_demoday)
app_data <- data_wo_demoday[data_wo_demoday$group == "app",]
summary(app_data)
text_data <- data_wo_demoday[data_wo_demoday$group == "text",]
summary(text_data)

################################################
################# Functions ####################
################################################

#plot a multiple factors as separate barplot and piechart
plot_simple <- function(data, indices, longnames, titles, legend_names, palette) {
  for (i in seq(length(indices))) {
    idx <- indices[i]
    #barplot
    ggplot(data, aes(x=data[,idx], fill = data[,idx])) +
      scale_y_continuous(expand = c(0,0), limits = c(0, range(table(data[,idx]))[2]+1)) +
      geom_bar() +
      geom_bar(color="black", show.legend = FALSE) +
      theme_classic(base_size = img_size) +
      theme(legend.text=element_text(size=rel(1))) +
      xlab(longnames[idx]) +
      ylab("Number of Participants") +
      ggtitle(titles[i]) +
      scale_fill_brewer(name = legend_names[i], na.value="grey",
                        drop = FALSE, palette = palette[i]) +
      scale_x_discrete(drop=FALSE)
    ggsave(paste0("plots/Survey2/dataplot_", idx, "_", longnames[idx], ".png"),
           width = img_width, height = img_height, dpi = img_dpi)
    
    #pie chart
    df<-data.frame(table(data[,idx]))
    df <- df %>%  mutate(prop = Freq / sum(Freq) * 100)
    df <- df %>%  mutate(prop_perc = percent(prop/100, 1))
    df <- df %>%  arrange(desc(Var1)) %>% mutate(ypos = cumsum(prop) - 0.5*prop)
    df <- df %>%  mutate(al = ifelse(prop == 0, 0, 1))
    ggplot(df, aes(x="",y=prop,fill=Var1)) +
      geom_bar(width = 1, stat="identity", color = "white", size = 3) +
      coord_polar("y", start=0) +
      theme_void(base_size = img_size) +
      theme(legend.text=element_text(size=rel(1))) +
      geom_text(aes(y = ypos, label = prop_perc), color = "black", size = img_textsize) +
      xlab(longnames[idx]) +
      ylab("Number of Participants") +
      ggtitle(titles[i]) +
      scale_fill_brewer(name = legend_names[i], na.value="grey",
                        drop = FALSE, palette = palette[i])
    ggsave(paste0("plots/Survey2/piechart_", idx, "_", longnames[idx], ".png"),
           width = img_width, height = img_height, dpi = img_dpi)
  }
}

#plots multiple factors in one plot with separate side by side bars per level
plot_multiple <- function(data, col_names_vec, filename, legend_name, sort_vec, ...) {
  # get data as table
  data_table <- table(col(data), as.matrix(data), exclude=NULL)
  # combine them into a data frame
  data_frame <- data.frame(cbind(data_table), col_names_vec)
  data_frame <- data_frame[,sort_vec]
  print(data_frame)
  # melt the data frame for plotting
  data_frame_m <- melt(data_frame, id.vars='col_names_vec')
  data_frame_m$col_names_vec <- factor(data_frame_m$col_names_vec, levels = col_names_vec)
  data_frame_m$variable <- factor(data_frame_m$variable, levels=c(levels(data[,1]), c("NA")))
  # plot the data
  ggplot(data_frame_m, aes(x = col_names_vec, y = value)) +   
    scale_y_continuous(expand = c(0,0), limits = c(0, range(data_table)[2]+1)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity", color="black", show.legend = FALSE) +
    theme_classic(base_size=img_size) +
    labs(...) +
    theme(legend.text=element_text(size=rel(1))) +
    scale_fill_viridis_d(na.value="grey", name=legend_name)
  ggsave(paste0("plots/Survey2/", filename, ".png"),
         width=img_width, height=img_height, dpi=img_dpi)
}

#plots two factors against each other normalizing the columns
plot_dependent_bar_fill <- function(data, i, j, legend_name, palette, ...){
  if(palette == "Viridis"){
    ggplot(data) +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "fill") +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "fill", color="black", show.legend = FALSE) +
      theme_classic(base_size = img_size) +
      labs(...) +
      theme(legend.text=element_text(size=rel(1))) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
      scale_fill_viridis_d(name=legend_name, na.value="grey", drop = FALSE)
  } else {
    ggplot(data) +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "fill") +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "fill", color="black", show.legend = FALSE) +
      theme_classic(base_size = img_size) +
      theme(legend.text=element_text(size=rel(1))) +
      labs(...) +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1), expand=c(0,0)) +
      scale_fill_brewer(palette = palette, name=legend_name, na.value="grey", drop = FALSE)
  }
  ggsave(paste0("plots/Survey2/testplot_", i, "_", j, "_fill.png"), width = img_width, height = img_height, dpi = img_dpi)
}

#plots two factors against each other stacking the columns
plot_dependent_bar_stack <- function(data, i, j, legend_name, palette, ...){
  if(palette == "Viridis") {
    ggplot(data) +
      scale_y_continuous(expand = c(0,0), limits = c(0, range(table(data[,i]))[2]+1)) +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "stack") +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "stack", color="black", show.legend = FALSE) +
      theme_classic(base_size = img_size) +
      labs(...) +
      scale_fill_viridis_d(name = legend_name, na.value="grey", drop = FALSE)
  } else {
    ggplot(data) +
      scale_y_continuous(expand = c(0,0), limits = c(0, range(table(data[,i]))[2]+1)) +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "stack") +
      geom_bar(aes(x=data[,i], fill=data[,j]), position = "stack", color="black", show.legend = FALSE) +
      theme_classic(base_size = img_size) +
      labs(...) +
      scale_fill_brewer(palette = palette, name = legend_name, na.value="grey", drop = FALSE)
  }
  ggsave(paste0("plots/Survey2/testplot_", i, "_", j, "_stack.png"), width = img_width, height = img_height, dpi = img_dpi)
}

#produces a boxplot
plot_dependent_box <- function(data, i, j, palette, legend_name, ...){
  ggplot(data, aes(x=data[,i], y=data[,j])) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1), limits=c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4, aes(fill=data[,i])) +
    geom_jitter(shape=16, width = 0.2, height = 0, size = 3) +
    stat_summary(fun.y=mean, geom="point", shape=4, size=4) +
    scale_fill_brewer(palette = palette, name = legend_name) +
    theme_classic(base_size = img_size) +
    #theme(legend.position="bottom") +
    theme(legend.text=element_text(size=rel(0.9))) +
    labs(...)
  ggsave(paste0("plots/Survey2/boxplot_", i, "_", j, ".png"), width = img_width, height = img_height, dpi = img_dpi)
}

#returns a jitterplot which can be used as a base for other plots
get_jitter_plot <- function(data, i, j, color_idx, palette, legendname,
                            alpha, jitter_width, jitter_height){
  return(ggplot(data, aes(x=data[,i], y=data[,j], color=data[,color_idx])) +
           geom_jitter(alpha = alpha, cex=img_pointsize, width = jitter_width, height = jitter_height) +
           scale_color_brewer(palette = palette, name = legendname) +
           theme_classic(base_size = img_size)+
           theme(legend.text=element_text(size=rel(1))) #+ theme(legend.position="bottom")
         )
}

#plots quantile regression
plot_regression <- function(baseplot, model, filename, ...){
  g <- baseplot + geom_line(aes(y=predict(model)), size=img_linesize) +
    labs(...) + 
    scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
  ggsave(plot = g, filename = paste0("plots/Survey2/", filename, ".png"),
         width=img_width, height=img_height, dpi=img_dpi)
  return(g)
}

#plots multiple quantile regression models next to each other with combined title
plot_regression_multiple <- function(model1, model2, model3, filename, title, ...){
  plot1 <- baseplot + geom_line(aes(y=predict(model1)), size=img_linesize, show.legend = FALSE) +
    labs(...) +
    theme_classic(base_size = 12) +
    theme(legend.text=element_text(size=rel(1))) +
    scale_y_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
  plot2 <- baseplot + geom_line(aes(y=predict(model2)), size=img_linesize, show.legend = FALSE) +
    labs(...) +
    theme_classic(base_size = 12) +
    theme(legend.text=element_text(size=rel(1))) +
    scale_y_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
  plot3 <- baseplot + geom_line(aes(y=predict(model3)), size=img_linesize, show.legend = FALSE) +
    labs(...) +
    theme_classic(base_size = 12) +
    theme(legend.text=element_text(size=rel(1))) +
    scale_y_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
  
  ar <- ggarrange(plot1, plot2, plot3, common.legend = TRUE, legend = "bottom", ncol=3)
  ar <- annotate_figure(ar, top = text_grob(title, size=15))
  ggsave(plot = ar, filename = paste0("plots/Survey2/", filename, ".png"),
         width=11, height=4, dpi=300)
}

#plots GAM regression with CI intervals
plot_regression_confidence <- function(baseplot, newdata, model, filename, ...){
  prediction <- predict(model, newdata, type="link", se.fit=TRUE)
  g <- baseplot +
    geom_line(data=newdata, aes(x=newdata[,1], y=prediction$fit, color=newdata[,2]), size=img_linesize) +
    geom_ribbon(data=newdata, aes(ymin = prediction$fit - prediction$se.fit*2,
                                  ymax = prediction$fit + prediction$se.fit*2,
                                  x=newdata[,1],
                                  y=prediction$fit,
                                  color=NA, fill=newdata[,2]),
                alpha=0.25) +
    guides(fill=FALSE, size=FALSE) +
    labs(...) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05)) 
  ggsave(plot = g, filename = paste0("plots/Survey2/", filename, ".png"),
         width=img_width, height=img_height, dpi=img_dpi)
}

#plot bootstrapped GAM with num_samples_shown out of total number of samples
plot_regression_boot <- function(baseplot, boot_model, levelnames, filename,
                                 num_samples_total, num_samples_shown, ...){
  g<-baseplot
  for (i in sample(num_samples_total,num_samples_shown))
  {
    df <- data.frame(score_general = as.numeric(boot_model$t[i,][1:200]),
                     group = as.factor(boot_model$t[i,][201:400]),
                     score_object = as.numeric(boot_model$t[i,][401:600]),
                     se = as.numeric(boot_model$t[i,][601:800]))
    levels(df$group)<-levelnames
    if(df$score_object == -1){ next }
    g<-g+geom_line(data=df, aes(x=score_general, y=score_object, color=group), alpha=0.2) #+ 
    #geom_ribbon(data=df, aes(ymin = score_object - se*2,
    #ymax = score_object + se*2,
    #x=df[,1],
    #y=score_object,
    #color=NA, fill=df[,2]),
    #alpha=0.05) +
    #guides(fill=FALSE, size=FALSE)
  }
  g<-g+labs(...) +
    scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
  ggsave(plot = g, filename = paste0("plots/Survey2/", filename, ".png"),
         width=img_width, height=img_height, dpi=img_dpi)
}

################################################
################# Globals ######################
################################################

longnames <- c("Gender", "Age Group", "Place", "Group","","","","", "Preknowledge Score", "Preknowledge Level", "Exhibit Knowledge Score", "Exhibit Knowledge Level")

img_size <- 25
img_linesize <- 2
img_pointsize <- 7
img_textsize <- 8
img_width <- 10
img_height <- 7
img_dpi <- 300

##############################################
################# Plots ######################
##############################################

#plot basic variables
indices_basic <- c(1,2,4,10,12)
legend_names_basic <- c("Gender:", "Age Group:", "Group:", "Knowledge\nLevel:", "Knowledge\nLevel:")
titles_basic <- c("The Gender Distribution of the Participants",
                  "The Age Distribution of the Participants",
                  "Participants per Group",
                  "Level of Prior Knowledge in Grecian History",
                  "Acquired Knowledge about the Exhibit")
palette_basic <- c("Paired","Oranges","Set1","RdPu", "RdPu")
plot_simple(data_wo_demoday, indices_basic, longnames, titles_basic, legend_names_basic, palette_basic)
plot_simple(data_wo_demoday[data_wo_demoday$group=="app",], c(12), longnames, c(titles_basic[5]), c(legend_names_basic[5]), palette_basic[5])

#plot multiple
col_names_vec = c("General","Tools")
legend_name = "Usability Feedback"
x="Answers to Usability Questions"
y="Number of Participants"
title="Summary of all Questions regarding Usability"
usability_data <- data_wo_demoday[data_wo_demoday$group=="app",c(5,7)]
plot_multiple(data = usability_data, col_names_vec, "dataplot_usability_questions_cut3",
              legend_name, c(1,2,3,4), x=x, y=y, title=title)
usability_data <- data_wo_demoday[data_wo_demoday$group=="app",c(6,8)]
plot_multiple(data = usability_data, col_names_vec, "dataplot_usability_questions_cut5",
              legend_name, c(1,2,3,4,5,6), x=x, y=y, title=title)

#plot dependent
idx_a <- c(12,12,12,12,10,10,10)
idx_b <- c(1,2,4,10,1,2,4)
palette <- c("Paired","Oranges","Set1","RdPu","Paired","Oranges","Set1")
#try all for visualization purposes
for (i in 1:length(idx_a)){
  plot_dependent_bar_stack(data_wo_demoday, idx_a[i], idx_b[i], longnames[idx_b[i]],
                           x = longnames[idx_a[i]], y = "Number of Participants",
                           title = paste0(longnames[idx_a[i]], " by ", longnames[idx_b[i]]),
                           palette = palette[i])
  plot_dependent_bar_fill(data_wo_demoday, idx_a[i], idx_b[i], longnames[idx_b[i]],
                          x = longnames[idx_a[i]], y = "Number of Participants (norm.)",
                          title = paste0(longnames[idx_a[i]], " by ", longnames[idx_b[i]]),
                          palette = palette[i])
}
#plot single interesting one
plot_dependent_bar_fill(data_wo_demoday, 12, 4, longnames[4],
                        x = longnames[12], y = "Number of Participants (norm.)",
                        title = "Exhibit Knowledge Level by Group",
                        palette = "Set1")

#boxplots
plot_dependent_box(data = data_wo_demoday, i = 4, j = 11, palette = "Set1",
                   title = paste0("Acquired Knowledge by Group"),
                   x = longnames[4], y = longnames[11], legend_name = paste0(longnames[4], ":"))

########################################################
################# testing and stats ####################
########################################################

#get age statistics
age_ordered <- factor(data_wo_demoday$age, ordered = TRUE, levels = levels(data_wo_demoday$age))
summary(age_ordered)
quantile(age_ordered, 0.25, type=1)
quantile(age_ordered, 0.5, type=1)
quantile(age_ordered, 0.75, type=1)

age_ordered <- factor(text_data$age, ordered = TRUE, levels = levels(text_data$age))
summary(age_ordered)
quantile(age_ordered, 0.25, type=1)
quantile(age_ordered, 0.5, type=1)
quantile(age_ordered, 0.75, type=1)

age_ordered <- factor(app_data$age, ordered = TRUE, levels = levels(app_data$age))
summary(age_ordered)
quantile(age_ordered, 0.25, type=1)
quantile(age_ordered, 0.5, type=1)
quantile(age_ordered, 0.75, type=1)

# compare to normal distribution
score <- table(factor(data_wo_demoday$score_object, ordered=TRUE, levels = seq(0,1,by=1/6)))
exp <- dnorm(seq(0,1,by=1/6), mean=mean(data_wo_demoday$score_object), sd=sd(data_wo_demoday$score_object))
chisq.test(score, p=exp, rescale.p = TRUE, simulate.p.value = TRUE)
plot(score)
breaks_cdf <- pnorm(seq(0,1.1666666666666666666,by=1/6), mean=mean(data_wo_demoday$score_object), sd=sd(data_wo_demoday$score_object))
exp <- rollapply(breaks_cdf, 2, function(x) x[2] - x[1])
chisq.test(score, p=exp, rescale.p = TRUE, simulate.p.value = TRUE)
exp <- rnorm(100000, mean=mean(data_wo_demoday$score_object), sd=sd(data_wo_demoday$score_object))
ks.test(data_wo_demoday$score_object, "pnorm", mean=mean(data_wo_demoday$score_object), sd=sd(data_wo_demoday$score_object))
shapiro.test(data_wo_demoday$score_object)
qqnorm(data_wo_demoday$score_object)
qqline(exp)
hist(data_wo_demoday$score_object, breaks = 7)

# get average scores by group
data_rel <- data_wo_demoday[,c("score_object", "score_general", "group")]

aggregate(score_object ~ group, data_rel, mean)
aggregate(score_object ~ group, data_rel, median)
aggregate(score_general ~ group, data_rel, mean)
aggregate(score_general ~ group, data_rel, median)

sd(text_data$score_object)
sd(text_data$score_general)
sd(app_data$score_object)
sd(app_data$score_general)

var(text_data$score_object)
var(app_data$score_object)
var(text_data$score_general)
var(app_data$score_general)

# non-parametric alternatives:
# Wilcoxon Signed Rank Test for num-num instead of paired t-test
# ruskal Wallis for num-factor instead of anova
kruskal.test(data_rel$score_object~data_rel$group)

wilcox.test(data_rel$score_general,data_rel$score_object,paired=TRUE)
cor.test(data_rel$score_general,data_rel$score_object, method="spearman")
cor.test(data_rel$score_general,data_rel$score_object, method="kendall")
chisq.test(table(data_wo_demoday$level_object, data_rel$group))
chisq.test(table(data_rel$score_object, data_rel$group))
chisq.test(table(data_wo_demoday$level_general, data_rel$group))
chisq.test(table(data_rel$score_general, data_rel$group))

########################################################
#################### Regression ########################
########################################################

# generate base plots for regression

# baseplot_horizontal <- get_jitter_plot(data=data_rel, i=3, j=1, color_idx=3,
#                             palette="Set1", legendname=paste0(longnames[4],":"),
#                             alpha=1, jitter_width = 0.2, jitter_height = 0.1)
# baseplot_horizontal
baseplot <- get_jitter_plot(data=data_rel, i=2, j=1, color_idx=3,
                            palette="Set1", legendname=paste0(longnames[4],":"),
                            alpha=1, jitter_width = 0.05, jitter_height = 0.05)
baseplot

# Quantile Regression
#jitter data to avoid errors due to discretization
data_rel$score_object <- jitter(data_rel$score_object, factor=0.000001)
data_rel$score_general <- jitter(data_rel$score_general, factor=0.000001)

#for (i in seq(0.1,0.9,0.05))
for (i in c(0.25, 0.5, 0.75))
{
  model_null <- rq(score_object ~ 1, data = data_rel, tau=i)
  model <- rq(score_object ~ group + score_general, data = data_rel, tau=i)
  p <- anova(model, model_null)
  #if(p$table$pvalue<=0.05){
    print(summary(model))
    print(p)
    pv <- format(p$table$pvalue, digits=4)
    if (p$table$pvalue == 0)
    {
      pv <- "< 2.2e-16"
    }
    plot_regression(baseplot, model, paste0("quant_reg_",i),
                    x=longnames[5], y=longnames[7],
                    title=paste0("Quantile Regression at ", i*100, "th Percentile"),
                    subtitle=paste0("p-value: ", pv))
  #}
}

#plot 3 percentiles in one image
model1 <- rq(score_object ~ group + score_general, data = data_rel, tau=0.25)
model2 <- rq(score_object ~ group + score_general, data = data_rel, tau=0.5)
model3 <- rq(score_object ~ group + score_general, data = data_rel, tau=0.75)
plot_regression_multiple(model1, model2, model3,
                         "quant", "Quantile Regression at 25th, 50th and 75th Percentile",
                         x=longnames[9], y=longnames[11])

#reset data
data_rel <- data_wo_demoday[,c("score_object", "score_general", "group")]

#Generalized additive models

gam_model <- gam(score_object ~ group + s(score_general, k=6, bs="ps"), data = data_rel)
summary(gam_model)
gam_model_null <- gam(score_object ~ 1, data = data_rel)
anova(gam_model, gam_model_null)
test<-lrtest(gam_model, gam_model_null)
test
#plot non-smooth
plot_regression(baseplot, gam_model, "gam_reg", x=longnames[4], y=longnames[7], title="GAM with p-value of 0.0028")
#plot with CIs
newdata <- with(data_rel, data.frame(score_general = seq(from=0, to=1, length=200), group = levels(factor(data_rel$group))))
plot_regression_confidence(baseplot, newdata, gam_model, "gam_reg_CI", x=longnames[9], y=longnames[11], title="Generalized Additive Model")#, subtitle=paste0("p-value: ", round(test$`Pr(>Chisq)`[2],5)))

#plot using library
#plotGAM(gam_model, smooth.cov="score_general", groupCovs = "group")

###########################################################
#################### Bootstrapping ########################
###########################################################

bs_stat <- function(data, indices, formula) {
  d <- data[indices,] # allows boot to select sample
  return(c(aggregate(formula, d, mean)[1,2],
           aggregate(formula, d, mean)[2,2],
           aggregate(formula, d, median)[1,2],
           aggregate(formula, d, median)[2,2]))
}
bs_cor <- function(data, indices) {
  d <- data[indices,] # allows boot to select sample
  return(as.numeric(cor(d[,1],d[,2])))
}
bs_test <- function(data, indices){
  d <- data[indices,] # allows boot to select sample
  test <- kruskal.test(d[,1]~d[,3])
  return(c(test$statistic, test$parameter))
}
bs_gam <- function(data, indices, formula){
  d <- data[indices,] # allows boot to select sample
  gam_model <- gam(formula, data = d)
  newdata <- with(d, data.frame(score_general = seq(from=min(d[,2]), to=max(d[,2]), length=200), group = levels(factor(d[,3]))))
  p <- predict(gam_model, newdata, type="link", se.fit=TRUE)
  gam_model_null <- gam(score_object ~ 1, data = d)
  test <- lrtest(gam_model, gam_model_null)
  if(test$`Pr(>Chisq)`[2] > 0.01){p$fit[seq(1:length(p$fit))]<--1}
  return(c(newdata$score_general, newdata$group, p$fit, p$se.fit))
}
bs_quant <- function(data, indices, formula){
  d <- data[indices,] # allows boot to select sample
  model <- rq(formula, data = d, tau=0.75)
  newdata <- with(d, data.frame(score_general = c(0,0,1,1), group = levels(data[,3])))
  p <- predict(model, newdata)
  return (c(newdata$score_general, newdata$group, p))
}

set.seed(12347)
num_samples <- 1500

boot_stat <- boot(data=data_rel, statistic=bs_stat, R=num_samples, formula=score_object ~ group)
boot_stat
colMeans(boot_stat$t)
apply(boot_stat$t, 2, median)
boot.ci(boot_stat, index = 3)
plot(boot_stat, index = 1)

boot_stat <- boot(data=data_rel, statistic=bs_stat, R=num_samples, formula=score_general ~ group)
boot_stat
colMeans(boot_stat$t)
plot(boot_stat, index = 4)
boot.ci(boot_stat, index = 1)

boot_cor <- boot(data=data_rel, statistic=bs_cor, R=num_samples)
boot_cor
mean(boot_cor$t)
plot(boot_cor)
boot.ci(boot_cor)

boot_test <- boot(data=data_rel, statistic=bs_test, R=num_samples)
boot_test
test_res <- colMeans(boot_test$t)
pchisq(test_res[1], df=test_res[2], lower.tail=FALSE)

boot_gam <- boot(data=data_rel, statistic=bs_gam, R=num_samples, formula=score_object ~ group + s(score_general, k=6, bs="ps"))
plot_regression_boot(baseplot, boot_gam, levels(factor(data_rel$group)), "gam_boot",
                     num_samples, 400,
                     x=longnames[9], y=longnames[11], title="Bootstrapped GAM")

#plot bootstrapped gam with CIs

# boot_gam_rel <- boot_gam$t[boot_gam$t[,401] != -1,]
# boot_gam_rel[boot_gam_rel == "app"] <- 1
# boot_gam_rel[boot_gam_rel == "text"] <- 2
# class(boot_gam_rel) <- "numeric"
# boot_gam_mean <- colMeans(boot_gam_rel)
# df <- data.frame(score_general = boot_gam_mean[1:200], group = as.factor(boot_gam_mean[201:400]), score_object = boot_gam_mean[401:600], se= boot_gam_mean[601:800])
# levels(df$group)<-levels(data_rel$group)
# g <- baseplot2 +
#   geom_line(data=df, aes(x=score_general, y=score_object, color=group), size=3) +
#   geom_ribbon(data=df, aes(ymin = score_object - se*2,
#                                 ymax = score_object + se*2,
#                                 x=score_general,
#                                 y=score_object,
#                                 color=NA, fill=group),
#               alpha=0.25) +
#   guides(fill=FALSE, size=FALSE) +
#   labs(x=longnames[5], y=longnames[7], title="Bootstrapped GAM", subtitle = "Averaged prediction with 95% CI") + 
#   scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
#   scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
#   coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05))
# ggsave(plot = g, filename = "gam_boot_mean.png", width=10, height=10, dpi=300)

#bootstrap quantile regression 

# boot_quant <- boot(data=data_rel, statistic=bs_quant, R=1500, formula=score_object ~ group + score_general)
# g<-baseplot2
# for (i in sample(1500,300))
# {
#   df <- data.frame(score_general = boot_quant$t[i,][1:4], group = as.factor(boot_quant$t[i,][5:8]), score_object = boot_quant$t[i,][9:12])
#   levels(df$group)<-levels(data_rel$group)
#   g<-g+geom_line(data=df, aes(x=score_general, y=score_object, color=group), alpha=0.2)
# }
# g<-g+labs(x=longnames[5], y=longnames[7], title="Bootstrapped Quantile Regression", subtitle="300 out of 1500 samples") +
#   scale_y_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
#   scale_x_continuous(labels=scales::percent_format(accuracy = 1), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
#   coord_cartesian(ylim = c(-0.05, 1.05), xlim = c(-0.05, 1.05)) 
# ggsave(plot = g, filename = "quant_boot.png", width=10, height=10, dpi=300)
# g
