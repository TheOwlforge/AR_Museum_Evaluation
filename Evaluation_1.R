# library(forcats) #factors
library(plyr) #revalue
library(dplyr) #mutate, case_when
# library(lattice) #plotting
library(ggplot2) #plotting
library(reshape2) #melt
library(corrplot)
library(MASS) #plotr
library(nnet) #multinom
library(RColorBrewer) #more color scales
library(scales)#percent

########################################################
################# reading in data ######################
########################################################

#anglisized
#usability answers deleted with app usage no
#nr 38 deleted, too less answeres, only 4/27
#last two feedback questions deleted
#categoriezed history questions
data_orig <- read.csv(file ="data/data_cleaned.csv", 
                      na.strings = "NA")

########################################################
################# data wrangling  ######################
########################################################

#reorder gender
data_orig$a2 <- factor(data_orig$a2, levels=c("female", "male", "diverse"))
data_orig$a1 <- as.factor(data_orig$a1)
data_orig$a3 <- as.factor(data_orig$a3)

#reverse b1 and b6
b1_fac <- revalue(as.factor(data_orig$b1), c("1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1"))
data_orig$b1 <- as.numeric(as.character(b1_fac))
b6_fac <- revalue(as.factor(data_orig$b6), c("1" = "5", "2" = "4", "3" = "3", "4" = "2", "5" = "1"))
data_orig$b6 <- as.numeric(as.character(b6_fac))

# Paket mit %>% pipeline
# data <- data %>% mutate(q3=factor(....), q1=factor(....)) %>% mutate(x1=q1+q3) 

#extract checked aspect (multiple were possible)
data_orig <- mutate(data_orig, aspect_t=case_when(data_orig$i5 %in% c("t", "kt", "ti", "kti") ~ TRUE,
                                                         is.na(data_orig$i5) ~ NA,
                                                         TRUE ~ FALSE))
data_orig <- mutate(data_orig, aspect_a=case_when(data_orig$i5 %in% c("k", "kt", "ki", "kti") ~ TRUE,
                                                         is.na(data_orig$i5) ~ NA,
                                                         TRUE ~ FALSE))
data_orig <- mutate(data_orig, aspect_i=case_when(data_orig$i5 %in% c("i", "ti", "ki", "kti") ~ TRUE,
                                                         is.na(data_orig$i5) ~ NA,
                                                         TRUE ~ FALSE))

#extract correctness of open questions
data_orig <- mutate(data_orig, q1=factor(factor(case_when(data_orig$w7 %in% c("Fuß, Leisten", "Holzfuß", "Schuh, Leisten") ~ "correct",
                                                          data_orig$w7 %in% c("Leisten") ~ "special",
                                                          is.na(data_orig$w7) ~ NA_character_,
                                                          TRUE ~ "wrong")), levels = c("wrong", "correct", "special"), ordered=TRUE))
data_orig <- mutate(data_orig, q2=factor(factor(case_when(data_orig$w8 %in% c("Töchter", "trauernde Angehörige, Töchter", "Tochter") ~ "correct",
                                                          data_orig$w8 %in% c("Chiton, Tochter", "heiratsfähige Frau", "junge Frau, Tochter") ~ "special",
                                                          is.na(data_orig$w8) ~ NA_character_,
                                                          TRUE ~ "wrong")), levels = c("wrong", "correct", "special"), ordered=TRUE))
data_orig <- mutate(data_orig, q3=factor(factor(case_when(data_orig$w9 %in% c("Xanthippos") ~ "correct",
                                                          data_orig$w9 %in% c("Xanthippos, blondes Pferd", "Xanthos Hippos") ~ "special",
                                                          is.na(data_orig$w9) ~ NA_character_,
                                                          TRUE ~ "wrong")), levels = c("wrong", "correct", "special"), ordered=TRUE))

#extract sum of correct or better answered questions
sum_correct <- rowSums(data_orig[c("q1", "q2", "q3")] == "correct") + rowSums(data_orig[c("q1", "q2", "q3")] == "special")
sum_correct[is.na(sum_correct)] <- 0
data_orig <- mutate(data_orig, q_sum=factor(sum_correct, ordered = TRUE))
table(data_orig$q_sum)

#correct highlight question
#most people answered something even though they didn't use Info Screen
data_orig$w5[data_orig$w1==1] <- 1
data_orig$w5[data_orig$w1==2] <- 1
data_orig$w5[data_orig$w1==3] <- 1

#extract scores of question groups
integration_data <- data_orig[,(15:18)]
integration_data <- integration_data %>% rowwise() %>% mutate(Avg=mean(c(i1, i2, i3, i4), na.rm=T))
integration_score <- mean(integration_data$Avg, na.rm=T)
data_orig <- mutate(data_orig, score_i=integration_data$Avg)

usability_data <- data_orig[,(20:25)]
usability_data <- usability_data %>% rowwise() %>% mutate(Avg=mean(c(b1, b2, b3, b4, b5, b6), na.rm=T))
usability_score <- mean(usability_data$Avg, na.rm=T)
data_orig <- mutate(data_orig, score_u=usability_data$Avg)

knowledge_data <- data_orig[,(6:10)]
knowledge_data <- knowledge_data %>% rowwise() %>% mutate(Avg=mean(c(w1, w2, w3, w4, w5), na.rm=T))
knowledge_score <- mean(knowledge_data$Avg, na.rm=T)
data_orig <- mutate(data_orig, score_k=knowledge_data$Avg)

#overview of the original data
summary(data_orig)
integration_score
usability_score
knowledge_score

#extract numerical data
data_num <- subset(data_orig, select = c(-a2, -a3, -w7, -w8, -w9, -i5, -group, -aspect_t,
                                         -aspect_a, -aspect_i, -q1, -q2, -q3))
data_num$q_sum <- as.numeric(data_num$q_sum)
data_num$a1 <- case_when(data_num$a1 %in% c("16-20") ~ 18,
                                 data_num$a1 %in% c("21-25") ~ 23,
                                 data_num$a1 %in% c("25-35") ~ 30,
                                 data_num$a1 %in% c("36-45") ~ 40,
                                 data_num$a1 %in% c("46-55") ~ 50,
                                 data_num$a1 %in% c("56-65") ~ 60,
                                 data_num$a1 %in% c("66+") ~ 70)


#create ordered factors from numerical data with different abstractions
data_cut3 <- data_orig
data_cut3$a4 <- cut(data_cut3$a4, c(0,3,5), right = FALSE, labels = c("rarely or never", "often or sometimes"), ordered_result = TRUE)
data_cut3$a5 <- cut(data_cut3$a5, c(0,3,5), right = FALSE, labels = c("rarely or never", "often or sometimes"), ordered_result = TRUE)
data_cut3$w1 <- cut(data_cut3$w1, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$w2 <- cut(data_cut3$w2, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$w3 <- cut(data_cut3$w3, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$w4 <- cut(data_cut3$w4, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$w5 <- cut(data_cut3$w5, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$w6 <- cut(data_cut3$w6, c(0,3,4,6), right = FALSE, labels = c("no", "neutral", "yes"), ordered_result = TRUE)
data_cut3$i1 <- cut(data_cut3$i1, c(0,3,4,6), right = FALSE, labels = c("negative", "neutral", "positive"), ordered_result = TRUE)
data_cut3$i2 <- cut(data_cut3$i2, c(0,3,4,6), right = FALSE, labels = c("negative", "neutral", "positive"), ordered_result = TRUE)
data_cut3$i3 <- cut(data_cut3$i3, c(0,3,4,6), right = FALSE, labels = c("negative", "neutral", "positive"), ordered_result = TRUE)
data_cut3$i4 <- cut(data_cut3$i4, c(0,3,4,6), right = FALSE, labels = c("negative", "neutral", "positive"), ordered_result = TRUE)
data_cut3$b1 <- cut(data_cut3$b1, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
data_cut3$b2 <- cut(data_cut3$b2, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
data_cut3$b3 <- cut(data_cut3$b3, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
data_cut3$b4 <- cut(data_cut3$b4, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
data_cut3$b5 <- cut(data_cut3$b5, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
data_cut3$b6 <- cut(data_cut3$b6, c(0,3,4,6), right = FALSE, labels = c("difficult", "neutral", "easy"), ordered_result = TRUE)
summary(data_cut3)


data_cut5 <- data_orig
data_cut5$a4 <- cut(data_cut5$a4, c(0,2,3,4,5), right = FALSE, labels = c("never", "rarely", "sometimes", "often"), ordered_result = TRUE)
data_cut5$a5 <- cut(data_cut5$a5, c(0,2,3,4,5), right = FALSE, labels = c("never", "rarely", "sometimes", "often"), ordered_result = TRUE)
data_cut5$w1 <- cut(data_cut5$w1, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$w2 <- cut(data_cut5$w2, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$w3 <- cut(data_cut5$w3, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$w4 <- cut(data_cut5$w4, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$w5 <- cut(data_cut5$w5, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$w6 <- cut(data_cut5$w6, c(0,2,3,4,5,6), right = FALSE, labels = c("no", "not_really", "neutral", "rather_yes", "yes"), ordered_result = TRUE)
data_cut5$i1 <- cut(data_cut5$i1, c(0,2,3,4,5,6), right = FALSE, labels = c("negative", "rather_negative", "neutral", "rather_positive", "positive"), ordered_result = TRUE)
data_cut5$i2 <- cut(data_cut5$i2, c(0,2,3,4,5,6), right = FALSE, labels = c("negative", "rather_negative", "neutral", "rather_positive", "positive"), ordered_result = TRUE)
data_cut5$i3 <- cut(data_cut5$i3, c(0,2,3,4,5,6), right = FALSE, labels = c("negative", "rather_negative", "neutral", "rather_positive", "positive"), ordered_result = TRUE)
data_cut5$i4 <- cut(data_cut5$i4, c(0,2,3,4,5,6), right = FALSE, labels = c("negative", "rather_negative", "neutral", "rather_positive", "positive"), ordered_result = TRUE)
data_cut5$b1 <- cut(data_cut5$b1, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_cut5$b2 <- cut(data_cut5$b2, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_cut5$b3 <- cut(data_cut5$b3, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_cut5$b4 <- cut(data_cut5$b4, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_cut5$b5 <- cut(data_cut5$b5, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
data_cut5$b6 <- cut(data_cut5$b6, c(0,2,3,4,5,6), right = FALSE, labels = c("difficult", "rather_difficult", "neutral", "rather_easy", "easy"), ordered_result = TRUE)
summary(data_cut5)

age_ordered <- factor(data_cut5$a1, ordered = TRUE, levels = levels(data_cut5$a1))
summary(age_ordered)
mode(age_ordered)
quantile(age_ordered, 0.25, type=1)
quantile(age_ordered, 0.5, type=1)
quantile(age_ordered, 0.75, type=1)
IQR(age_ordered)

#extract count of votes per aspect
aspect_a <- table(data_orig$aspect_a)["TRUE"]
aspect_t <- table(data_orig$aspect_t)["TRUE"]
aspect_i <- table(data_orig$aspect_i)["TRUE"]
aspect_data <- data.frame(
  aspect = factor(c("artistic","technical", "informative"),
                  levels=c("artistic","technical", "informative")),
  aspect_count = c(aspect_a, aspect_t, aspect_i)
)
summary(aspect_data)
print(aspect_data)

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
    ggsave(paste0(img_path, "dataplot_", idx, "_", longnames[idx], ".png"),
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
    ggsave(paste0(img_path, "piechart_", idx, "_", longnames[idx], ".png"),
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
  ggsave(paste0(img_path, filename, ".png"),
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
  ggsave(paste0(img_path, "testplot_", i, "_", j, "_fill.png"), width = img_width, height = img_height, dpi = img_dpi)
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
  ggsave(paste0(img_path, "testplot_", i, "_", j, "_stack.png"), width = img_width, height = img_height, dpi = img_dpi)
}

#produces a boxplot
plot_dependent_box <- function(data1, data2, i, j, legend_name, palette, ...){
  df<-data.frame(data1[,i])
  df <- df %>%  mutate(d2 = data2[,j])
  ggplot(df, aes(x=df[,1], y=df[,2])) +
    scale_y_continuous(expand = c(0,0), limits = c(0,5.5)) +
    geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4, aes(fill=df[,1])) +
    geom_jitter(shape=16, width = 0.15, height = 0.05, size = 2) +
    stat_summary(fun.y=mean, geom="point", shape=4, size=4) +
    scale_fill_brewer(palette = palette, name = legend_name) +
    theme_classic(base_size = img_size) +
    #theme(legend.position="bottom") +
    theme(legend.text=element_text(size=rel(0.9))) +
    labs(...)
  ggsave(paste0(img_path, "boxplot_", i, "_", j, ".png"),
         width = img_width, height = img_height, dpi = img_dpi)
}

################################################
################# Globals ######################
################################################

longnames <- c("Age Group", "Gender", "App Usage", "Mobile Device Usage", "Drawing Frequency",
               "Used\nInfoscreen", "Read\nObject Panel", "Motivation",
               "Text Length\nAppropriateness", "Noticed\nHighlights", "Background\nKnowledge",
               "Last Question", "Daughter Question", "Name Question",
               "Enhancment", "Integration", "Would Use Again",
               "Request for more Digital Apps", "Aspect Combination",
               "General Difficulty", "Help Usefull", "Color Selection Difficulty",
               "Drawing Process Difficulty", "Artistic Freedom", "Creativity", "Group",
               "Technical Aspect", "Artistic Aspect", "Informative Aspect",
               "Answer Correctness", "Answer Correctness", "Answer Correctness",
               "Number of correct or better answers",
               "Integration Score", "Usability Score", "Knowledge Score")

img_size <- 25
img_linesize <- 2
img_pointsize <- 7
img_textsize <- 8
img_width <- 10
img_height <- 7
img_dpi <- 300
img_path <- "plots/Survey1/"

################# plots ######################
#plot basic variables a1, a2, a3, group, q1, q2, q3, q_sum
indices_basic <- c(1,2,3,26,30,31,32,33)
legend_names_basic <- c("Age Group", "Gender", "App Usage", "Group",
                        "Level of\nCorrectness", "Level of\nCorrectness", "Level of\nCorrectness",
                        "Correct Answers\nCount")
titles_basic <- c("The Age Distribution of the Participants",
                  "The Gender Distribution of the Participants",
                  "Did the Visitors interact with the App?",
                  "Participants per Event",
                  "What is the man holding? A Last.",
                  "Who are the two smaller figures? His Daughters.",
                  "What is the name of the man? Xanthippos.",
                  "Overall score in the historic questions")
palette_basic <- c("Oranges","Paired","Set1","Set3","Spectral","Spectral","Spectral","RdPu")
plot_simple(data_orig, indices_basic, longnames, titles_basic, legend_names_basic, palette_basic)

#plot general questions a4, a5
indices_general <- c(4,5)
legend_names_general <- c("Mobile Device Usage", "Drawing Frequency")
titles_general <- c("Mobile Device Usage among the Participants",
                    "Drawing Frequency among the Participants")
palette_general <- c("GnBu", "GnBu")
plot_simple(data_cut5, indices_general, longnames, titles_general, legend_names_general, palette_general)

#plot aspect frequency
ggplot(data=aspect_data, aes(x=aspect, y=aspect_count, fill=aspect)) +
  scale_y_continuous(expand = c(0,0), limits = c(0, range(aspect_data$aspect_count)[2] + 2)) +
  geom_bar(stat="identity") +
  geom_bar(stat="identity", color="black", show.legend = FALSE) +
  theme_classic(base_size=img_size) +
  theme(legend.text=element_text(size=rel(1))) +
  labs(x="Aspect", 
       y="Number of Participants",
       title="Which Aspect of the App is the most intriguing?")+#,
       #subtitle="(multiple answeres were possible)") +
  scale_fill_brewer(name = "Aspect", na.value="grey", palette="Dark2")
ggsave(paste0(img_path, "dataplot_aspect_data.png"), width=img_width, height=img_height, dpi=img_dpi)

#plot integration questions
col_names_vec = c("Enrichment","Integration","Use again", "More digital Apps")
legend_name = "Integration\nFeedback"
x="Answers to Integration Questions"
y="Number of Participants"
title="Summary of all Questions regarding Integration"

integration_data <- data_cut3[,(15:18)]
plot_multiple(data = integration_data, col_names_vec, "dataplot_integration_questions_cut3.png",
              legend_name, c(1,2,3,4,5), x=x, y=y, title=title)

integration_data <- data_cut5[,(15:18)]
plot_multiple(data = integration_data, col_names_vec, "dataplot_integration_questions_cut5.png",
              legend_name, c(1,4,2,5,3,6,7), x=x, y=y, title=title)

#plot usability questions
col_names_vec = c("Operation","Help","Color\nSelection", "Tools", "Artistic\nfreedom", "Creativity")
legend_name = "Usability\nFeedback"
x="Answers to Usability Questions"
y="Number of Participants"
title="Summary of all Questions regarding Usability"

usability_data <- data_cut3[,(20:25)]
plot_multiple(data = usability_data, col_names_vec, "dataplot_usability_questions_cut3.png",
              legend_name, c(4,1,3,2,5), x=x, y=y, title=title)

usability_data <- data_cut5[,(20:25)]
plot_multiple(data = usability_data, col_names_vec, "dataplot_usability_questions_cut5.png",
              legend_name, c(1,4,3,5,2,6,7), x=x, y=y, title=title)

#plot historic questions
legend_name = "Question\nCorrectness"
x="Answers to Historic Questions"
y="Number of Participants"
title="Summary of all Historic Questions"
plot_multiple(data = data_orig[,(30:32)], longnames[12:14], "dataplot_historic_questions.png",
              legend_name, c(3,1,2,4,5), x=x, y=y, title=title)

#plot knowledge related questions
col_names_vec = longnames[6:11]
legend_name = "Answer"
x="Answers to Knowledge Transfer Questions"
y="Number of Participants"
title="Summary of all Questions regarding Knowledge Transfer"

knowledge_data <- data_cut3[,(6:11)]
plot_multiple(data = knowledge_data, col_names_vec, "dataplot_knowledge_questions_cut3.png",
              legend_name, c(1,2,3,4,5), x=x, y=y, title=title)

knowledge_data <- data_cut5[,(6:11)]
plot_multiple(data = knowledge_data, col_names_vec, "dataplot_knowledge_questions_cut5.png",
              legend_name, c(1,2,3,4,5,6,7), x=x, y=y, title=title)

################# find dependencies ####################
# Compute correlation coefficient
cor.test(data_num$a5,data_num$i3, method = "kendall", use = "pair")
cor.test(data_num$a1,data_num$b5, method = "kendall", use = "pair")
cor.test(data_num$a1,data_num$b1, method = "kendall", use = "pair")
cor.test(data_num$a4,data_num$score_i, method = "spearman", use = "pair")
cor.test(data_num$a4,data_num$score_u, method = "kendall", use = "pair")
cor.test(data_num$a4,data_num$score_k, method = "spearman", use = "pair")
testcor <- cor(data_num, method = "kendall", use = "pair")
corp <- cor.mtest(testcor, conf.level=0.99)
png(paste0(img_path, "corrplot.png"), width=3000, height=3000, pointsize=90)
corrplot(testcor, p.mat=corp$p, method="color", type="upper", addgrid.col = "black",
         insig="blank", title="Correlation between the numerical variables",
         tl.col="black", diag=FALSE, mar=c(0,0,1,0), cl.pos = "n",
         addCoef.col = "black", number.cex = 0.25)
dev.off()

d <- data_cut3[data_cut3$w1=="yes",]
d <- d[!is.na(d$w1),]
summary(d)

#plot factor vs factor

#17=i3 (use again)
#20=b1 (operation)
#23=b4 (drawing process dificulty)
#24=b5 (artistic freedom)
#25=b6 (creativity)
#33=q_sum

idx_a <- c(1,1,4,4,4,4,4,5,5,5,6,7,11)
idx_b <- c(20,23,20,23,1,2,5,17,24,25,33,33,33)
palette <- c("Viridis","Viridis","Viridis","Viridis","Oranges","Paired", "GnBu", "Viridis",
             "Viridis", "Viridis", "RdPu", "RdPu", "RdPu")
for (i in 1:length(idx_a)){
  plot_dependent_bar_stack(data_cut5, idx_a[i], idx_b[i], longnames[idx_b[i]],
                           x = longnames[idx_a[i]], y = "Number of Participants",
                           title = paste0(longnames[idx_a[i]], " vs. ", longnames[idx_b[i]]),
                           palette = palette[i])
  plot_dependent_bar_fill(data_cut5, idx_a[i], idx_b[i], longnames[idx_b[i]],
                          x = longnames[idx_a[i]], y = "Number of Participants (norm.)",
                          title = paste0(longnames[idx_a[i]], " vs. ", longnames[idx_b[i]]),
                          palette = palette[i])
}

#plot factor vs score 
plot_dependent_box(data1=data_cut5, data2=data_num, i=1, j=18, legend_name=longnames[24],
                   palette = "Oranges", x=longnames[1],y=longnames[24],
                   title="Correlation of age and artistic fulfillment",
                   subtitle="Kendall's tau: -0.363, p-value: 0.0118")
plot_dependent_box(data1=data_cut5, data2=data_cut5, i=4, j=34, legend_name=longnames[34],
                   palette = "GnBu", x=longnames[4],y=longnames[34],
                   title="Mobile device usage and integration",
                   subtitle="Spearman's rho: 0.3144, p-value: 0.0426")
plot_dependent_box(data1=data_cut5, data2=data_cut5, i=4, j=36, legend_name=longnames[36],
                   palette = "GnBu", x=longnames[4],y=longnames[36],
                   title="Mobile device usage and knowledge",
                   subtitle="Spearman's rho: -0.378, p-value: 0.0162")

#plot interesting ones again in nice
plot_dependent_bar_stack(data_cut5, 6, 33, "Correct Answers\nCount",
                         x = longnames[6], y = "Number of Participants",
                         title = "Relation between using the Infoscreen\nand the number of correct or better answers",
                         subtitle = "correlat.: 0.11, p-value: 0.09225, not significant",
                         palette = "RdPu")
plot_dependent_bar_stack(data_cut5, 7, 33, "Correct Answers\nCount",
                         x = longnames[7], y = "Number of Participants",
                         title = "Relation between reading the Panel\nand the number of correct or better answers",
                         subtitle = "correlat.: 0.32, p-value: 0.01338, significant",
                         palette = "RdPu")
plot_dependent_bar_stack(data_cut5, 11, 33, "Correct Answers\nCount",
                         x = longnames[11], y = "Number of Participants",
                         title = "Relation between prior knowledge\nand the number of correct or better answers",
                         subtitle = "Kendall-Correlation Coefficient: 0.36",
                         palette = "RdPu")
plot_dependent_bar_fill(data_cut5, 5, 17, "Would\nUse Again",
                        x = longnames[5], y = "Number of Participants (norm.)",
                        title = "Relation between Drawing Frequency\nand would use again",
                        subtitle = "Kendall's tau: -0.32, p-value: 0.0238",
                        palette = "Viridis")

################# testing ####################
#p-value correction bei multiple testing through bonferroni korrektur (0.05 / number of tests)
#chisq.test for two factors (compute table before)
#t.test for numerical data
#aov(data, bla~blub) for factor-number combination

test1 <- t.test(x=data_num$i3, y=data_num$a5)
print(test1)
if(test1$p.value < 0.05/3) {
  print("i3 and a5 statistically significant")
}

test2 <- chisq.test(table(data_cut5$q_sum, data_cut5$w1))
print(test2)
if(test2$p.value < 0.05/3) {
  print("q_sum and w1 statistically significant")
}

test3 <- chisq.test(table(data_cut5$q_sum, data_cut5$w2))
print(test3)
if(test3$p.value < 0.05/3) {
  print("q_sum and w2 statistically significant")
}

# Regression, achtung logarithmisch und unsymmetrisch
# glm for numerical data
# polr for ordered factors
# multinom for normal factors

glm(i3 ~ a5, data = data_num)
polr(q_sum ~ w2, data = data_cut5)
