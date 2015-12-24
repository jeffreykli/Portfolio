---
title: "Lava Hackathon 2015 - PhET Simulation Dataset"
output:
  html_document:
    keep_md: yes
  pdf_document: default
---
```{r, echo=FALSE, message=FALSE, warning=FALSE, error=FALSE}
#Load dependencies
require(plyr)
require(dplyr)
require(ggplot2)
require(gridExtra)
require(scales)
```

```{r, echo=FALSE, error=FALSE, warning=FALSE, message=FALSE}
#Original data
event.data <- read.csv("C:/Users/Jeffrey/Desktop/Jeffrey/UBC Clubs/CASE COMP/LAVA Hackathon 2015/Physics Simulation - Ido Roll/_Data Team_/Transformed or Formatted Data/Data01.csv")

test.data <- read.csv("C:/Users/Jeffrey/Desktop/Jeffrey/UBC Clubs/CASE COMP/LAVA Hackathon 2015/Physics Simulation - Ido Roll/_Data Team_/Transformed or Formatted Data/test.data.csv")

#Check # students in events.data to match test.data
NumberOfStudents <- event.data %>%
	select(hashed.Student) %>%
	unique() %>%
	mutate(count = 1) %>%
	summarize(count = sum(count))

#Remove NA's in test.data
clean.test.data <- test.data %>%
	select(- pre.all.score, -Post.all.score) %>%
	filter(Pre.normal.score != "#N/A",
		Post.normal.score != "#N/A",
		Pre.transfer.score != "#N/A",
		Post.transfer.score != "#N/A") %>%
	mutate(Pre.normal.score = as.numeric(as.character(Pre.normal.score))) %>%
	mutate(Pre.transfer.score = as.numeric(as.character(Pre.transfer.score))) %>%
	mutate(Post.normal.score = as.numeric(as.character(Post.normal.score))) %>%
	mutate(Post.transfer.score = as.numeric(as.character(Post.transfer.score)))

#How many students used Game mode?
new.event.data <- event.data %>%
	mutate(Time.minute.second.miliseconds. = as.numeric(Time.minute.second.miliseconds.))

mode.data <- event.data %>%
	select(hashed.Student, Mode) %>%
	mutate(count = 1) %>%
	group_by(hashed.Student) %>%
	filter(Mode == "Game" | Mode == "Balance Lab") %>%
	distinct(Mode) %>%
	mutate(total.count = sum(count)) %>%
	ungroup() %>%
	arrange(hashed.Student) %>%
	distinct(hashed.Student) %>%
	select(-count, -Mode)

mode.data.matrix <- matrix(nrow = 2, ncol = 2)
colnames(mode.data.matrix) <- c("Mode", "Count")
mode.data.matrix[1 ,1] <- "Game and Lab"
mode.data.matrix[2, 1] <- "Lab Only"
mode.data.matrix[1, 2] <- 44
mode.data.matrix[2, 2] <- 42
mode.data.df <- as.data.frame(mode.data.matrix)

mode.plot <- ggplot(data = mode.data.df, aes(x = factor(Mode), y = Count, fill = Mode)) +
	geom_bar(stat = "identity", alpha = 0.5) +
	theme_minimal() +
	xlab("") +
	theme(legend.position = "none")

#Do the students who use game mode have less prior knowledge?
merged.mode.test <- inner_join(mode.data, clean.test.data, by = c("hashed.Student" = "hashed.Student"))

clean.mode.test <- merged.mode.test %>%
	mutate(Mode = ifelse(total.count == 1, "Balance Lab Only", "Game and Balance Lab")) %>%
	select(- total.count) %>%
	group_by(Mode) %>%
	mutate(avg.pre.normal = mean(Pre.normal.score)) %>%
	mutate(avg.post.normal = mean(Post.normal.score)) %>%
	mutate(avg.pre.transfer = mean(Pre.transfer.score)) %>%
	mutate(avg.post.transfer = mean(Post.transfer.score)) %>%
	select(- hashed.Student, - Pre.normal.score, - Post.normal.score, - Pre.transfer.score, - Post.transfer.score) %>%
	distinct(Mode)

mode.vs.score.matrix <- matrix(nrow = 8, ncol = 3)
colnames(mode.vs.score.matrix) <- c("Mode", "Type", "Score")
mode.vs.score.matrix[1:4, 1] <- c(rep("Game and Balance Lab", 4))
mode.vs.score.matrix[5:8, 1] <- c(rep("Balance Lab Only", 4))
mode.vs.score.matrix[1:8, 2] <- c(rep(c("Pre.Normal", "Post.Normal", "Pre.Transfer", "Post.Transfer"), 2))
mode.vs.score.matrix[1:4, 3] <- c(0.8014706, 0.867471, 0.338253, 0.6764706)
mode.vs.score.matrix[5:8, 3] <- c(0.7758621, 0.8706897, 0.3965517, 0.6551724)
mode.vs.score.df <- as.data.frame(mode.vs.score.matrix, stringsAsFactors = FALSE)
mode.vs.score.df$Score <- as.numeric(mode.vs.score.df$Score)
mode.vs.score.df$Type <- factor(mode.vs.score.df$Type, levels = c("Pre.Normal", "Post.Normal","Pre.Transfer", "Post.Transfer"))

mode.vs.normal.matrix <- matrix(nrow = 4, ncol = 3)
colnames(mode.vs.normal.matrix) <- c("Mode", "Type", "Score")
mode.vs.normal.matrix[1:2, 1] <- c(rep("Game and Balance Lab", 2))
mode.vs.normal.matrix[3:4, 1] <- c(rep("Balance Lab Only", 2))
mode.vs.normal.matrix[1:4, 2] <- c(rep(c("Pre.Normal", "Post.Normal"), 2))
mode.vs.normal.matrix[1:4, 3] <- c(0.8014706, 0.867471, 0.7758621, 0.9806897)
mode.vs.normal.df <- as.data.frame(mode.vs.normal.matrix, stringsAsFactors = FALSE)
mode.vs.normal.df$Score <- as.numeric(mode.vs.normal.df$Score)
mode.vs.normal.df$Type <- factor(mode.vs.normal.df$Type, levels = c("Pre.Normal", "Post.Normal"))

mode.vs.transfer.matrix <- matrix(nrow = 4, ncol = 3)
colnames(mode.vs.transfer.matrix) <- c("Mode", "Type", "Score")
mode.vs.transfer.matrix[1:2, 1] <- c(rep("Game and Balance Lab", 2))
mode.vs.transfer.matrix[3:4, 1] <- c(rep("Balance Lab Only", 2))
mode.vs.transfer.matrix[1:4, 2] <- c(rep(c("Pre.Transfer", "Post.Transfer"), 2))
mode.vs.transfer.matrix[1:4, 3] <- c(0.338253, 0.6764706, 0.3965517, 0.6551724)
mode.vs.transfer.df <- as.data.frame(mode.vs.transfer.matrix, stringsAsFactors = FALSE)
mode.vs.transfer.df$Score <- as.numeric(mode.vs.transfer.df$Score)
mode.vs.transfer.df$Type <- factor(mode.vs.transfer.df$Type, levels = c("Pre.Transfer", "Post.Transfer"))

#Plots
mode.vs.score.plot <- ggplot(data = mode.vs.score.df, aes(x = Type, y = Score, fill = Mode)) +
	geom_bar(stat = "identity", position = position_dodge(), alpha = 0.5) + 
	theme_minimal() +
	ylim(0, 1) +
	ggtitle("Mode vs. Score") +
  theme(plot.title = element_text(lineheight = 1, face = "bold"), axis.text.x = element_blank()) +
  geom_text(aes(label = Type), size = 4, position = position_dodge(width = 0.8), hjust = -0.1, angle = -90) +
	xlab("")

mode.vs.normal.line <- ggplot(data = mode.vs.normal.df, aes(x = Type, y = Score, group = Mode, colour = Mode)) +
	geom_line(size = 1.3) +
	geom_point() +
	theme_minimal() +
	ylim(0, 1) +
	xlab("") +
	ggtitle("Mode vs. Normal Score") +
  
	geom_text(aes(label = Score, vjust = 3), size = 3)

mode.vs.transfer.line <- ggplot(data = mode.vs.transfer.df, aes(x = Type, y = Score, group = Mode, colour = Mode)) +
	geom_line(size = 1.3) +
	geom_point() +
	theme_minimal() +
	ylim(0, 1) +
	xlab("") +
	ggtitle("Mode vs. Transfer Score") +
	geom_text(aes(label = Score, vjust = 1, hjust = 1.25), size = 3)
```

```{r, echo=FALSE, error=FALSE, warning=FALSE, message=FALSE}
mode.vs.score.plot
mode.vs.normal.line
mode.vs.transfer.line
```