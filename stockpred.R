library(infotheo)
library(caret)
library(dplyr)
detach(HackathonRound)
library(reader)
str(HackathonRound)


HackathonRound <- filter(HackathonRound1,Share_Names=="Share1")
HackathonRound$Share_Names<-as.factor(HackathonRound$Share_Names)


new_set <- c()
for (row_set in seq(200)) {
  row_quant <- sample(10:30, 1)
  print(row_set)
  row_start <- sample(1:(nrow(HackathonRound) - row_quant), 1)
  market_subset <- HackathonRound[row_start:(row_start + row_quant),]
  market_subset <- mutate(market_subset,Close_Date = max(market_subset$Date),
Prev_Close_Diff =(Prev_Close-lag(Prev_Close))/lag(Prev_Close),
                                 Open_Price_Diff =(Open_Price-lag(Open_Price))/lag(Open_Price),
                                 High_Price_Diff =(High_Price-lag(High_Price))/lag(High_Price),
                                 Low_Price_Diff =(Low_Price-lag(Low_Price))/lag(Low_Price),
                                 Last_Price_Diff =(Last_Price-lag(Last_Price))/lag(Last_Price),
                                 Close_Price_Diff =(Close_Price-lag(Close_Price))/lag(Close_Price),
                                 Average_Price_Diff=(Average_Price-lag(Average_Price))/lag(Average_Price),
                                 Total_Traded_Quantity_Diff=(Total_Traded_Quantity-lag(Total_Traded_Quantity))/lag(Total_Traded_Quantity),
                                 Turnover_in_Lacs_Diff=(Turnover_in_Lacs-lag(Turnover_in_Lacs))/lag(Turnover_in_Lacs))
  list <- c("Share_Names","Close_Date","Prev_Close_Diff","Open_Price_Diff","High_Price_Diff","Low_Price_Diff","Last_Price_Diff","Close_Price_Diff","Average_Price_Diff","Total_Traded_Quantity_Diff","Turnover_in_Lacs","Deliverable_Qty","percent_DlyQt_Traded_Qty")
market_subset <- subset(market_subset,select =list)

  market_subset$Sequence_ID <- row_set 
new_set <- rbind(new_set, market_subset)
new_set <- na.omit(new_set)
}


list <- c("Share_Names","Close_Date","Prev_Close_Diff","Open_Price_Diff","High_Price_Diff","Low_Price_Diff","Last_Price_Diff","Close_Price_Diff","Average_Price_Diff","Total_Traded_Quantity_Diff","Turnover_in_Lacs","Deliverable_Qty","percent_DlyQt_Traded_Qty")
HackathonRound <- subset(new_set,select =list)

range(new_set$Prev_Close_Diff)
data_discretized <- discretize(new_set$Prev_Close_Diff,disc = "equalfreq",nbins = 3)
new_set$Prev_Close_Diff <-data_discretized$X
new_set$Prev_Close_Diff_LMH <- ifelse(new_set$Prev_Close_Diff ==1,'L',ifelse(new_set$Prev_Close_Diff ==2,'M','H'))



range(new_set$Open_Price_Diff)
data_discretized <- discretize(new_set$Open_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$Open_Price_Diff <-data_discretized$X
new_set$Open_Price_Diff_LMH <- ifelse(new_set$Open_Price_Diff ==1,'L',ifelse(new_set$Open_Price_Diff ==2,'M','H'))

range(new_set$High_Price_Diff)
data_discretized <- discretize(new_set$High_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$High_Price_Diff <-data_discretized$X
new_set$High_Price_Diff_LMH <- ifelse(new_set$High_Price_Diff ==1,'L',ifelse(new_set$High_Price_Diff ==2,'M','H'))

range(new_set$Low_Price_Diff)
data_discretized <- discretize(new_set$Low_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$Low_Price_Diff <-data_discretized$X
new_set$Low_Price_Diff_LMH <- ifelse(new_set$Low_Price_Diff ==1,'L',ifelse(new_set$Low_Price_Diff ==2,'M','H')) 

range(new_set$Last_Price_Diff)
data_discretized <- discretize(new_set$Last_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$Last_Price_Diff <-data_discretized$X
new_set$Last_Price_Diff_LMH <- ifelse(new_set$Last_Price_Diff ==1,'L',ifelse(new_set$Last_Price_Diff ==2,'M','H'))

range(new_set$Close_Price_Diff)
data_discretized <- discretize(new_set$Close_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$Close_Price_Diff <-data_discretized$X
new_set$Close_Price_Diff_LMH <- ifelse(new_set$Close_Price_Diff ==1,'L',ifelse(new_set$Close_Price_Diff ==2,'M','H'))

range(new_set$Average_Price_Diff)
data_discretized <- discretize(new_set$Average_Price_Diff,disc = "equalfreq",nbins = 3)
new_set$Average_Price_Diff <-data_discretized$X
new_set$Average_Price_Diff_LMH <- ifelse(new_set$Average_Price_Diff ==1,'L',ifelse(new_set$Average_Price_Diff ==2,'M','H'))

range(new_set$Total_Traded_Quantity_Diff)
data_discretized <- discretize(new_set$Total_Traded_Quantity_Diff,disc = "equalfreq",nbins = 3)
new_set$Total_Traded_Quantity_Diff <-data_discretized$X
new_set$Total_Traded_Quantity_Diff_LMH <- ifelse(new_set$Total_Traded_Quantity_Diff ==1,'L',ifelse(new_set$Total_Traded_Quantity_Diff ==2,'M','H'))

range(new_set$Turnover_in_Lacs)
data_discretized <- discretize(new_set$Turnover_in_Lacs,disc = "equalfreq",nbins = 3)
new_set$Turnover_in_Lacs <-data_discretized$X
new_set$Turnover_in_Lacs_Diff_LMH <- ifelse(new_set$Turnover_in_Lacs ==1,'L',ifelse(new_set$Turnover_in_Lacs ==2,'M','H'))

range(new_set$Deliverable_Qty)
data_discretized <- discretize(new_set$Deliverable_Qty,disc = "equalfreq",nbins = 3)
new_set$Deliverable_Qty <-data_discretized$X
new_set$Deliverable_Qty_Diff_LMH <- ifelse(new_set$Deliverable_Qty ==1,'L',ifelse(new_set$Deliverable_Qty ==2,'M','H'))

range(new_set$percent_DlyQt_Traded_Qty)
data_discretized <- discretize(new_set$percent_DlyQt_Traded_Qty,disc = "equalfreq",nbins = 3)
new_set$percent_DlyQt_Traded_Qty <-data_discretized$X
new_set$percent_DlyQt_Traded_Qty_Diff_LMH <- ifelse(new_set$percent_DlyQt_Traded_Qty ==1,'L',ifelse(new_set$percent_DlyQt_Traded_Qty ==2,'M','H'))



list1 <- c("Sequence_ID","Share_Names","Close_Date","Prev_Close_Diff_LMH","Open_Price_Diff_LMH","High_Price_Diff_LMH","Low_Price_Diff_LMH","Last_Price_Diff_LMH","Close_Price_Diff_LMH","Average_Price_Diff_LMH","Total_Traded_Quantity_Diff_LMH")

HackathonRoundbin <- subset(new_set,select = list1)
HackathonRoundbin$Combination <- paste0(HackathonRoundbin$Prev_Close_Diff_LMH,HackathonRoundbin$Open_Price_Diff_LMH,HackathonRoundbin$Close_Price_Diff_LMH)

#reduce
compressed_set <- dplyr::group_by(HackathonRoundbin, Sequence_ID, Close_Date) %>%
  dplyr::summarize(Combination = paste(Combination, collapse = ",")) %>%
  data.frame
compressed_set <- merge(x=compressed_set,y=HackathonRoundbin[,c(1,12)], by="Sequence_ID")

library(dplyr)
compressed_set_validation <- dplyr::filter(compressed_set, Close_Date >= Sys.Date()-90)
dim(compressed_set_validation)
compressed_set <- dplyr::filter(compressed_set, Close_Date < Sys.Date()-90)
dim(compressed_set)

compressed_set <- dplyr::select(compressed_set,-Close_Date) 
compressed_set_validation <- dplyr::select(compressed_set_validation,-Close_Date)

build_transition_grid <- function(compressed_grid, unique_patterns) {
  grids <- c()
  for (from_event in unique_patterns) {
    print(from_event)
    
    # how many times 
    for (to_event in unique_patterns) {
      pattern <- paste0(from_event, ',', to_event)
      IDs_matches <- compressed_grid[grep(pattern, compressed_grid$Combination),]
      if (nrow(IDs_matches) > 0) {
        Event_Pattern <- paste0(IDs_matches$Event_Pattern, collapse = ',', sep='~~')
        found <- gregexpr(pattern = pattern, text = Event_Pattern)[[1]]
        grid <- c(pattern,  length(found))
      } else {
        grid <- c(pattern,  0)
      }
      grids <- rbind(grids, grid)
    }
  }
  
  # create to/from grid
  grid_Df <- data.frame(pairs=grids[,1], counts=grids[,2])
  grid_Df$x <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 1)
  grid_Df$y <- sapply(strsplit(as.character(grid_Df$pairs), ","), `[`, 2)
  head(grids)
  
  all_events_count <- length(unique_patterns)
  transition_matrix = t(matrix(as.numeric(as.character(grid_Df$counts)), ncol=all_events_count, nrow=all_events_count))
  transition_dataframe <- data.frame(transition_matrix)
  names(transition_dataframe) <- unique_patterns
  row.names(transition_dataframe) <- unique_patterns
  head(transition_dataframe)
  
  # replace all NaN with zeros
  transition_dataframe[is.na(transition_dataframe)] = 0
  # transition_dataframe <- opp_matrix
  transition_dataframe <- transition_dataframe/rowSums(transition_dataframe) 
  return (transition_dataframe)
}

unique_patterns <- unique(strsplit(x = paste0(compressed_set$Combination, collapse = ','), split = ',')[[1]])
grid_PROB<- build_transition_grid(compressed_set, unique_patterns)



