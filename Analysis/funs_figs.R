########################################
# funs_figs.R

# Load package
library(ggplot2)
library(reshape2)

########################################
# Functions to create figures

# Horizontal bar plots of one categorical variable
plot_bar1 <- function(Var1, lab1, lab2="Frequency", colpal=allcol, return=T, plot=T, save=F, dims=dims1a, file="image") {
  tdf <- as.data.frame(table(Var1))
  tdf$Percent <- paste0(round(tdf$Freq/sum(tdf$Freq)*100,0),"%")
  tp <- ggplot(tdf, aes(x=Var1, y=Freq)) + 
    geom_col(fill=colpal) + coord_flip() + 
    scale_x_discrete(limits = rev) + 
    geom_text(aes(label=Percent), nudge_y=(1+2*max(tdf$Freq)/100)) + 
    labs(title=lab1, x=NULL, y=lab2) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(tdf) }
}

# Horizontal bar plots of multiple logical variables (T/F)
plot_barXTF <- function(df, lab1, lab2="Frequency", lab3="Legend", colpal=allcol, return=T, plot=T, save=F, dims=dims1a, file="image") {
  tdf <- data.frame(Response=character(), False=integer(), True=integer())
  for (i in names(df)) {
    ttab <- table(df[,i])
    trow <- data.frame(Response=i, False=ttab["FALSE"], True=ttab["TRUE"])
    tdf <- rbind(tdf, trow)
    rm(ttab, trow)
  }; rm(i)
  tdf[is.na(tdf)] <- 0
  tdf <- tdf[order(tdf$True, decreasing=T),]
  row.names(tdf) <- NULL
  tdf$Response <- factor(tdf$Response, levels=tdf$Response)
  tdf$Percent <- paste0(round(tdf$True/(tdf$False+tdf$True)*100,0), "%")
  tp <- ggplot(tdf, aes(x=Response, y=True)) + 
    geom_col(fill=colpal) + coord_flip() + 
    scale_x_discrete(limits = rev)+
    geom_text(aes(label=Percent), nudge_y=(1+2*max(tdf$True)/100)) + 
    labs(title=lab1, x=NULL, y=lab2) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(tdf) }
}

# Horizontal bar plots of multiple categorical variables
# with multiple same categories (e.g., likert scale), faceted by row
plot_barXABC <- function(df, sortby=NA, lab1, lab2="Frequency", lab3="Legend", colpal=allcolpal0, return=T, plot=T, save=F, dims=dims2a, file="image") {
  tlev <- levels(df[,1])
  tdf <- as.data.frame(matrix(data=NA, nrow=ncol(df), ncol=(1+length(tlev))))
  names(tdf) <- c("Response", tlev)
  for (i in 1:ncol(df)) {
    ttab <- table(df[,i])
    trow <- tdf[i,]
    trow[1,"Response"] <- names(df)[i]
    trow[1,2:ncol(trow)] <- ttab
    tdf[i,] <- trow
    rm(ttab, trow)
  }; rm(i)
  tdf[is.na(tdf)] <- 0
  if (!is.na(sortby)) {
    tdf <- tdf[order(tdf[,sortby], decreasing=T),]
  } else {
    ptdf <- 100*prop.table(as.matrix(tdf[,-1]), margin=1)
    tdf <- tdf[order(ptdf %*% c(ncol(ptdf):1), decreasing=T),]
    rm(ptdf)
  }
  ptdf <- round(prop.table(as.matrix(tdf[,-1]), margin=1)*100,0)
  ptdf <- apply(ptdf, 2, as.character)
  ptdf <- apply(ptdf, 2, paste0, "%")
  ptdf <- data.frame(tdf$Response, ptdf)
  names(ptdf) <- names(tdf)
  row.names(tdf) <- NULL
  row.names(ptdf) <- NULL
  tdfl <- melt(tdf, id.vars="Response")
  tdfl$Response <- factor(tdfl$Response, levels=rev(tdf$Response))
  tdfl$variable <- factor(tdfl$variable, levels = rev(levels(tdfl$variable))) 
  # tdfl$value2 <- ifelse(tdfl$variable=="True", tdfl$value, "")
  tp <- ggplot(tdfl, aes(fill=variable, y=value, x=Response)) + 
    geom_bar(position="stack", stat="identity") + 
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_brewer(palette=colpal, name=lab3) + 
    coord_flip() +
    labs(title=lab1, x=NULL, y=lab2) + # scale_fill_discrete(name=lab3) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(list(tdf, ptdf)) }
}


# Histogram of one continuous variable
plot_hist1 <- function(Var1, lab1, lab2="Frequency", title="Histogram", colpal=allcol, bins=5, return=T, plot=T, save=F, dims=dims1a, file="image") {
  tdf <- data.frame(Var1)
  tp <- ggplot(tdf, aes(Var1)) + 
    geom_histogram(binwidth=bins, fill=colpal,color="black") + 
    labs(x=lab1, y=lab2) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(summary(tdf)) }
}

# Horizontal box plots of multiple continuous variables (same scale)
plot_boxX <- function(df, lab1, lab2="Frequency", labv=names(df), colpal=allcol, return=T, plot=T, save=F, dims=dims1a, file="image") {
  names(df) <- labv
  tdfl <- melt(df, id.vars=0, na.rm=T)
  tp <- ggplot(tdfl, aes(y=value)) + 
    geom_boxplot(fill=colpal) + 
    facet_grid(rows=vars(variable), switch="y") + 
    coord_flip() + labs(x=lab1, y=lab2) + 
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(summary(df)) }
}

# Box plots of continuous variable vs categorical variable
plot_box_cat_cont <- function(df, catvar, contvar, lab1="Category", lab2="Value", title="Boxplot", colpal="lightblue", return=F, plot=T, save=F, dims=dims1a, file="image") {
  df <- data.frame(Category = df[[catvar]], Value = df[[contvar]])
  tp <- ggplot(df, aes(x=Category, y=Value, fill=Category)) + 
    geom_boxplot(outlier.color="red", fill=colpal) +
    labs(title=title, x=lab1, y=lab2) + 
    theme_bw() +
    theme(legend.position="none")
  
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(summary(df)) }
}

# Count overlapping points plot
plot_count2 <- function(df, lab1, lab2="Frequency", lab3="Legend", return=T, plot=T, save=F, dims=dims1a, file="image") {
  tdf <- as.data.frame(table(df[,1], df[,2]))
  tp <- ggplot(tdf, aes(x=Var1, y=Var2, size=Freq)) + 
    geom_count() + labs(x=lab1, y=lab2) + 
    geom_text(aes(label=Freq), nudge_y=0.1, size=5) + 
    scale_fill_discrete(name=lab3) + 
    theme_bw()
  if (save==T) {
    ggsave(filename=paste0(file, ".", dims[[1]]), plot=tp, device=dims[[1]], 
           width=dims[[2]], height=dims[[3]], units=dims[[4]], dpi=dims[[5]])
  }
  if (plot==T) { print(tp) }
  if (return==T) { return(tdf) }
}

########################################
# END
########################################