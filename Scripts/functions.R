#### Top level ####
setwd("C:/Users/Michael/Google Drive/R stuff/3 - Fall 2020/POLISCI919/Project")
w <- tidyselect::vars_select_helpers$where
`%notin%` <- Negate(`%in%`)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#### Tidy functions ####
tidy.gbm <- function(x,
                     n_trees = x$n.trees,
                     scale = FALSE,
                     sort = TRUE,
                     normalise = TRUE,
                     ...){
  
  if (n_trees < 1) {
    stop("n_trees must be greater than 0.")
  }
  
  if (n_trees > x$n.trees) {
    warning("Exceeded total number of GBM terms. Results use n.trees=",
            x$n.trees, " terms.\n")
    n_trees <- x$n.trees
  }
  
  imp_df <- gbm::relative.influence(object = x,
                                    n.trees = n_trees,
                                    scale. = scale,
                                    sort. = sort
  )
  
  imp_df2 <- varImp(object = x, numTrees=n_trees) %>%
    data.frame() %>%
    rownames_to_column(., var = "variable") %>%
    rename(importance=Overall) %>%
    arrange(desc(importance))
  
  imp_df[imp_df < 0] <- 0
  
  imp_df <- tibble::tibble(variable=names(imp_df),
                           rel_importance=imp_df,
                           importance=imp_df2$importance)
  
  if (sort) {
    # arrange the rows by importance
    imp_df <- imp_df[order(imp_df$rel_importance,
                           decreasing = TRUE), ]
  }
  
  if (normalise) {
    imp_df$rel_importance <- 100 * imp_df$rel_importance/
      sum(imp_df$rel_importance)
  }
  
  return(tibble::as_tibble(imp_df))
  
  
}

tidy.ordfor <- function(final_model){
  imp <- final_model$variable.importance %>%
    data.frame(importance=.) %>%
    rownames_to_column(., var="variable") %>%
    arrange(desc(importance)) %>%
    mutate(rel_importance=100*importance/sum(importance)) %>%
    left_join(categories, by="variable") %>%
    select(variable, category, everything()) %>%
    tibble()
  
  return(imp)
}

color_gradient <- function(x, colors=c("red", "white"), 
                           colsteps=20) {
  return( colorRampPalette(colors) (colsteps)
          [ findInterval(x, seq(min(x),max(x),
                                length.out=colsteps)) ] )
}

#### Plotting function ####
my_plot_func <- function(data_for_plot, 
                         grp=c("AAS", "ARAS_S",
                               "ARAS_P", "ARAS_H"), 
                         n_vars){
  
  data_for_plot <- data_for_plot %>%
    mutate(short_descr=case_when(
      str_detect(short_descr, "\\(")~
        str_replace(short_descr, "(.*)\\s(\\(.*\\))",
                    "\\1\n\\2"),
      T~short_descr
    ))
  
  grp <- grp[1]
  
  pcts <- data_for_plot %>%
    filter(Group %in% grp
    ) %>%
    group_by(category, S_or_F) %>%
    summarize(prop=sum(rel_importance)) %>%
    full_join(data_for_plot %>% 
                drop_na() %>% 
                pull(category) %>%
                levels() %>%
                rep(., times=2) %>%
                tibble(category=.,
                       S_or_F=rep(c("Student", "Faculty"),
                                  each=11)) %>%
                mutate(category=factor(category,
                                       levels=c("Attitude",
                                                "Pain/Distress",
                                                "Purpose",
                                                "Species",
                                                " ",
                                                "Demographic",
                                                "Personal",
                                                "Sources",
                                                "Rules",
                                                "  ",
                                                "General"))),
              by=c("category", "S_or_F")) %>%
    mutate(prop=ifelse(is.na(prop),
                       0, 
                       round(prop, 1)),
           prop_print=ifelse(category%notin%c(" ", "  "),
                             paste(category, "\n(",
                                   prop, "%) ", sep=""),
                             as.character(category)),
           pct=paste(prop, "%", sep="")) %>%
    arrange(category) %>%
    filter(category %notin% c(" ", "  "))
  
  tt <- pcts %>% 
    select(category, S_or_F, pct) %>%
    rename(Category=category, "% of Total"=pct) %>%
    split(., .$S_or_F) %>%
    lapply(., select, -S_or_F)
  
  tt <- tt[c("Faculty", "Student")]
  
  tt <- tibble(x=rep(Inf, length(tt)),
               y=rep(-Inf, length(tt)),
               S_or_F=c("Faculty", "Student"),
               lab=tt,
               col=pcts %>%
                 group_by(S_or_F) %>%
                 mutate(col=color_gradient(abs(prop),
                                           c("white", "coral"))) %>%
                 select(S_or_F, col) %>%
                 ungroup() %>%
                 split(., .$S_or_F) %>%
                 lapply(., select, -S_or_F) %>%
                 lapply(., pull, col))
  
  vars <- data_for_plot %>%
    filter(Group %in% grp
    ) %>%
    group_by(S_or_F) %>%
    slice_max(order_by=rel_importance, n=n_vars) %>%
    pull(variable) %>% 
    unique()
  
  pl_d <- data_for_plot %>%
    filter(Group %in% grp
           , variable %in% vars) %>%
    arrange(desc(rel_importance)) %>%
    ungroup() %>%
    mutate(subq=fct_relevel(subq, 
                            as.character(rev(unique(subq)))),
           question=fct_relevel(question, 
                                as.character(rev(unique(question)))),
           short_descr=fct_relevel(short_descr, 
                                   as.character(rev(unique(short_descr)))))
  
  pp_f <- ggplot(data=subset(pl_d, S_or_F=="Faculty"),
                 aes(x=rel_importance,
                     y=short_descr,
                     fill=category)) +
    geom_col(position="dodge") +
    theme_bw() +
    theme(axis.text.y = element_text(size=11)) +
    facet_grid(~S_or_F) +
    labs(x="Proportional Importance (%)",
         y="Questionnaire Item",
         title=NULL) +
    scale_fill_manual(values=c(cbPalette[1:4],
                               "white",
                               cbPalette[5:8],
                               "white",
                               "#000000"),
                      name="Item Category",
                      drop=F,
                      guide=F) +
    shadowtext::geom_shadowtext(data=subset(pl_d, 
                                            rel_importance>=0.85*max(rel_importance)&
                                              S_or_F=="Faculty"),
                                aes(label=round(rel_importance, 1)),
                                hjust=1, color="white",
                                nudge_x=-0.5,
                                fontface="bold") +
    geom_text(data=subset(pl_d,  rel_importance<0.85*max(rel_importance)&
                            S_or_F=="Faculty"),
              aes(label=round(rel_importance, 1)),
              hjust=0, color="black",
              nudge_x=0.5,
              fontface="bold") +
    geom_table(data=subset(tt, S_or_F=="Faculty"),
               aes(x=x, y=y, label=lab),
               fill="white",
               col="black",
               size = 1,
               table.theme=ttheme_default(
                 core=list(bg_params = 
                             list(fill = c(c(cbPalette[1:4],
                                             cbPalette[5:8],
                                             "#000000"),
                                           subset(tt, S_or_F=="Faculty")$
                                             col[["Faculty"]]),
                                  col='grey50'),
                           fg_params=list(cex=0.7,
                                          col=c(rep("black", 8),
                                                "white",
                                                rep("black", 9)))),
                 colhead=list(bg_params = 
                                list(col="grey50"),
                              fg_params=list(cex=0.7))
               )) +
    xlim(c(0, max(pl_d$rel_importance)))
  
  pp_s <- ggplot(data=subset(pl_d, S_or_F=="Student"),
                 aes(x=rel_importance,
                     y=short_descr,
                     fill=category)) +
    geom_col(position="dodge") +
    theme_bw() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          plot.margin = unit(c(5.5, 5.5, 5.5, 0), "pt")) +
    facet_grid(~S_or_F) +
    labs(x="Proportional Importance (%)",
         y=NULL,
         title=NULL) +
    scale_fill_manual(values=c(cbPalette[1:4],
                               "white",
                               cbPalette[5:8],
                               "white",
                               "#000000"),
                      name="Item Category",
                      drop=F) +
    shadowtext::geom_shadowtext(data=subset(pl_d, 
                                rel_importance>=0.85*max(rel_importance)&
                                              S_or_F=="Student"),
                                aes(label=round(rel_importance, 1)),
                                hjust=1, color="white",
                                nudge_x=-0.5,
                                fontface="bold") +
    geom_text(data=subset(pl_d,  rel_importance<0.85*max(rel_importance)&
                            S_or_F=="Student"),
              aes(label=round(rel_importance, 1)),
              hjust=0, color="black",
              nudge_x=0.5,
              fontface="bold") +
    geom_table(data=subset(tt, S_or_F=="Student"),
               aes(x=x, y=y, label=lab),
               fill="white", 
               col="black", 
               size = 1,
               table.theme=ttheme_default(
                 core=list(bg_params = 
                             list(fill = c(c(cbPalette[1:4],
                                             cbPalette[5:8],
                                             "#000000"),
                                           subset(tt, S_or_F=="Student")$
                                             col[["Student"]]),
                                  col='grey50'),
                           fg_params=list(cex=0.7,
                                          col=c(rep("black", 8),
                                                "white",
                                                rep("black", 9)))),
                 colhead=list(bg_params = 
                                list(col="grey50"),
                              fg_params=list(cex=0.7))
               )) +
    xlim(c(0, max(pl_d$rel_importance)))
  
  pp_f <- ggplotGrob(pp_f)
  pp_s <- ggplotGrob(pp_s)
  
  dif_df <- pl_d %>%
    select(S_or_F, short_descr, rel_importance) %>%
    pivot_wider(id_cols=short_descr, names_from=S_or_F,
                values_from=rel_importance,
                values_fill=0) %>%
    mutate(Difference=round(Faculty-Student, 1),
           col=ifelse(Difference>0,
                      color_gradient(abs(Difference),
                                     c("white", "deepskyblue2")),
                      color_gradient(abs(Difference),
                                     c("white", "coral")))) %>%
    select(Difference, col)
  
  dif_tab <- gridExtra::tableGrob(dif_df$Difference,
                                  rows=NULL, cols="Difference",
                                  theme=gridExtra::ttheme_default(
                                    core=list(bg_params = 
                                                list(fill = dif_df$col,
                                                     col='grey50')),
                                    colhead=list(bg_params = 
                                                   list(col="grey50"),
                                                 fg_params=list(cex=0.9),
                                                 padding=unit.c(unit(1, "mm"),
                                                                unit(1, "mm"))),
                                    base_size = 10,
                                    padding = unit(c(0,0.01),"npc")))
  
  dif_tab$heights <- unit(c(((16+n_vars)/25)/(nrow(dif_tab)+1),
                            rep(((15+n_vars)/26)/(nrow(dif_tab)+1),
                                nrow(dif_tab)-1)), "npc")
  dif_tab$widths <- unit.pmax(dif_tab$widths, unit(1.5, "lines"))
  
  pp_f <- gtable_add_cols(pp_f, sum(dif_tab$widths))
  pp_s <- gtable_add_cols(pp_s, sum(dif_tab$widths)/24,
                          pos=0)
  
  pp_f <- gtable_add_grob(pp_f,
                          grid::rectGrob(gp=grid::gpar(fill="white",
                                                       col = NA)),
                          t=1, b=10, l=10, r=10) 
  
  pp_f <- gtable_add_grob(pp_f, grobs = dif_tab, t=2, b=8, 
                          l=10,
                          r=10)
  
  
  grp <- case_when(
    grp=="ARAS_P"~"ARAS-P",
    grp=="ARAS_S"~"ARAS-S",
    grp=="ARAS_H"~"ARAS-PD",
    T~grp
  )
  
  grand_tit <- paste("Proportional Variable Importance for Distinguishing",
                     grp,
                     "Groups")
  
  g <- arrangeGrob(arrangeGrob(pp_f, 
                           pp_s,
                           ncol=2,
                           widths=c(1.2, 1),
                           heights=unit(1, "null")),
               top=textGrob(grand_tit,
                            gp=gpar(fontsize=16,font=2)))
  
  return(grid.draw(g))
  
  
}

