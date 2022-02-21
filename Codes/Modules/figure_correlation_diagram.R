


gdata <- reg_data[sim==1, ] %>% 
    .[, c("sim", "aunit_id", "block_id", "X", "Y", "N_tgt", "N2") := NULL]

#make the correlation matrix form the first four columns (5th is species names)
(c <- cor(gdata))  
#now put them alltogether (with melt() to reshape the correlation matrix) 
A <- melt(c) %>% 
    data.table() %>% 
    setnames(c("M1", "M2", "corr")) %>% 
    .[, C1 := as.numeric(factor(M1))] %>% 
    .[, C2 := as.numeric(factor(M2))] %>% 
    data.frame()


#define each triangle of the plot matric and the diagonal (mi.ids)
A.diag <- subset(A, M1 == M2)
A.lower <- subset(A[lower.tri(c),], M1 != M2)
A.upper <- subset(A[upper.tri(c),], M1 != M2)

# now plot just these values, adding labels (geom_text) for the names and th values
meas <- as.character(unique(A$M2))
ggplot() +
    geom_tile(data = A.lower, aes(C1, C2, fill=corr)) +
    # geom_text(data = A.lower, aes(C1, C2, label=paste0(round(corr,3)))) + 
    geom_text(data = A.diag, aes(C1, C2, label=M2), hjust = 1) +
    scale_colour_identity() + 
        scale_fill_gradientn(colours= c("red", "white", "blue")) +
        scale_x_discrete(limits=meas[length(meas):1]) + #flip the x axis 
        scale_y_discrete(limits=meas) +
    xlab(NULL) + ylab(NULL) + 
    theme_void()
ggsave(file = here('GitControlled/Graphs/corr_diag.png'),
       height=6.5,width=6.5)
