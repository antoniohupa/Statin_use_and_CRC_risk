library(tidyverse)
library(forestplot)


# Table data
table_data <- tibble(mean  = c(rep(NA,3), 0.89, rep(NA,3), 0.83, rep(NA,3), 0.80, rep(NA,3), 0.89, rep(NA,5),
                               0.86, rep(NA,3), 0.86, rep(NA,3), 0.84, rep(NA,2), NA, NA), 
                    lower = c(rep(NA,3), 0.82, rep(NA,3), 0.78, rep(NA,3), 0.74, rep(NA,3), 0.83, rep(NA,5), 
                              0.76, rep(NA,3), 0.79, rep(NA,3), 0.77, rep(NA,2), NA, NA),
                    upper = c(rep(NA,3), 0.96, rep(NA,3), 0.88, rep(NA,3), 0.87, rep(NA,3), 0.94, rep(NA,5),  
                              0.98, rep(NA,3), 0.92, rep(NA,3),  0.92, rep(NA,2), NA, NA),
                    pinteraction = c(rep("",4),  0.17, rep("",7), 0.04, rep("",12), "-", rep("",8)),
                    subgroup = c("", "Women", "", "Any duration", "", "Men", "", "Any duration",
                                 "", "≤70 years", "", "Any duration", "", ">70 years", "", "Any duration",
                                 "", "BMI, in kg/m2", "","<25","", "Any duration", "", "25-30", "", "Any duration", "",
                                 ">30","", "Any duration", "", "Missing", "", "-"),
                    cases = c("", "n=6,376", "", "1,306 (20.5)", "", "n=9,115", "", "2,026 (22.2)", "", "n=7,448",
                              "", "1,257 (16.9)", "", "n=8,043", "", "2,075 (25.8)", "","", "", "n=2,123", "", "423 (19.9)", "",
                              "n=4,933", "", "1,300 (26.4)","","n=3,652","", "1,008 (27.6)", "", "n=4,783","", "601 (12.6)"),
                    controls = c("", "n=24,693", "", "5,115 (20.7)", "", "n=35,307", "", "7,990 (22.6)", "", "n=28,866",
                                 "", "4,981 (17.3)", "", "n=31,134", "", "8,124 (26.1)", "","", "", "n=7,635", "", "1,527 (20.0)", 
                                 "", "n=18,108", "", "4,932 (27.2)", "", "n=13,421", "","3,929 (29.3)", "", "n=20,836", "",
                                 "2,717 (13.0)"),
                    crudeOR = c("","","", "1.01 (0.94-1.08)", "", "", "", "0.97 (0.92-1.03)", "", "", "", "0.98 (0.91-1.05)", "", 
                                "", "", "0.99 (0.94-1.05)", "", "", "", "", "", "1.02 (0.90-1.16)","","","", "0.95 (0.89-1.03)", 
                                "","","", "0.92 (0.85-1.00)", "","","","Imputed"),
                    adjustedOR = c("","","", "0.89 (0.82-0.96)", "", "", "", "0.83 (0.78-0.88)", "", "", "", "0.80 (0.74-0.87)", "", 
                                "", "", "0.89 (0.83-0.94)", "", "", "","","", "0.86 (0.76-0.98)","","","", "0.86 (0.79-0.92)", 
                                "","","","0.84 (0.77-0.92)", "","","", "Imputed"))

header <- tibble(subgroup = c("", ""), 
                 cases = c("Cases (%)","N=15,491"),
                 controls = c("Controls (%)","N=60,000"),
                 crudeOR = c("Non-adjusted", "OR (95%CI)*"),
                 adjustedOR = c("Fully-adjusted","OR (95%CI)†"),
                 pinteraction = c("p-value", "(test for interaction)"))

output_df <- bind_rows(header, table_data)

#tabletext <- cbind(c("", table_data$active_principle), 
#                   c("Cases (%)", table_data$cases), 
#                   c("Controls (%)", table_data$controls),
#                   c("AOR (95%CI)", table_data$OR))


output_df %>% 
  forestplot(labeltext = c(subgroup, cases, controls, crudeOR, adjustedOR, pinteraction),
             graph.pos = 5,
             is.summary = c(rep(TRUE,2), FALSE, TRUE, rep(FALSE,3), TRUE, rep(FALSE,3), TRUE, rep(FALSE,3), TRUE, rep(FALSE,3), TRUE,
                            FALSE, TRUE, rep(FALSE,3), TRUE, rep(FALSE,3), TRUE, rep(FALSE,3), TRUE, rep(FALSE,2)),
           title="",
           xlab="AOR (95%CI)",
           xlog=TRUE,
           
           hrzl_lines=list("3" = gpar(lwd=1, lineend="butt", columns=c(1:7), col="black"),
                           #"7" = gpar(lwd=120, lineend="butt", columns=c(1:7), col="#99999922"),
                           "12" = gpar(lwd=245, lineend="butt", columns=c(1:7), col="#99999922")),
           
           txt_gp=fpTxtGp(label=gpar(cex=0.9),
                          ticks=gpar(cex=0.9, fontface=2),
                          xlab=gpar(cex = 0.9, fontface=2),
                          title=gpar(cex = 0.9)),
           
           col=fpColors(box="black", lines="black", zero = "gray48"), graphwidth = unit(40,"mm"),
           zero=1, cex=1.0, lineheight = "auto", boxsize=0.4, colgap=unit(3.0,"mm"), line.margin = 0.2,
           lwd.ci=1.5, ci.vertices=TRUE, ci.vertices.height = 0.1, align = c("l","c","c","c", "c", "c"),
           xticks = c(0.6,0.8,1,1.2))





