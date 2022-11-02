#check priors
bplot_width = 1
bplot_space = 0.2
palette_Dark2 = brewer.pal(8, "Dark2")
palette_Set1 = brewer.pal(9, "Set1")
palette_Paired = brewer.pal(12, "Paired")

names.ethnic_group = c("Borana", "Rendille", "Samburu", "Turkana")
names.territorial_section = c("Kwatela", "Ngibochoros", "Ngiyapakuno")
names.clan = c("LDUPSAI","LPISIKISHU", "LUKUMAI", "NGIDOCA", "NGIPONGAA",
               "NGISIGER", "NOONITUU", "SAALE", "WARRAJIDAA")
names.subclan = c("Adiyakiche", "Agletim", "Ajaj", "Amoye", "Anna", "Beere",
                  "Bulyare", "Dabalen", "Eisimdele", "Erot", "Gudereiyo", "Harugurayo",
                  "Ilman dhubana", "Jaro", "Kadodo", "Karrayu", "Kura", "Lamayanak", "Letip",
                  "Lkuume", "Loisilale", "Lowagozo", "Miillo", "Mirgichan", "Mursa", "Nurtu", "Oro",
                  "Sakuye", "Soroitare", "Timado", "Uso", "Wambileyio", "Waragu")
names.sex = c("Female", "Male")
names.marital_status = c("Divorced", "Married", "Single", "Unofficially", "Widowed")
names.params = list(ethnic_group = names.ethnic_group,
                    territorial_section = names.territorial_section,
                    clan = names.clan, subclan = names.subclan,
                    sex = names.sex, marital_status = names.marital_status)

#KIDNAP responses by ethnic group

ethnic_group_cols = palette_Dark2[c(1:3,6)]
barplot(prop.table(table(data$KIDNAP, data$ethnic_group), margin = 2)[2,], beside = T,
        width = bplot_width, space = bplot_space, xlim = c(0,5), ylim = c(0,1),
        names.arg = names.ethnic_group, col = ethnic_group_cols)

#KIDNAP responses by clan
#color-coding clans by ethnic group
clan_bool = prop.table(table(data$ethnic_group, data$clan), margin = 2)
clan_cols_ind = as.vector(c(1:length(names.ethnic_group))%*%clan_bool)
clan_cols = ethnic_group_cols[clan_cols_ind]

barplot(prop.table(table(data$KIDNAP, data$clan), margin = 2)[2,], beside = T,
        width = bplot_width-0.1, space = bplot_space, xlim = c(0,10), ylim = c(0,1),
        names.arg = names.clan, col = clan_cols)
legend("topright", legend = names.ethnic_group, fill = ethnic_group_cols)


##KIDNAP responses by clan within each ethnic group
graphics.off()
par(mfrow=c(2,2), mar = c(5,5,2,5), oma = c(0,1,5,1))
for(i in 0:3){
  data_sub = subset(data, subset = data$ethnic_group==i)
  barplot(prop.table(table(data_sub$KIDNAP, data_sub$clan), margin = 2), beside = F,
          width = as.numeric(table(data_sub$ethnic_group, data_sub$clan)/sum(table(data_sub$ethnic_group, data_sub$clan))), 
          space = bplot_space, ylim = c(0,1),
          names.arg = names.clan[sort(unique(data_sub$clan))+1], col = palette_Paired[c(1,2)],
          main = paste0(names.ethnic_group[i+1]))
}
par(mfrow=c(1,1), xpd = TRUE, new = TRUE)

title(main = "KIDNAP responses by clan", outer = TRUE, line = 2.5)
dev.off()

#KIDNAP responses by subclan
#color-coding subclans by ethnic group
subclan_bool = prop.table(table(data$ethnic_group, data$subclan), margin = 2)
subclan_cols_ind = as.vector(c(1:length(names.ethnic_group))%*%subclan_bool)
subclan_cols = ethnic_group_cols[subclan_cols_ind]

barplot(prop.table(table(data$KIDNAP, data$subclan), margin = 2)[2,], beside = F,
        width = bplot_width-0.75, space = bplot_space, xlim = c(0,10), ylim = c(0,1),
        names.arg = c("Turkana", names.subclan), col = subclan_cols)
legend("topright", legend = names.ethnic_group, fill = ethnic_group_cols, outer)

##KIDNAP responses by subclan within each ethnic group

graphics.off()
par(mfrow=c(3,1), mar = c(5,5,2,5), oma = c(0,1,5,1))
for(i in 0:2){
  data_sub = subset(data, subset = data$ethnic_group==i)
  barplot(prop.table(table(data_sub$KIDNAP, data_sub$subclan), margin = 2), beside = F,
          width = as.numeric(table(data_sub$ethnic_group, data_sub$subclan)/sum(table(data_sub$ethnic_group, data_sub$subclan))), 
          space = bplot_space, ylim = c(0,1),
          names.arg = names.subclan[sort(unique(data_sub$subclan))], col = palette_Paired[c(1,2)],
          main = paste0(names.ethnic_group[i+1]))
}
par(mfrow=c(1,1), xpd = TRUE, new = TRUE)

title(main = "KIDNAP responses by subclan", outer = TRUE, line = 2.5)
dev.off()

##KIDNAP responses by territorial section within the Turkana
graphics.off()

data_sub = subset(data, subset = data$ethnic_group==3)
barplot(prop.table(table(data_sub$KIDNAP, data_sub$territorial_section), margin = 2), beside = F,
          width = as.numeric(table(data_sub$ethnic_group, data_sub$territorial_section)/sum(table(data_sub$ethnic_group, data_sub$territorial_section))), 
          space = bplot_space, ylim = c(0,1),
          names.arg = names.territorial_section[sort(unique(data_sub$territorial_section))], col = palette_Paired[c(1,2)],
          main = "KIDNAP responses by territorial section")

dev.off()

##KIDNAP responses by clan within each territorial section (Turkana)
graphics.off()
par(mfrow=c(3,1), mar = c(5,5,2,5), oma = c(0,1,5,1))
for(i in 1:3){
  data_sub = subset(data, subset = data$territorial_section==i)
  barplot(prop.table(table(data_sub$KIDNAP, data_sub$clan), margin = 2), beside = F,
          width = as.numeric(table(data_sub$territorial_section, data_sub$clan)/sum(table(data_sub$territorial_section, data_sub$clan))), 
          space = bplot_space, ylim = c(0,1),
          names.arg = names.clan[sort(unique(data_sub$clan))+1], col = palette_Paired[c(1,2)],
          main = paste0(names.territorial_section[i]))
}
par(mfrow=c(1,1), xpd = TRUE, new = TRUE)

title(main = "KIDNAP responses by clan", outer = TRUE, line = 2.5)
dev.off()

##KIDNAP responses across territorial sections by clan (Turkana)
graphics.off()
par(mfrow=c(3,1), mar = c(5,5,2,5), oma = c(0,1,5,1))
for(i in 3:5){
  data_sub = subset(data, subset = data$clan==i)
  barplot(prop.table(table(data_sub$KIDNAP, data_sub$territorial_section), margin = 2), beside = F,
          width = as.numeric(table(data_sub$clan, data_sub$territorial_section)/sum(table(data_sub$clan, data_sub$territorial_section))), 
          space = bplot_space, ylim = c(0,1),
          names.arg = names.territorial_section[sort(unique(data_sub$territorial_section))], col = palette_Paired[c(1,2)],
          main = paste0(names.clan[i+1]))
}
par(mfrow=c(1,1), xpd = TRUE, new = TRUE)

title(main = "KIDNAP responses across territorial sections by clan", outer = TRUE, line = 2.5)

dev.off()
