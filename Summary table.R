
table(respi$gender)

temp <- respi %>% select(gender, z_fev1, z_fvc, z_fev1_fvc_ratio) %>%
                  filter(z_fvc < 10,z_fev1 < 10, z_fev1_fvc_ratio < 10) %>% 
                  mutate(gender = ifelse(gender == 0,'Female',
                                    ifelse(gender ==1, 'Male',gender)))

table(temp$gender)

df_long <- temp %>% pivot_longer(cols=c('z_fev1', 'z_fvc', 'z_fev1_fvc_ratio'),
                                 names_to='index',
                                 values_to='agg')

dat_count <- df_long %>% 
  group_by(gender) %>% 
  summarise(n = n()) %>%
  ungroup() %>% 
  select(n)

df_temp <- respi %>%
  select(gender, bel_lln_ave_fev1, bel_lln_ave_fvc, bel_lln_ave_fev1_fvc_ratio) %>% 
  pivot_longer(cols=c('bel_lln_ave_fev1', 'bel_lln_ave_fvc', 'bel_lln_ave_fev1_fvc_ratio'),
               names_to='index',
               values_to='agg')%>% 
  mutate(gender = ifelse(gender == 0,'Female',
                         ifelse(gender ==1, 'Male',gender))) %>% 
  group_by(gender, index, agg) %>% 
  summarise(count = n()) %>%
  ungroup() %>% 
  group_by(gender, index) %>% 
  mutate(tot = sum(count)) %>%
  ungroup() %>% 
  mutate(proportion = count/tot * 100) %>% 
  filter(agg == 1) %>%
  select(gender, index, proportion) %>% 
  mutate(index_no = row_number())


dat1 <- df_long %>%
    group_by(gender, index) %>% 
  summarise( `50%` = mean(agg, na.rm = T),
            `5%` = quantile(agg, probs = 0.05, na.rm = T),
            `95%` = quantile(agg, probs = 0.95, na.rm = T),
            .groups = 'drop') %>% 
  ungroup() %>% 
  mutate(index_no = c(1,2,3,4,5,6)) %>% 
  left_join(df_temp %>% select(-index), by =c('gender', 'index_no')) %>% 
  mutate(`Subjects n` = ifelse(gender == 'Female', dat_count$n[1], dat_count$n[2])) %>% 
  as_grouped_data(groups = c('gender', 'Subjects n')) %>% 
          select("gender", "index", 'Subjects n', "50%", "5%", "95%", `%<LLN#` = 'proportion')

dat1[3,1] <- dat1[1,1]
dat1[3,3] <- dat1[2,3]

dat1[8,1] <- dat1[6,1]
dat1[8,3] <- dat1[7,3]

dat2 <- dat1 %>% slice(-c(1,2,6,7)) %>%
        mutate(no = c(4,6,5,1,3,2)) %>%
        arrange(no) %>% select(-no) 

ft_1 <- as_flextable(as.data.frame(dat2), separate_with = 'gender', sep_w = 0) %>% 
        add_footer_lines(values = 'FEV1: forced expiratory volume in 1 s; FVC: forced vital capacity; LLN: lower limit of normal. #: according to GLI-2012.', top = FALSE) %>% 
        add_header_lines("Adolescents aged <20 years")

#autofit(ft_1)

ft_1

save_as_docx(my_table, ft_1, path = "LLN_problem.docx")

file.edit('all plots.Rmd')
