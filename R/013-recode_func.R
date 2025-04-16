

hrs16_func <- readRDS(fs::path(r_objects_folder, "010_hrs16_func.rds"))

hrs16_func <- hrs16_func %>%
  mutate(jorm1 = case_when(PD506==1 & PD507==1 ~ 1,
                           PD506==1 & PD507==2 ~ 2,
                           PD506==2 ~ 3,
                           PD506==3 & PD508==4 ~ 4,
                           PD506==3 & PD508==5 ~ 5),
         jorm2 = case_when(PD509==1 & PD510==1 ~ 1,
                           PD509==1 & PD510==2 ~ 2,
                           PD509==2 ~ 3,
                           PD509==3 & PD511==4 ~ 4,
                           PD509==3 & PD511==5 ~ 5),
         jorm3 = case_when(PD512==1 & PD513==1 ~ 1,
                           PD512==1 & PD513==2 ~ 2,
                           PD512==2 ~ 3,
                           PD512==3 & PD514==4 ~ 4,
                           PD512==3 & PD514==5 ~ 5),
         jorm4 = case_when(PD515==1 & PD516==1 ~ 1,
                           PD515==1 & PD516==2 ~ 2,
                           PD515==2 ~ 3,
                           PD515==3 & PD517==4 ~ 4,
                           PD515==3 & PD517==5 ~ 5),
         jorm5 = case_when(PD518==1 & PD519==1 ~ 1,
                           PD518==1 & PD519==2 ~ 2,
                           PD518==2 ~ 3,
                           PD518==3 & PD520==4 ~ 4,
                           PD518==3 & PD520==5 ~ 5),
         jorm6 = case_when(PD521==1 & PD522==1 ~ 1,
                           PD521==1 & PD522==2 ~ 2,
                           PD521==2 ~ 3,
                           PD521==3 & PD523==4 ~ 4,
                           PD521==3 & PD523==5 ~ 5),
         jorm7 = case_when(PD524==1 & PD525==1 ~ 1,
                           PD524==1 & PD525==2 ~ 2,
                           PD524==2 ~ 3,
                           PD524==3 & PD526==4 ~ 4,
                           PD524==3 & PD526==5 ~ 5),
         jorm8 = case_when(PD527==1 & PD528==1 ~ 1,
                           PD527==1 & PD528==2 ~ 2,
                           PD527==2 ~ 3,
                           PD527==3 & PD529==4 ~ 4,
                           PD527==3 & PD529==5 ~ 5),
         jorm9 = case_when(PD530==1 & PD531==1 ~ 1,
                           PD530==1 & PD531==2 ~ 2,
                           PD530==2 ~ 3,
                           PD530==3 & PD532==4 ~ 4,
                           PD530==3 & PD532==5 ~ 5),
         jorm10 =case_when(PD533==1 & PD534==1 ~ 1,
                           PD533==1 & PD534==2 ~ 2,
                           PD533==2 ~ 3,
                           PD533==3 & PD535==4 ~ 4,
                           PD533==3 & PD535==5 ~ 5),
         jorm11 =case_when(PD536==1 & PD537==1 ~ 1,
                           PD536==1 & PD537==2 ~ 2,
                           PD536==2 ~ 3,
                           PD536==3 & PD538==4 ~ 4,
                           PD536==3 & PD538==5 ~ 5),
         jorm12 =case_when(PD539==1 & PD540==1 ~ 1,
                           PD539==1 & PD540==2 ~ 2,
                           PD539==2 ~ 3,
                           PD539==3 & PD541==4 ~ 4,
                           PD539==3 & PD541==5 ~ 5),
         jorm13 =case_when(PD542==1 & PD543==1 ~ 1,
                           PD542==1 & PD543==2 ~ 2,
                           PD542==2 ~ 3,
                           PD542==3 & PD544==4 ~ 4,
                           PD542==3 & PD544==5 ~ 5),
         jorm14 =case_when(PD545==1 & PD546==1 ~ 1,
                           PD545==1 & PD546==2 ~ 2,
                           PD545==2 ~ 3,
                           PD545==3 & PD547==4 ~ 4,
                           PD545==3 & PD547==5 ~ 5),
         jorm15 =case_when(PD548==1 & PD549==1 ~ 1,
                           PD548==1 & PD549==2 ~ 2,
                           PD548==2 ~ 3,
                           PD548==3 & PD550==4 ~ 4,
                           PD548==3 & PD550==5 ~ 5),
         jorm16 =case_when(PD551==1 & PD552==1 ~ 1,
                           PD551==1 & PD552==2 ~ 2,
                           PD551==2 ~ 3,
                           PD551==3 & PD553==4 ~ 4,
                           PD551==3 & PD553==5 ~ 5)
         )


hrs16_func <- hrs16_func %>%
  rowwise() %>%
  mutate(jorm = mean(c(jorm1, jorm2, jorm3, jorm4, jorm5, jorm6, jorm7, jorm8, jorm9, jorm10, jorm11, jorm12, jorm13, jorm14, jorm15, jorm16), na.rm=TRUE)) %>%
  ungroup()

hrs16_func <- hrs16_func  %>%
  mutate(self_concerns = case_when(PD102==3 ~ 1,
                                   PD102 %in% c(1, 2) ~ 0))

hrs16_func <- hrs16_func %>%
  labelled::set_variable_labels(jorm1 = "Remembering things about family") %>%
  labelled::set_value_labels(jorm1 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm2 = "Remembering things that happened recently") %>%
  labelled::set_value_labels(jorm2 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm3 = "Recalling conversations a few day later") %>%
  labelled::set_value_labels(jorm3 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm4 = "Remembering telephone number") %>%
  labelled::set_value_labels(jorm4 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm5 = "Remembering day and month") %>%
  labelled::set_value_labels(jorm5 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm6 = "Remembering where things are kept") %>%
  labelled::set_value_labels(jorm6 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm7 = "Remembering where to find things") %>%
  labelled::set_value_labels(jorm7 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm8 = "Knowing how to work familar machines around the house") %>%
  labelled::set_value_labels(jorm8 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm9 = "Learning to use a new gadget") %>%
  labelled::set_value_labels(jorm9 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm10 = "Learning new things in general") %>%
  labelled::set_value_labels(jorm10 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm11 = "Following a story in a book or on TV") %>%
  labelled::set_value_labels(jorm11 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm12 = "Making decisions on everyday matters") %>%
  labelled::set_value_labels(jorm12 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm13 = "Handling money for shopping") %>%
  labelled::set_value_labels(jorm13 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm14 = "Handling financial matters") %>%
  labelled::set_value_labels(jorm14 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm15 = "Handling everyday arthimetic problems") %>%
  labelled::set_value_labels(jorm15 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm16 = "Using intelligence to understand what's going on") %>%
  labelled::set_value_labels(jorm16 = c("Much improved" = 1, "A bit improved" = 2, "Not much change" = 3, "A bit worse" = 4, "Much worse" = 5)) %>%
  labelled::set_variable_labels(jorm = "Jorm score (HRS)")  %>%
  labelled::set_variable_labels(self_concerns = "Compared to two years ago, would you say your memory is better now, about the same, or worse now than it was then?") %>%
  labelled::set_value_labels(self_concerns = c("Worse" = 1, "Same/Better" = 0))


saveRDS(hrs16_func, fs::path(r_objects_folder, "013_hrs16_func.rds"))
