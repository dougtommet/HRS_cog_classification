
hrshcap <- readRDS(fs::path(r_objects_folder, "005_hrshcap.rds"))

hrshcap <- hrshcap %>%
  mutate(imputed_flag = case_when(vs2memsc_iflag == 0 &
                                    vs2exfsc_iflag == 0 &
                                    vs2lflsc_iflag == 0 &
                                    vs2vissc_iflag == 0 &
                                    vs2vdori1_iflag == 0 &
                                    vs2memimp_iflag == 0 &
                                    vs2exfimp_iflag == 0 &
                                    vs2lflimp_iflag == 0 &
                                    vs2memimp_eap_iflag ==0 &
                                    vs2exfimp_eap_iflag ==0 &
                                    vs2lflimp_eap_iflag ==0 &
                                    vs2visimp_iflag ==0 &
                                    vs2orimp_iflag  ==0 &
                                    vs3jormsc_iflag ==0 &
                                    vs3blessedsc_iflag ==0 ~ 1,
                                  TRUE ~ 0))

saveRDS(hrshcap, fs::path(r_objects_folder, "014_hrshcap.rds"))
