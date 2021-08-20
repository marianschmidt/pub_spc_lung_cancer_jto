# project:      SPN - Data Analysis
# script:       01.x2.fn_analysis.helper.functions.R
# author:       Marian Eberl
# recent date:  2020-09-21
# versions:     2020-09-21 first version
# req packages: tidyverse
# content:		  create helper functions for 01.an_analyses.spc.lung.Rmd 
# used in:
# dependend on:
# comments:
# execution time:
# quality check: (version 2017-07-28, YYYY-MM-DD, Name Programmer)
#----------------------------------------------------------------------------


#---------- Define functions -----------------------------

#cr_site_vars_smoke: Create multiple new variable that recode site var into groups and smoke-related sites)

cr_site_vars_smoke <- function(results_df, site_var, en_gb = FALSE){
  site_var <- rlang::ensym(site_var)
  
  icdgroup_c15_code    <- if(en_gb){"Oesophagus (C15)"}else{"Esophagus (C15)"}
  icdgroup_c9195_code  <- if(en_gb){"Leukaemias (C91-C95)"}else{"Leukemias (C91-C95)"}
  icdgroup_c9195s_code <- if(en_gb){"Non-Myeloid Leukaemias (C91, C93-C95)"}else{"Non-Myeloid Leukemias (C91, C93-C95)"}
  icdgroup_c92_code    <- if(en_gb){"Myeloid leukaemia (C92)"}else{"Myeloid leukemia (C92)"}
  icdgroup_c96_code    <- if(en_gb){"Other lymphoid, haematopoietic tissue (C96)"}else{"Other lymphoid, hematopoietic tissue (C96)"}
  
  results_df %>%
    #var t_icdgroup -> grouping used by German Cancer registry
    dplyr::mutate(t_icdgroup = recode_factor(!!site_var,
                                             `C00`="Lip, oral cavity and pharynx (C00-C14)", 
                                             `C01`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C02`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C03`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C04`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C05`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C06`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C07`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C08`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C09`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C10`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C11`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C12`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C13`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C14`="Lip, oral cavity and pharynx (C00-C14)",
                                             `C15`=icdgroup_c15_code,                     
                                             `C16`="Stomach (C16)",
                                             `C17`="Small intestine (C17)",
                                             `C18`="Colorectum (C18-C20)",
                                             `C19`="Colorectum (C18-C20)",
                                             `C20`="Colorectum (C18-C20)",
                                             `C21`="Anus (C21)",
                                             `C22`="Liver (C22)",
                                             `C23`="Gallbladder (C23-C24)",
                                             `C24`="Gallbladder (C23-C24)",
                                             `C25`="Pancreas (C25)",
                                             `C26`="Other digestive organs (C26)",
                                             `C30`="Nasal cavity, middle ear (C30)",
                                             `C31`="Accessory sinuses (C31)",
                                             `C32`="Larynx (C32)",
                                             `C33`="Lung (C33-C34)",
                                             `C34`="Lung (C33-C34)",
                                             `C37`="Thymus (C37)",
                                             `C38`="Heart, mediastinum, pleura (C38)",
                                             `C39`="Other respiratory system, inthrathoracic organs (C39)",
                                             `C40`="Bone, articular cartilage (C40-C41)",
                                             `C41`="Bone, articular cartilage (C40-C41)",
                                             `C43`="Skin (C43)",
                                             `C44`="Non-melanoma skin (C44)",
                                             `C45`="Mesothelial, soft tissue (C45-C49)",
                                             `C46`="Mesothelial, soft tissue (C45-C49)",
                                             `C47`="Mesothelial, soft tissue (C45-C49)",
                                             `C48`="Mesothelial, soft tissue (C45-C49)",
                                             `C49`="Mesothelial, soft tissue (C45-C49)",
                                             `C50`="Breast (C50)",
                                             `C51`="Vulva (C51)",
                                             `C52`="Vagina (C52)",
                                             `C53`="Cervix (C53)",
                                             `C54`="Uterus (C54-C55)",
                                             `C55`="Uterus (C54-C55)",
                                             `C56`="Ovary (C56)",
                                             `C57`="Other female genital organs (C57)",
                                             `C58`="Placenta (C58)",
                                             `C60`="Penis (C60)",
                                             `C61`="Prostate (C61)",
                                             `C62`="Testis (C62)",
                                             `C63`="Other male genital organs (C63)",
                                             `C64`="Kidney (C64)",
                                             `C65`="Urinary tract (C65-C68)",
                                             `C66`="Urinary tract (C65-C68)",
                                             `C67`="Urinary tract (C65-C68)",
                                             `C68`="Urinary tract (C65-C68)",
                                             `C69`="Eye (C69)",
                                             `C70`="Brain, central nervous system (C70-C72)",
                                             `C71`="Brain, central nervous system (C70-C72)",
                                             `C72`="Brain, central nervous system (C70-C72)",
                                             `C73`="Thyroid, endocrine glands (C73-C75)",
                                             `C74`="Thyroid, endocrine glands (C73-C75)",
                                             `C75`="Thyroid, endocrine glands (C73-C75)",
                                             `C76`="Ill-defined sites (C76-C80)",
                                             `C77`="Ill-defined sites (C76-C80)",
                                             `C78`="Ill-defined sites (C76-C80)",
                                             `C79`="Ill-defined sites (C76-C80)",
                                             `C80`="Ill-defined sites (C76-C80)",
                                             `C81`="Hodgkin lymphoma (C81)",
                                             `C82`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C83`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C84`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C85`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C86`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C87`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C88`="Non-Hodgkin lymphoma (C82-C88)",
                                             `C90`="Multiple myeloma (C90)",
                                             `C91`=icdgroup_c9195_code,
                                             `C92`=icdgroup_c9195_code,
                                             `C93`=icdgroup_c9195_code,
                                             `C94`=icdgroup_c9195_code,
                                             `C95`=icdgroup_c9195_code,
                                             `C96`=icdgroup_c96_code
    ))%>%
    sjlabelled::var_labels(t_icdgroup="Tumor group (ICD-10GM based grouping)") %>%
    #   
    #var t_icdgroup_pub -> grouping used in publication
    dplyr::mutate(t_icdgroup_pub = recode_factor(!!site_var,
                                                 `C00`="Lip (C00)",  
                                                 `C01`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C02`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C03`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C04`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C05`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C06`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C07`="Salivary glands (C07-C08)",
                                                 `C08`="Salivary glands (C07-C08)",
                                                 `C09`="Tonsil (C09)",
                                                 `C10`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C11`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C12`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C13`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C14`="Oral cavity, pharynx (C01-C06,C10-C14)",
                                                 `C15`=icdgroup_c15_code,                     
                                                 `C16`="Stomach (C16)",
                                                 `C17`="Small intestine (C17)",
                                                 `C18`="Colorectum (C18-C20)",
                                                 `C19`="Colorectum (C18-C20)",
                                                 `C20`="Colorectum (C18-C20)",
                                                 `C21`="Anus (C21)",
                                                 `C22`="Liver (C22)",
                                                 `C23`="Gallbladder (C23-C24)",
                                                 `C24`="Gallbladder (C23-C24)",
                                                 `C25`="Pancreas (C25)",
                                                 `C26`="Other digestive organs (C26)",
                                                 `C30`="Nasal cavity, middle ear (C30)",
                                                 `C31`="Accessory sinuses (C31)",
                                                 `C32`="Larynx (C32)",
                                                 `C33`="Lung (C33-C34)",
                                                 `C34`="Lung (C33-C34)",
                                                 `C37`="Thymus (C37)",
                                                 `C38`="Heart, mediastinum, pleura (C38)",
                                                 `C39`="Other respiratory system, inthrathoracic organs (C39)",
                                                 `C40`="Bone, articular cartilage (C40-C41)",
                                                 `C41`="Bone, articular cartilage (C40-C41)",
                                                 `C43`="Skin melanoma (C43)",
                                                 `C44`="Non-melanoma skin (C44)",
                                                 `C45`="Mesothelial, soft tissue (C45-C49)",
                                                 `C46`="Mesothelial, soft tissue (C45-C49)",
                                                 `C47`="Mesothelial, soft tissue (C45-C49)",
                                                 `C48`="Mesothelial, soft tissue (C45-C49)",
                                                 `C49`="Mesothelial, soft tissue (C45-C49)",
                                                 `C50`="Breast (C50)",
                                                 `C51`="Vulva (C51)",
                                                 `C52`="Vagina (C52)",
                                                 `C53`="Cervix (C53)",
                                                 `C54`="Uterus (C54-C55)",
                                                 `C55`="Uterus (C54-C55)",
                                                 `C56`="Ovary (C56)",
                                                 `C57`="Other female genital organs (C57)",
                                                 `C58`="Placenta (C58)",
                                                 `C60`="Penis (C60)",
                                                 `C61`="Prostate (C61)",
                                                 `C62`="Testis (C62)",
                                                 `C63`="Other male genital organs (C63)",
                                                 `C64`="Kidney (C64)",
                                                 `C65`="Urinary tract (C65-C68)",
                                                 `C66`="Urinary tract (C65-C68)",
                                                 `C67`="Urinary tract (C65-C68)",
                                                 `C68`="Urinary tract (C65-C68)",
                                                 `C69`="Eye (C69)",
                                                 `C70`="Brain, central nervous system (C70-C72)",
                                                 `C71`="Brain, central nervous system (C70-C72)",
                                                 `C72`="Brain, central nervous system (C70-C72)",
                                                 `C73`="Thyroid, endocrine glands (C73-C75)",
                                                 `C74`="Thyroid, endocrine glands (C73-C75)",
                                                 `C75`="Thyroid, endocrine glands (C73-C75)",
                                                 `C76`="Ill-defined sites (C76-C80)",
                                                 `C77`="Ill-defined sites (C76-C80)",
                                                 `C78`="Ill-defined sites (C76-C80)",
                                                 `C79`="Ill-defined sites (C76-C80)",
                                                 `C80`="Ill-defined sites (C76-C80)",
                                                 `C81`="Hodgkin lymphoma (C81)",
                                                 `C82`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C83`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C84`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C85`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C86`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C87`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C88`="Non-Hodgkin lymphoma (C82-C88)",
                                                 `C90`="Multiple myeloma (C90)",
                                                 `C91`=icdgroup_c9195s_code,
                                                 `C92`=icdgroup_c92_code,
                                                 `C93`=icdgroup_c9195s_code,
                                                 `C94`=icdgroup_c9195s_code,
                                                 `C95`=icdgroup_c9195s_code,
                                                 `C96`=icdgroup_c96_code
    ))%>%
    sjlabelled::var_labels(t_icdgroup_pub="Tumor group (ICD-10GM based grouping)") %>%
    #    
    #var t_icdgroup_pub -> grouping used in publication detailed
    dplyr::mutate(t_icdgroup_pubdet = recode_factor(!!site_var,
                                                    `C00`="Lip (C00)",  
                                                    `C01`="Oral cavity (C01-C06)",
                                                    `C02`="Oral cavity (C01-C06)",
                                                    `C03`="Oral cavity (C01-C06)",
                                                    `C04`="Oral cavity (C01-C06)",
                                                    `C05`="Oral cavity (C01-C06)",
                                                    `C06`="Oral cavity (C01-C06)",
                                                    `C07`="Salivary glands (C07-C08)",
                                                    `C08`="Salivary glands (C07-C08)",
                                                    `C09`="Tonsil (C09)",
                                                    `C10`="Pharynx (C10-C14)",
                                                    `C11`="Pharynx (C10-C14)",
                                                    `C12`="Pharynx (C10-C14)",
                                                    `C13`="Pharynx (C10-C14)",
                                                    `C14`="Pharynx (C10-C14)",
                                                    `C15`=icdgroup_c15_code,                     
                                                    `C16`="Stomach (C16)",
                                                    `C17`="Small intestine (C17)",
                                                    `C18`="Colorectum (C18-C20)",
                                                    `C19`="Colorectum (C18-C20)",
                                                    `C20`="Colorectum (C18-C20)",
                                                    `C21`="Anus (C21)",
                                                    `C22`="Liver (C22)",
                                                    `C23`="Gallbladder (C23-C24)",
                                                    `C24`="Gallbladder (C23-C24)",
                                                    `C25`="Pancreas (C25)",
                                                    `C26`="Other digestive organs (C26)",
                                                    `C30`="Nasal cavity, middle ear, accessory sinuses (C30-C31)",
                                                    `C31`="Nasal cavity, middle ear, accessory sinuses (C30-C31)",
                                                    `C32`="Larynx (C32)",
                                                    `C33`="Lung (C33-C34)",
                                                    `C34`="Lung (C33-C34)",
                                                    `C37`="Thymus (C37)",
                                                    `C38`="Heart, mediastinum, pleura (C38)",
                                                    `C39`="Other respiratory system, inthrathoracic organs (C39)",
                                                    `C40`="Bone, articular cartilage (C40-C41)",
                                                    `C41`="Bone, articular cartilage (C40-C41)",
                                                    `C43`="Skin melanoma (C43)",
                                                    `C44`="Non-melanoma skin (C44)",
                                                    `C45`="Mesothelial, soft tissue (C45-C49)",
                                                    `C46`="Mesothelial, soft tissue (C45-C49)",
                                                    `C47`="Mesothelial, soft tissue (C45-C49)",
                                                    `C48`="Mesothelial, soft tissue (C45-C49)",
                                                    `C49`="Mesothelial, soft tissue (C45-C49)",
                                                    `C50`="Breast (C50)",
                                                    `C51`="Vulva (C51)",
                                                    `C52`="Vagina (C52)",
                                                    `C53`="Cervix (C53)",
                                                    `C54`="Uterus (C54-C55)",
                                                    `C55`="Uterus (C54-C55)",
                                                    `C56`="Ovary (C56)",
                                                    `C57`="Other female genital organs (C57)",
                                                    `C58`="Placenta (C58)",
                                                    `C60`="Penis (C60)",
                                                    `C61`="Prostate (C61)",
                                                    `C62`="Testis (C62)",
                                                    `C63`="Other male genital organs (C63)",
                                                    `C64`="Kidney (C64)",
                                                    `C65`="Urinary tract (C65-C68)",
                                                    `C66`="Urinary tract (C65-C68)",
                                                    `C67`="Urinary tract (C65-C68)",
                                                    `C68`="Urinary tract (C65-C68)",
                                                    `C69`="Eye (C69)",
                                                    `C70`="Brain, central nervous system (C70-C72)",
                                                    `C71`="Brain, central nervous system (C70-C72)",
                                                    `C72`="Brain, central nervous system (C70-C72)",
                                                    `C73`="Thyroid, endocrine glands (C73-C75)",
                                                    `C74`="Thyroid, endocrine glands (C73-C75)",
                                                    `C75`="Thyroid, endocrine glands (C73-C75)",
                                                    `C76`="Ill-defined sites (C76-C80)",
                                                    `C77`="Ill-defined sites (C76-C80)",
                                                    `C78`="Ill-defined sites (C76-C80)",
                                                    `C79`="Ill-defined sites (C76-C80)",
                                                    `C80`="Ill-defined sites (C76-C80)",
                                                    `C81`="Hodgkin lymphoma (C81)",
                                                    `C82`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C83`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C84`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C85`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C86`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C87`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C88`="Non-Hodgkin lymphoma (C82-C88)",
                                                    `C90`="Multiple myeloma (C90)",
                                                    `C91`=icdgroup_c9195s_code,
                                                    `C92`=icdgroup_c92_code,
                                                    `C93`=icdgroup_c9195s_code,
                                                    `C94`=icdgroup_c9195s_code,
                                                    `C95`=icdgroup_c9195s_code,
                                                    `C96`=icdgroup_c96_code
    ))%>%
    sjlabelled::var_labels(t_icdgroup_pubdet="Tumor group (ICD-10GM based grouping)") %>%
    #    
    #var t_icdgroup_s -> grouping of SPC - separated by smoking-relatedness
    dplyr::mutate(t_icdgroup_s = recode_factor(!!site_var,
                                               `C00`="Lip (C00)",  
                                               `C01`="Oral cavity (C01-C06)",
                                               `C02`="Oral cavity (C01-C06)",
                                               `C03`="Oral cavity (C01-C06)",
                                               `C04`="Oral cavity (C01-C06)",
                                               `C05`="Oral cavity (C01-C06)",
                                               `C06`="Oral cavity (C01-C06)",
                                               `C07`="Salivary glands (C07-C08)",
                                               `C08`="Salivary glands (C07-C08)",
                                               `C09`="Tonsil (C09)",
                                               `C10`="Pharynx (C10-C14)",
                                               `C11`="Pharynx (C10-C14)",
                                               `C12`="Pharynx (C10-C14)",
                                               `C13`="Pharynx (C10-C14)",
                                               `C14`="Pharynx (C10-C14)",
                                               `C15`=icdgroup_c15_code,                     
                                               `C16`="Stomach (C16)",
                                               `C17`="Small intestine (C17)",
                                               `C18`="Colorectum (C18-C20)",
                                               `C19`="Colorectum (C18-C20)",
                                               `C20`="Colorectum (C18-C20)",
                                               `C21`="Anus (C21)",
                                               `C22`="Liver (C22)",
                                               `C23`="Gallbladder (C23-C24)",
                                               `C24`="Gallbladder (C23-C24)",
                                               `C25`="Pancreas (C25)",
                                               `C26`="Other digestive organs (C26)",
                                               `C30`="Nasal cavity, middle ear (C30)",
                                               `C31`="Accessory sinuses (C31)",
                                               `C32`="Larynx (C32)",
                                               `C33`="Lung (C33-C34)",
                                               `C34`="Lung (C33-C34)",
                                               `C37`="Thymus (C37)",
                                               `C38`="Heart, mediastinum, pleura (C38)",
                                               `C39`="Other respiratory system, inthrathoracic organs (C39)",
                                               `C40`="Bone, articular cartilage (C40-C41)",
                                               `C41`="Bone, articular cartilage (C40-C41)",
                                               `C43`="Skin (C43)",
                                               `C44`="Non-melanoma skin (C44)",
                                               `C45`="Mesothelial, soft tissue (C45-C49)",
                                               `C46`="Mesothelial, soft tissue (C45-C49)",
                                               `C47`="Mesothelial, soft tissue (C45-C49)",
                                               `C48`="Mesothelial, soft tissue (C45-C49)",
                                               `C49`="Mesothelial, soft tissue (C45-C49)",
                                               `C50`="Breast (C50)",
                                               `C51`="Vulva (C51)",
                                               `C52`="Vagina (C52)",
                                               `C53`="Cervix (C53)",
                                               `C54`="Uterus (C54-C55)",
                                               `C55`="Uterus (C54-C55)",
                                               `C56`="Ovary (C56)",
                                               `C57`="Other female genital organs (C57)",
                                               `C58`="Placenta (C58)",
                                               `C60`="Penis (C60)",
                                               `C61`="Prostate (C61)",
                                               `C62`="Testis (C62)",
                                               `C63`="Other male genital organs (C63)",
                                               `C64`="Kidney (C64)",
                                               `C65`="Renal pelvis (C65)",
                                               `C66`="Ureter (C66)",
                                               `C67`="Urinary bladder (C67)",
                                               `C68`="Other and unspecified urinary organs (C68)",
                                               `C69`="Eye (C69)",
                                               `C70`="Brain, central nervous system (C70-C72)",
                                               `C71`="Brain, central nervous system (C70-C72)",
                                               `C72`="Brain, central nervous system (C70-C72)",
                                               `C73`="Thyroid, endocrine glands (C73-C75)",
                                               `C74`="Thyroid, endocrine glands (C73-C75)",
                                               `C75`="Thyroid, endocrine glands (C73-C75)",
                                               `C76`="Ill-defined sites (C76-C80)",
                                               `C77`="Ill-defined sites (C76-C80)",
                                               `C78`="Ill-defined sites (C76-C80)",
                                               `C79`="Ill-defined sites (C76-C80)",
                                               `C80`="Ill-defined sites (C76-C80)",
                                               `C81`="Hodgkin lymphoma (C81)",
                                               `C82`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C83`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C84`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C85`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C86`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C87`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C88`="Non-Hodgkin lymphoma (C82-C88)",
                                               `C90`="Multiple myeloma (C90)",
                                               `C91`=icdgroup_c9195s_code,
                                               `C92`=icdgroup_c92_code,
                                               `C93`=icdgroup_c9195s_code,
                                               `C94`=icdgroup_c9195s_code,
                                               `C95`=icdgroup_c9195s_code,
                                               `C96`=icdgroup_c96_code
    ))%>%
    sjlabelled::var_labels(t_icdgroup_s="Tumor group (ICD-10GM based grouping)") %>%
    #var t_icdgroup_sdet -> grouping of SPC - separated by smoking-relatedness; details on C00-C14
    dplyr::mutate(t_icdgroup_sdet = recode_factor(!!site_var,
                                                  `C00`="Lip (C00)",  
                                                  `C01`="Oral cavity (C01-C06)",
                                                  `C02`="Oral cavity (C01-C06)",
                                                  `C03`="Oral cavity (C01-C06)",
                                                  `C04`="Oral cavity (C01-C06)",
                                                  `C05`="Oral cavity (C01-C06)",
                                                  `C06`="Oral cavity (C01-C06)",
                                                  `C07`="Salivary glands (C07-C08)",
                                                  `C08`="Salivary glands (C07-C08)",
                                                  `C09`="Tonsil (C09)",
                                                  `C10`="Pharynx (C10-C14)",
                                                  `C11`="Pharynx (C10-C14)",
                                                  `C12`="Pharynx (C10-C14)",
                                                  `C13`="Pharynx (C10-C14)",
                                                  `C14`="Pharynx (C10-C14)",
                                                  `C15`=icdgroup_c15_code,                     
                                                  `C16`="Stomach (C16)",
                                                  `C17`="Small intestine (C17)",
                                                  `C18`="Colorectum (C18-C20)",
                                                  `C19`="Colorectum (C18-C20)",
                                                  `C20`="Colorectum (C18-C20)",
                                                  `C21`="Anus (C21)",
                                                  `C22`="Liver (C22)",
                                                  `C23`="Gallbladder (C23-C24)",
                                                  `C24`="Gallbladder (C23-C24)",
                                                  `C25`="Pancreas (C25)",
                                                  `C26`="Other digestive organs (C26)",
                                                  `C30`="Nasal cavity, middle ear (C30)",
                                                  `C31`="Accessory sinuses (C31)",
                                                  `C32`="Larynx (C32)",
                                                  `C33`="Lung (C33-C34)",
                                                  `C34`="Lung (C33-C34)",
                                                  `C37`="Thymus (C37)",
                                                  `C38`="Heart, mediastinum, pleura (C38)",
                                                  `C39`="Other respiratory system, inthrathoracic organs (C39)",
                                                  `C40`="Bone, articular cartilage (C40-C41)",
                                                  `C41`="Bone, articular cartilage (C40-C41)",
                                                  `C43`="Skin (C43)",
                                                  `C44`="Non-melanoma skin (C44)",
                                                  `C45`="Mesothelial, soft tissue (C45-C49)",
                                                  `C46`="Mesothelial, soft tissue (C45-C49)",
                                                  `C47`="Mesothelial, soft tissue (C45-C49)",
                                                  `C48`="Mesothelial, soft tissue (C45-C49)",
                                                  `C49`="Mesothelial, soft tissue (C45-C49)",
                                                  `C50`="Breast (C50)",
                                                  `C51`="Vulva (C51)",
                                                  `C52`="Vagina (C52)",
                                                  `C53`="Cervix (C53)",
                                                  `C54`="Uterus (C54-C55)",
                                                  `C55`="Uterus (C54-C55)",
                                                  `C56`="Ovary (C56)",
                                                  `C57`="Other female genital organs (C57)",
                                                  `C58`="Placenta (C58)",
                                                  `C60`="Penis (C60)",
                                                  `C61`="Prostate (C61)",
                                                  `C62`="Testis (C62)",
                                                  `C63`="Other male genital organs (C63)",
                                                  `C64`="Kidney (C64)",
                                                  `C65`="Renal pelvis (C65)",
                                                  `C66`="Ureter (C66)",
                                                  `C67`="Urinary bladder (C67)",
                                                  `C68`="Other and unspecified urinary organs (C68)",
                                                  `C69`="Eye (C69)",
                                                  `C70`="Brain, central nervous system (C70-C72)",
                                                  `C71`="Brain, central nervous system (C70-C72)",
                                                  `C72`="Brain, central nervous system (C70-C72)",
                                                  `C73`="Thyroid, endocrine glands (C73-C75)",
                                                  `C74`="Thyroid, endocrine glands (C73-C75)",
                                                  `C75`="Thyroid, endocrine glands (C73-C75)",
                                                  `C76`="Ill-defined sites (C76-C80)",
                                                  `C77`="Ill-defined sites (C76-C80)",
                                                  `C78`="Ill-defined sites (C76-C80)",
                                                  `C79`="Ill-defined sites (C76-C80)",
                                                  `C80`="Ill-defined sites (C76-C80)",
                                                  `C81`="Hodgkin lymphoma (C81)",
                                                  `C82`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C83`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C84`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C85`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C86`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C87`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C88`="Non-Hodgkin lymphoma (C82-C88)",
                                                  `C90`="Multiple myeloma (C90)",
                                                  `C91`=icdgroup_c9195s_code,
                                                  `C92`=icdgroup_c92_code,
                                                  `C93`=icdgroup_c9195s_code,
                                                  `C94`=icdgroup_c9195s_code,
                                                  `C95`=icdgroup_c9195s_code,
                                                  `C96`=icdgroup_c96_code
    ))%>%
    sjlabelled::var_labels(t_icdgroup_sdet="Tumor group detailed (ICD-10GM based grouping)") %>%
    #second cancer smoking related according to @secretanReviewHumanCarcinogens2009
    #* asterisk highlights sites where differentiation on an ICD-O-3 level is not possible and all diagnoses under this code have been defined as smoking-related.
    #Source: Secretan B, Straif K, Baan R, Grosse Y, Ghissassi FE, Bouvard V, et al. A review of human carcinogens—Part E: tobacco, areca nut, alcohol, coal smoke, and salted fish. The Lancet Oncology [Internet]. 2009 Nov 1 [cited 2019 Aug 19];10(11):1033–4. Available from: https://www.thelancet.com/journals/lanonc/article/PIIS1470-2045(09)70326-2/abstract
    dplyr::mutate(t_smokeiarc = case_when(
      !!site_var == "C00" ~ 0, #Lip
      !!site_var == "C01" ~ 1, #Oral cavity
      !!site_var == "C02" ~ 1, #Oral cavity
      !!site_var == "C03" ~ 1, #Oral cavity
      !!site_var == "C04" ~ 1, #Oral cavity
      !!site_var == "C05" ~ 1, #Oral cavity
      !!site_var == "C06" ~ 1, #Oral cavity
      !!site_var == "C07" ~ 0, #Salivary glands
      !!site_var == "C08" ~ 0, #Salivary glands
      !!site_var == "C09" ~ 0, #Tonsil
      !!site_var == "C10" ~ 1, #oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C11" ~ 1, #oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C12" ~ 1, #oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C13" ~ 1, #oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C14" ~ 1, #oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C15" ~ 1, #oesophagus
      !!site_var == "C16" ~ 1, #stomach
      !!site_var == "C18" ~ 1, #colorectum
      !!site_var == "C19" ~ 1, #colorectum
      !!site_var == "C20" ~ 1, #colorectum
      !!site_var == "C22" ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
      !!site_var == "C25" ~ 1, #pancreas
      !!site_var == "C30" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
      !!site_var == "C31" ~ 1, #paranasal sinuses
      !!site_var == "C32" ~ 1, #larynx
      !!site_var == "C33" ~ 1, #lung
      !!site_var == "C34" ~ 1, #lung
      !!site_var == "C53" ~ 1, #uterine cervix
      !!site_var == "C56" ~ 1, #ovary (mucinous)
      !!site_var == "C64" ~ 1, #kidney (body and pelvis)
      !!site_var == "C65" ~ 1, #kidney (body and pelvis)
      !!site_var == "C66" ~ 1, #urinary bladder, ureter
      !!site_var == "C67" ~ 1, #urinary bladder, ureter
      !!site_var == "C68" ~ 1, #other urinary
      !!site_var == "C92" ~ 1, #bone marrow (myeloid leukaemia)
      !is.na(!!site_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smokeiarc="Smoking-related Cancer type (IARC definition)") %>%
    sjlabelled::set_labels(t_smokeiarc, labels = c("other cancer" = 0,
                                                   "smoking-related IARC" = 1)) %>%
    dplyr::mutate(dplyr::across(.cols = t_smokeiarc, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    #second cancer smoking related according to @barclayIncidenceSecondHigher2019
    #ICD-10-based definition; ** denotes additional restriction to morphology codes by the orginal authors which could not be applied due to missing morphology data
    #Source: Barclay ME, Lyratzopoulos G, Walter FM, Jefferies S, Peake MD, Rintoul RC. Incidence of second and higher order smoking-related primary cancers following lung cancer: a population-based cohort study. Thorax [Internet]. 2019 [cited 2019 Apr 4];74:466–72. Available from: https://thorax.bmj.com/content/early/2019/02/17/thoraxjnl-2018-212456
    dplyr::mutate(t_smoke_barclay = case_when(
      !!site_var == "C00" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C01" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C02" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C03" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C04" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C05" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C06" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C07" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C08" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C09" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C10" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C11" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C12" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C13" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C14" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C15" ~ 1, #oesophagus**
      !!site_var == "C31" ~ 1, #paranasal sinuses
      !!site_var == "C32" ~ 1, #larynx
      !!site_var == "C33" ~ 1, #lung
      !!site_var == "C34" ~ 1, #lung
      !!site_var == "C67" ~ 1, #urinary bladder
      !is.na(!!site_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_barclay="Smoking-related Cancer type (Barclay definition)") %>%
    sjlabelled::set_labels(t_smoke_barclay, labels = c("other cancer" = 0,
                                                       "smoking-related cancer (Barclay)" = 1))%>%
    dplyr::mutate(dplyr::across(.cols = t_smoke_barclay, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    #second cancer smoking related according to \cite{boakyeTrendsRiskBurden2019}
    #ICD-10-based definition; top 10 smoking-related cancers 
    #Source: Boakye EA, Buchanan P, Hinyard L, Osazuwa‐Peters N, Simpson MC, Schootman M, et al. Trends in the risk and burden of second primary malignancy among survivors of smoking-related cancers in the United States. International Journal of Cancer. 2019;145(1):143–53. 
    dplyr::mutate(t_smoke_boakye = case_when(
      !!site_var == "C00" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C01" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C02" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C03" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C04" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C05" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C06" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C07" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C08" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C09" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C10" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C11" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C12" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C13" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C14" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!site_var == "C15" ~ 1, #oesophagus
      !!site_var == "C18" ~ 1, #colorectum  --- Figure 1 also shows small intestine, but main text and supplement file do not mention C17
      !!site_var == "C19" ~ 1, #colorectum
      !!site_var == "C20" ~ 1, #colorectum
      !!site_var == "C22" ~ 1, #liver
      !!site_var == "C25" ~ 1, #pancreas
      !!site_var == "C30" ~ 1, #nasal cavity and middle ear
      !!site_var == "C31" ~ 1, #paranasal sinuses
      !!site_var == "C32" ~ 1, #larynx
      !!site_var == "C33" ~ 1, #lung
      !!site_var == "C34" ~ 1, #lung
      !!site_var == "C53" ~ 1, #uterine cervix
      !!site_var == "C64" ~ 1, #kidney --- Boakye et al. mention kidney & renal pelvis, but supplement file shows C64.9 for kidney
      !!site_var == "C67" ~ 1, #urinary bladder
      !is.na(!!site_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_boakye="Smoking-related Cancer type (Boakye definition)") %>%
    sjlabelled::set_labels(t_smoke_boakye, labels = c("other cancer" = 0,
                                                      "smoking-related cancer (Boakye)" = 1))%>%
    dplyr::mutate(dplyr::across(.cols = t_smoke_boakye, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE)))  %>%
    {if (tidytable::is_tidytable(results_df)){.} else{tibble::as_tibble(.)}}
  
}

#cr_site_vars_smoke: Create multiple new variable that recode site var into groups and smoke-related sites)

cr_site_vars_smoke_tt <- function(results_df, site_var, en_gb = FALSE){
  site_var <- rlang::ensym(site_var)
  
  icdgroup_c15_code    <- if(en_gb){"Oesophagus (C15)"}else{"Esophagus (C15)"}
  icdgroup_c9195_code  <- if(en_gb){"Leukaemias (C91-C95)"}else{"Leukemias (C91-C95)"}
  icdgroup_c9195s_code <- if(en_gb){"Non-Myeloid Leukaemias (C91, C93-C95)"}else{"Non-Myeloid Leukemias (C91, C93-C95)"}
  icdgroup_c92_code    <- if(en_gb){"Myeloid leukaemia (C92)"}else{"Myeloid leukemia (C92)"}
  icdgroup_c96_code    <- if(en_gb){"Other lymphoid, haematopoietic tissue (C96)"}else{"Other lymphoid, hematopoietic tissue (C96)"}
  
  results_df %>%
    #var t_icdgroup -> grouping used by German Cancer registry
    tidytable::mutate.(
      t_icdgroup = tidytable::case_when.(
        !!site_var == "C00" ~ "Lip, oral cavity and pharynx (C00-C14)", 
        !!site_var == "C01" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C02" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C03" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C04" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C05" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C06" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C07" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C08" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C09" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C10" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C11" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C12" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C13" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C14" ~ "Lip, oral cavity and pharynx (C00-C14)",
        !!site_var == "C15" ~ icdgroup_c15_code,                     
        !!site_var == "C16" ~ "Stomach (C16)",
        !!site_var == "C17" ~ "Small intestine (C17)",
        !!site_var == "C18" ~ "Colorectum (C18-C20)",
        !!site_var == "C19" ~ "Colorectum (C18-C20)",
        !!site_var == "C20" ~ "Colorectum (C18-C20)",
        !!site_var == "C21" ~ "Anus (C21)",
        !!site_var == "C22" ~ "Liver (C22)",
        !!site_var == "C23" ~ "Gallbladder (C23-C24)",
        !!site_var == "C24" ~ "Gallbladder (C23-C24)",
        !!site_var == "C25" ~ "Pancreas (C25)",
        !!site_var == "C26" ~ "Other digestive organs (C26)",
        !!site_var == "C30" ~ "Nasal cavity, middle ear (C30)",
        !!site_var == "C31" ~ "Accessory sinuses (C31)",
        !!site_var == "C32" ~ "Larynx (C32)",
        !!site_var == "C33" ~ "Lung (C33-C34)",
        !!site_var == "C34" ~ "Lung (C33-C34)",
        !!site_var == "C37" ~ "Thymus (C37)",
        !!site_var == "C38" ~ "Heart, mediastinum, pleura (C38)",
        !!site_var == "C39" ~ "Other respiratory system, inthrathoracic organs (C39)",
        !!site_var == "C40" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C41" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C43" ~ "Skin (C43)",
        !!site_var == "C44" ~ "Non-melanoma skin (C44)",
        !!site_var == "C45" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C46" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C47" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C48" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C49" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C50" ~ "Breast (C50)",
        !!site_var == "C51" ~ "Vulva (C51)",
        !!site_var == "C52" ~ "Vagina (C52)",
        !!site_var == "C53" ~ "Cervix (C53)",
        !!site_var == "C54" ~ "Uterus (C54-C55)",
        !!site_var == "C55" ~ "Uterus (C54-C55)",
        !!site_var == "C56" ~ "Ovary (C56)",
        !!site_var == "C57" ~ "Other female genital organs (C57)",
        !!site_var == "C58" ~ "Placenta (C58)",
        !!site_var == "C60" ~ "Penis (C60)",
        !!site_var == "C61" ~ "Prostate (C61)",
        !!site_var == "C62" ~ "Testis (C62)",
        !!site_var == "C63" ~ "Other male genital organs (C63)",
        !!site_var == "C64" ~ "Kidney (C64)",
        !!site_var == "C65" ~ "Urinary tract (C65-C68)",
        !!site_var == "C66" ~ "Urinary tract (C65-C68)",
        !!site_var == "C67" ~ "Urinary tract (C65-C68)",
        !!site_var == "C68" ~ "Urinary tract (C65-C68)",
        !!site_var == "C69" ~ "Eye (C69)",
        !!site_var == "C70" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C71" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C72" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C73" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C74" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C75" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C76" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C77" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C78" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C79" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C80" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C81" ~ "Hodgkin lymphoma (C81)",
        !!site_var == "C82" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C83" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C84" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C85" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C86" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C87" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C88" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C90" ~ "Multiple myeloma (C90)",
        !!site_var == "C91" ~ icdgroup_c9195_code,
        !!site_var == "C92" ~ icdgroup_c9195_code,
        !!site_var == "C93" ~ icdgroup_c9195_code,
        !!site_var == "C94" ~ icdgroup_c9195_code,
        !!site_var == "C95" ~ icdgroup_c9195_code,
        !!site_var == "C96" ~ icdgroup_c96_code
      ))%>%
    #   
    #var t_icdgroup_pub -> grouping used in publication
    tidytable::mutate.(
      t_icdgroup_pub = tidytable::case_when.(
        !!site_var == "C00" ~ "Lip (C00)",  
        !!site_var == "C01" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C02" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C03" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C04" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C05" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C06" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C07" ~ "Salivary glands (C07-C08)",
        !!site_var == "C08" ~ "Salivary glands (C07-C08)",
        !!site_var == "C09" ~ "Tonsil (C09)",
        !!site_var == "C10" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C11" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C12" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C13" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C14" ~ "Oral cavity, pharynx (C01-C06,C10-C14)",
        !!site_var == "C15" ~ icdgroup_c15_code,                     
        !!site_var == "C16" ~ "Stomach (C16)",
        !!site_var == "C17" ~ "Small intestine (C17)",
        !!site_var == "C18" ~ "Colorectum (C18-C20)",
        !!site_var == "C19" ~ "Colorectum (C18-C20)",
        !!site_var == "C20" ~ "Colorectum (C18-C20)",
        !!site_var == "C21" ~ "Anus (C21)",
        !!site_var == "C22" ~ "Liver (C22)",
        !!site_var == "C23" ~ "Gallbladder (C23-C24)",
        !!site_var == "C24" ~ "Gallbladder (C23-C24)",
        !!site_var == "C25" ~ "Pancreas (C25)",
        !!site_var == "C26" ~ "Other digestive organs (C26)",
        !!site_var == "C30" ~ "Nasal cavity, middle ear (C30)",
        !!site_var == "C31" ~ "Accessory sinuses (C31)",
        !!site_var == "C32" ~ "Larynx (C32)",
        !!site_var == "C33" ~ "Lung (C33-C34)",
        !!site_var == "C34" ~ "Lung (C33-C34)",
        !!site_var == "C37" ~ "Thymus (C37)",
        !!site_var == "C38" ~ "Heart, mediastinum, pleura (C38)",
        !!site_var == "C39" ~ "Other respiratory system, inthrathoracic organs (C39)",
        !!site_var == "C40" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C41" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C43" ~ "Skin melanoma (C43)",
        !!site_var == "C44" ~ "Non-melanoma skin (C44)",
        !!site_var == "C45" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C46" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C47" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C48" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C49" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C50" ~ "Breast (C50)",
        !!site_var == "C51" ~ "Vulva (C51)",
        !!site_var == "C52" ~ "Vagina (C52)",
        !!site_var == "C53" ~ "Cervix (C53)",
        !!site_var == "C54" ~ "Uterus (C54-C55)",
        !!site_var == "C55" ~ "Uterus (C54-C55)",
        !!site_var == "C56" ~ "Ovary (C56)",
        !!site_var == "C57" ~ "Other female genital organs (C57)",
        !!site_var == "C58" ~ "Placenta (C58)",
        !!site_var == "C60" ~ "Penis (C60)",
        !!site_var == "C61" ~ "Prostate (C61)",
        !!site_var == "C62" ~ "Testis (C62)",
        !!site_var == "C63" ~ "Other male genital organs (C63)",
        !!site_var == "C64" ~ "Kidney (C64)",
        !!site_var == "C65" ~ "Urinary tract (C65-C68)",
        !!site_var == "C66" ~ "Urinary tract (C65-C68)",
        !!site_var == "C67" ~ "Urinary tract (C65-C68)",
        !!site_var == "C68" ~ "Urinary tract (C65-C68)",
        !!site_var == "C69" ~ "Eye (C69)",
        !!site_var == "C70" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C71" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C72" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C73" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C74" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C75" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C76" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C77" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C78" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C79" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C80" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C81" ~ "Hodgkin lymphoma (C81)",
        !!site_var == "C82" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C83" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C84" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C85" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C86" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C87" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C88" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C90" ~ "Multiple myeloma (C90)",
        !!site_var == "C91" ~ icdgroup_c9195s_code,
        !!site_var == "C92" ~ icdgroup_c92_code,
        !!site_var == "C93" ~ icdgroup_c9195s_code,
        !!site_var == "C94" ~ icdgroup_c9195s_code,
        !!site_var == "C95" ~ icdgroup_c9195s_code,
        !!site_var == "C96" ~ icdgroup_c96_code
      ))%>%
    #    
    #var t_icdgroup_pub -> grouping used in publication detailed
    tidytable::mutate.(
      t_icdgroup_pubdet = tidytable::case_when.(
        !!site_var == "C00" ~ "Lip (C00)",  
        !!site_var == "C01" ~ "Oral cavity (C01-C06)",
        !!site_var == "C02" ~ "Oral cavity (C01-C06)",
        !!site_var == "C03" ~ "Oral cavity (C01-C06)",
        !!site_var == "C04" ~ "Oral cavity (C01-C06)",
        !!site_var == "C05" ~ "Oral cavity (C01-C06)",
        !!site_var == "C06" ~ "Oral cavity (C01-C06)",
        !!site_var == "C07" ~ "Salivary glands (C07-C08)",
        !!site_var == "C08" ~ "Salivary glands (C07-C08)",
        !!site_var == "C09" ~ "Tonsil (C09)",
        !!site_var == "C10" ~ "Pharynx (C10-C14)",
        !!site_var == "C11" ~ "Pharynx (C10-C14)",
        !!site_var == "C12" ~ "Pharynx (C10-C14)",
        !!site_var == "C13" ~ "Pharynx (C10-C14)",
        !!site_var == "C14" ~ "Pharynx (C10-C14)",
        !!site_var == "C15" ~ icdgroup_c15_code,                     
        !!site_var == "C16" ~ "Stomach (C16)",
        !!site_var == "C17" ~ "Small intestine (C17)",
        !!site_var == "C18" ~ "Colorectum (C18-C20)",
        !!site_var == "C19" ~ "Colorectum (C18-C20)",
        !!site_var == "C20" ~ "Colorectum (C18-C20)",
        !!site_var == "C21" ~ "Anus (C21)",
        !!site_var == "C22" ~ "Liver (C22)",
        !!site_var == "C23" ~ "Gallbladder (C23-C24)",
        !!site_var == "C24" ~ "Gallbladder (C23-C24)",
        !!site_var == "C25" ~ "Pancreas (C25)",
        !!site_var == "C26" ~ "Other digestive organs (C26)",
        !!site_var == "C30" ~ "Nasal cavity, middle ear, accessory sinuses (C30-C31)",
        !!site_var == "C31" ~ "Nasal cavity, middle ear, accessory sinuses (C30-C31)",
        !!site_var == "C32" ~ "Larynx (C32)",
        !!site_var == "C33" ~ "Lung (C33-C34)",
        !!site_var == "C34" ~ "Lung (C33-C34)",
        !!site_var == "C37" ~ "Thymus (C37)",
        !!site_var == "C38" ~ "Heart, mediastinum, pleura (C38)",
        !!site_var == "C39" ~ "Other respiratory system, inthrathoracic organs (C39)",
        !!site_var == "C40" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C41" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C43" ~ "Skin melanoma (C43)",
        !!site_var == "C44" ~ "Non-melanoma skin (C44)",
        !!site_var == "C45" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C46" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C47" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C48" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C49" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C50" ~ "Breast (C50)",
        !!site_var == "C51" ~ "Vulva (C51)",
        !!site_var == "C52" ~ "Vagina (C52)",
        !!site_var == "C53" ~ "Cervix (C53)",
        !!site_var == "C54" ~ "Uterus (C54-C55)",
        !!site_var == "C55" ~ "Uterus (C54-C55)",
        !!site_var == "C56" ~ "Ovary (C56)",
        !!site_var == "C57" ~ "Other female genital organs (C57)",
        !!site_var == "C58" ~ "Placenta (C58)",
        !!site_var == "C60" ~ "Penis (C60)",
        !!site_var == "C61" ~ "Prostate (C61)",
        !!site_var == "C62" ~ "Testis (C62)",
        !!site_var == "C63" ~ "Other male genital organs (C63)",
        !!site_var == "C64" ~ "Kidney (C64)",
        !!site_var == "C65" ~ "Urinary tract (C65-C68)",
        !!site_var == "C66" ~ "Urinary tract (C65-C68)",
        !!site_var == "C67" ~ "Urinary tract (C65-C68)",
        !!site_var == "C68" ~ "Urinary tract (C65-C68)",
        !!site_var == "C69" ~ "Eye (C69)",
        !!site_var == "C70" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C71" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C72" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C73" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C74" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C75" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C76" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C77" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C78" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C79" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C80" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C81" ~ "Hodgkin lymphoma (C81)",
        !!site_var == "C82" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C83" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C84" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C85" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C86" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C87" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C88" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C90" ~ "Multiple myeloma (C90)",
        !!site_var == "C91" ~ icdgroup_c9195s_code,
        !!site_var == "C92" ~ icdgroup_c92_code,
        !!site_var == "C93" ~ icdgroup_c9195s_code,
        !!site_var == "C94" ~ icdgroup_c9195s_code,
        !!site_var == "C95" ~ icdgroup_c9195s_code,
        !!site_var == "C96" ~ icdgroup_c96_code
      ))%>%
    #    
    #var t_icdgroup_s -> grouping of SPC - separated by smoking-relatedness
    tidytable::mutate.(
      t_icdgroup_s = tidytable::case_when.(
        !!site_var == "C00" ~ "Lip (C00)",  
        !!site_var == "C01" ~ "Oral cavity (C01-C06)",
        !!site_var == "C02" ~ "Oral cavity (C01-C06)",
        !!site_var == "C03" ~ "Oral cavity (C01-C06)",
        !!site_var == "C04" ~ "Oral cavity (C01-C06)",
        !!site_var == "C05" ~ "Oral cavity (C01-C06)",
        !!site_var == "C06" ~ "Oral cavity (C01-C06)",
        !!site_var == "C07" ~ "Salivary glands (C07-C08)",
        !!site_var == "C08" ~ "Salivary glands (C07-C08)",
        !!site_var == "C09" ~ "Tonsil (C09)",
        !!site_var == "C10" ~ "Pharynx (C10-C14)",
        !!site_var == "C11" ~ "Pharynx (C10-C14)",
        !!site_var == "C12" ~ "Pharynx (C10-C14)",
        !!site_var == "C13" ~ "Pharynx (C10-C14)",
        !!site_var == "C14" ~ "Pharynx (C10-C14)",
        !!site_var == "C15" ~ icdgroup_c15_code,                     
        !!site_var == "C16" ~ "Stomach (C16)",
        !!site_var == "C17" ~ "Small intestine (C17)",
        !!site_var == "C18" ~ "Colorectum (C18-C20)",
        !!site_var == "C19" ~ "Colorectum (C18-C20)",
        !!site_var == "C20" ~ "Colorectum (C18-C20)",
        !!site_var == "C21" ~ "Anus (C21)",
        !!site_var == "C22" ~ "Liver (C22)",
        !!site_var == "C23" ~ "Gallbladder (C23-C24)",
        !!site_var == "C24" ~ "Gallbladder (C23-C24)",
        !!site_var == "C25" ~ "Pancreas (C25)",
        !!site_var == "C26" ~ "Other digestive organs (C26)",
        !!site_var == "C30" ~ "Nasal cavity, middle ear (C30)",
        !!site_var == "C31" ~ "Accessory sinuses (C31)",
        !!site_var == "C32" ~ "Larynx (C32)",
        !!site_var == "C33" ~ "Lung (C33-C34)",
        !!site_var == "C34" ~ "Lung (C33-C34)",
        !!site_var == "C37" ~ "Thymus (C37)",
        !!site_var == "C38" ~ "Heart, mediastinum, pleura (C38)",
        !!site_var == "C39" ~ "Other respiratory system, inthrathoracic organs (C39)",
        !!site_var == "C40" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C41" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C43" ~ "Skin (C43)",
        !!site_var == "C44" ~ "Non-melanoma skin (C44)",
        !!site_var == "C45" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C46" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C47" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C48" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C49" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C50" ~ "Breast (C50)",
        !!site_var == "C51" ~ "Vulva (C51)",
        !!site_var == "C52" ~ "Vagina (C52)",
        !!site_var == "C53" ~ "Cervix (C53)",
        !!site_var == "C54" ~ "Uterus (C54-C55)",
        !!site_var == "C55" ~ "Uterus (C54-C55)",
        !!site_var == "C56" ~ "Ovary (C56)",
        !!site_var == "C57" ~ "Other female genital organs (C57)",
        !!site_var == "C58" ~ "Placenta (C58)",
        !!site_var == "C60" ~ "Penis (C60)",
        !!site_var == "C61" ~ "Prostate (C61)",
        !!site_var == "C62" ~ "Testis (C62)",
        !!site_var == "C63" ~ "Other male genital organs (C63)",
        !!site_var == "C64" ~ "Kidney (C64)",
        !!site_var == "C65" ~ "Renal pelvis (C65)",
        !!site_var == "C66" ~ "Ureter (C66)",
        !!site_var == "C67" ~ "Urinary bladder (C67)",
        !!site_var == "C68" ~ "Other and unspecified urinary organs (C68)",
        !!site_var == "C69" ~ "Eye (C69)",
        !!site_var == "C70" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C71" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C72" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C73" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C74" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C75" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C76" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C77" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C78" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C79" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C80" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C81" ~ "Hodgkin lymphoma (C81)",
        !!site_var == "C82" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C83" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C84" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C85" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C86" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C87" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C88" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C90" ~ "Multiple myeloma (C90)",
        !!site_var == "C91" ~ icdgroup_c9195s_code,
        !!site_var == "C92" ~ icdgroup_c92_code,
        !!site_var == "C93" ~ icdgroup_c9195s_code,
        !!site_var == "C94" ~ icdgroup_c9195s_code,
        !!site_var == "C95" ~ icdgroup_c9195s_code,
        !!site_var == "C96" ~ icdgroup_c96_code
      ))%>%
    #var t_icdgroup_sdet -> grouping of SPC - separated by smoking-relatedness; details on C00-C14
    tidytable::mutate.(
      t_icdgroup_sdet = tidytable::case_when.(
        !!site_var == "C00" ~ "Lip (C00)",  
        !!site_var == "C01" ~ "Oral cavity (C01-C06)",
        !!site_var == "C02" ~ "Oral cavity (C01-C06)",
        !!site_var == "C03" ~ "Oral cavity (C01-C06)",
        !!site_var == "C04" ~ "Oral cavity (C01-C06)",
        !!site_var == "C05" ~ "Oral cavity (C01-C06)",
        !!site_var == "C06" ~ "Oral cavity (C01-C06)",
        !!site_var == "C07" ~ "Salivary glands (C07-C08)",
        !!site_var == "C08" ~ "Salivary glands (C07-C08)",
        !!site_var == "C09" ~ "Tonsil (C09)",
        !!site_var == "C10" ~ "Pharynx (C10-C14)",
        !!site_var == "C11" ~ "Pharynx (C10-C14)",
        !!site_var == "C12" ~ "Pharynx (C10-C14)",
        !!site_var == "C13" ~ "Pharynx (C10-C14)",
        !!site_var == "C14" ~ "Pharynx (C10-C14)",
        !!site_var == "C15" ~ icdgroup_c15_code,                     
        !!site_var == "C16" ~ "Stomach (C16)",
        !!site_var == "C17" ~ "Small intestine (C17)",
        !!site_var == "C18" ~ "Colorectum (C18-C20)",
        !!site_var == "C19" ~ "Colorectum (C18-C20)",
        !!site_var == "C20" ~ "Colorectum (C18-C20)",
        !!site_var == "C21" ~ "Anus (C21)",
        !!site_var == "C22" ~ "Liver (C22)",
        !!site_var == "C23" ~ "Gallbladder (C23-C24)",
        !!site_var == "C24" ~ "Gallbladder (C23-C24)",
        !!site_var == "C25" ~ "Pancreas (C25)",
        !!site_var == "C26" ~ "Other digestive organs (C26)",
        !!site_var == "C30" ~ "Nasal cavity, middle ear (C30)",
        !!site_var == "C31" ~ "Accessory sinuses (C31)",
        !!site_var == "C32" ~ "Larynx (C32)",
        !!site_var == "C33" ~ "Lung (C33-C34)",
        !!site_var == "C34" ~ "Lung (C33-C34)",
        !!site_var == "C37" ~ "Thymus (C37)",
        !!site_var == "C38" ~ "Heart, mediastinum, pleura (C38)",
        !!site_var == "C39" ~ "Other respiratory system, inthrathoracic organs (C39)",
        !!site_var == "C40" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C41" ~ "Bone, articular cartilage (C40-C41)",
        !!site_var == "C43" ~ "Skin (C43)",
        !!site_var == "C44" ~ "Non-melanoma skin (C44)",
        !!site_var == "C45" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C46" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C47" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C48" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C49" ~ "Mesothelial, soft tissue (C45-C49)",
        !!site_var == "C50" ~ "Breast (C50)",
        !!site_var == "C51" ~ "Vulva (C51)",
        !!site_var == "C52" ~ "Vagina (C52)",
        !!site_var == "C53" ~ "Cervix (C53)",
        !!site_var == "C54" ~ "Uterus (C54-C55)",
        !!site_var == "C55" ~ "Uterus (C54-C55)",
        !!site_var == "C56" ~ "Ovary (C56)",
        !!site_var == "C57" ~ "Other female genital organs (C57)",
        !!site_var == "C58" ~ "Placenta (C58)",
        !!site_var == "C60" ~ "Penis (C60)",
        !!site_var == "C61" ~ "Prostate (C61)",
        !!site_var == "C62" ~ "Testis (C62)",
        !!site_var == "C63" ~ "Other male genital organs (C63)",
        !!site_var == "C64" ~ "Kidney (C64)",
        !!site_var == "C65" ~ "Renal pelvis (C65)",
        !!site_var == "C66" ~ "Ureter (C66)",
        !!site_var == "C67" ~ "Urinary bladder (C67)",
        !!site_var == "C68" ~ "Other and unspecified urinary organs (C68)",
        !!site_var == "C69" ~ "Eye (C69)",
        !!site_var == "C70" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C71" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C72" ~ "Brain, central nervous system (C70-C72)",
        !!site_var == "C73" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C74" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C75" ~ "Thyroid, endocrine glands (C73-C75)",
        !!site_var == "C76" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C77" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C78" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C79" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C80" ~ "Ill-defined sites (C76-C80)",
        !!site_var == "C81" ~ "Hodgkin lymphoma (C81)",
        !!site_var == "C82" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C83" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C84" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C85" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C86" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C87" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C88" ~ "Non-Hodgkin lymphoma (C82-C88)",
        !!site_var == "C90" ~ "Multiple myeloma (C90)",
        !!site_var == "C91" ~ icdgroup_c9195s_code,
        !!site_var == "C92" ~ icdgroup_c92_code,
        !!site_var == "C93" ~ icdgroup_c9195s_code,
        !!site_var == "C94" ~ icdgroup_c9195s_code,
        !!site_var == "C95" ~ icdgroup_c9195s_code,
        !!site_var == "C96" ~ icdgroup_c96_code
      ))%>%
    #second cancer smoking related according to @secretanReviewHumanCarcinogens2009
    #* asterisk highlights sites where differentiation on an ICD-O-3 level is not possible and all diagnoses under this code have been defined as smoking-related.
    #Source: Secretan B, Straif K, Baan R, Grosse Y, Ghissassi FE, Bouvard V, et al. A review of human carcinogens—Part E: tobacco, areca nut, alcohol, coal smoke, and salted fish. The Lancet Oncology [Internet]. 2009 Nov 1 [cited 2019 Aug 19];10(11):1033–4. Available from: https://www.thelancet.com/journals/lanonc/article/PIIS1470-2045(09)70326-2/abstract
    tidytable::mutate.(
      t_smokeiarc = tidytable::case_when.(
        !!site_var == "C00" ~ 0, #Lip
        !!site_var == "C01" ~ 1, #Oral cavity
        !!site_var == "C02" ~ 1, #Oral cavity
        !!site_var == "C03" ~ 1, #Oral cavity
        !!site_var == "C04" ~ 1, #Oral cavity
        !!site_var == "C05" ~ 1, #Oral cavity
        !!site_var == "C06" ~ 1, #Oral cavity
        !!site_var == "C07" ~ 0, #Salivary glands
        !!site_var == "C08" ~ 0, #Salivary glands
        !!site_var == "C09" ~ 0, #Tonsil
        !!site_var == "C10" ~ 1, #oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C11" ~ 1, #oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C12" ~ 1, #oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C13" ~ 1, #oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C14" ~ 1, #oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C15" ~ 1, #oesophagus
        !!site_var == "C16" ~ 1, #stomach
        !!site_var == "C18" ~ 1, #colorectum
        !!site_var == "C19" ~ 1, #colorectum
        !!site_var == "C20" ~ 1, #colorectum
        !!site_var == "C22" ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
        !!site_var == "C25" ~ 1, #pancreas
        !!site_var == "C30" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
        !!site_var == "C31" ~ 1, #paranasal sinuses
        !!site_var == "C32" ~ 1, #larynx
        !!site_var == "C33" ~ 1, #lung
        !!site_var == "C34" ~ 1, #lung
        !!site_var == "C53" ~ 1, #uterine cervix
        !!site_var == "C56" ~ 1, #ovary (mucinous)
        !!site_var == "C64" ~ 1, #kidney (body and pelvis)
        !!site_var == "C65" ~ 1, #kidney (body and pelvis)
        !!site_var == "C66" ~ 1, #urinary bladder, ureter
        !!site_var == "C67" ~ 1, #urinary bladder, ureter
        !!site_var == "C68" ~ 1, #other urinary
        !!site_var == "C92" ~ 1, #bone marrow (myeloid leukaemia)
        !is.na(!!site_var)  ~ 0, #all other cancers
        TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smokeiarc="Smoking-related Cancer type (IARC definition)") %>%
    sjlabelled::set_labels(t_smokeiarc, labels = c("other cancer" = 0,
                                                   "smoking-related IARC" = 1)) %>%
    #second cancer smoking related according to @barclayIncidenceSecondHigher2019
    #ICD-10-based definition; ** denotes additional restriction to morphology codes by the orginal authors which could not be applied due to missing morphology data
    #Source: Barclay ME, Lyratzopoulos G, Walter FM, Jefferies S, Peake MD, Rintoul RC. Incidence of second and higher order smoking-related primary cancers following lung cancer: a population-based cohort study. Thorax [Internet]. 2019 [cited 2019 Apr 4];74:466–72. Available from: https://thorax.bmj.com/content/early/2019/02/17/thoraxjnl-2018-212456
    tidytable::mutate.(
      t_smoke_barclay = tidytable::case_when.(
        !!site_var == "C00" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C01" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C02" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C03" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C04" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C05" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C06" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C07" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C08" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C09" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C10" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C11" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C12" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C13" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C14" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C15" ~ 1, #oesophagus**
        !!site_var == "C31" ~ 1, #paranasal sinuses
        !!site_var == "C32" ~ 1, #larynx
        !!site_var == "C33" ~ 1, #lung
        !!site_var == "C34" ~ 1, #lung
        !!site_var == "C67" ~ 1, #urinary bladder
        !is.na(!!site_var)    ~ 0, #all other cancers
        TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_barclay="Smoking-related Cancer type (Barclay definition)") %>%
    sjlabelled::set_labels(t_smoke_barclay, labels = c("other cancer" = 0,
                                                       "smoking-related cancer (Barclay)" = 1))%>%
    #second cancer smoking related according to \cite{boakyeTrendsRiskBurden2019}
    #ICD-10-based definition; top 10 smoking-related cancers 
    #Source: Boakye EA, Buchanan P, Hinyard L, Osazuwa‐Peters N, Simpson MC, Schootman M, et al. Trends in the risk and burden of second primary malignancy among survivors of smoking-related cancers in the United States. International Journal of Cancer. 2019;145(1):143–53. 
    tidytable::mutate.(
      t_smoke_boakye = tidytable::case_when.(
        !!site_var == "C00" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C01" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C02" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C03" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C04" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C05" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C06" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C07" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C08" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C09" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C10" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C11" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C12" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C13" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C14" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
        !!site_var == "C15" ~ 1, #oesophagus
        !!site_var == "C18" ~ 1, #colorectum  --- Figure 1 also shows small intestine, but main text and supplement file do not mention C17
        !!site_var == "C19" ~ 1, #colorectum
        !!site_var == "C20" ~ 1, #colorectum
        !!site_var == "C22" ~ 1, #liver
        !!site_var == "C25" ~ 1, #pancreas
        !!site_var == "C30" ~ 1, #nasal cavity and middle ear
        !!site_var == "C31" ~ 1, #paranasal sinuses
        !!site_var == "C32" ~ 1, #larynx
        !!site_var == "C33" ~ 1, #lung
        !!site_var == "C34" ~ 1, #lung
        !!site_var == "C53" ~ 1, #uterine cervix
        !!site_var == "C64" ~ 1, #kidney --- Boakye et al. mention kidney & renal pelvis, but supplement file shows C64.9 for kidney
        !!site_var == "C67" ~ 1, #urinary bladder
        !is.na(!!site_var)  ~ 0, #all other cancers
        TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_boakye="Smoking-related Cancer type (Boakye definition)") %>%
    sjlabelled::set_labels(t_smoke_boakye, labels = c("other cancer" = 0,
                                                      "smoking-related cancer (Boakye)" = 1))%>%
    tidytable::mutate_across.(.cols = c(t_smokeiarc, t_smoke_barclay, t_smoke_boakye), .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))  %>%
    tidytable::mutate_across.(.cols = c(t_icdgroup, t_icdgroup_pub, t_icdgroup_pubdet,
                                        t_icdgroup_s, t_icdgroup_sdet), .fns = as.factor) %>%
    tidytable::mutate_across.(.cols = c(t_icdgroup, t_icdgroup_pub, t_icdgroup_pubdet,
                                        t_icdgroup_s, t_icdgroup_sdet), .fns = ~forcats::fct_reorder(.x, .x = !!site_var, .fun = min)) %>%
    sjlabelled::var_labels(t_icdgroup="Tumor group (ICD-10GM based grouping)") %>%
    sjlabelled::var_labels(t_icdgroup_pub="Tumor group (ICD-10GM based grouping)") %>%
    sjlabelled::var_labels(t_icdgroup_pubdet="Tumor group (ICD-10GM based grouping)") %>%
    sjlabelled::var_labels(t_icdgroup_s="Tumor group (ICD-10GM based grouping)") %>%
    sjlabelled::var_labels(t_icdgroup_sdet="Tumor group detailed (ICD-10GM based grouping)") %>%
    {if (tidytable::is_tidytable(results_df)){.} else{tibble::as_tibble(.)}}
  
}



#cr_icdgroup_vars_smoke: Create new variable that recodes site group var into smoke-related sites

cr_icdgroup_vars_smoke <- function(results_df, sitegroup_var, en_gb = FALSE){
  sitegroup_var <- rlang::ensym(sitegroup_var)
  
  icdgroup_c15_code    <- if(en_gb){"Oesophagus (C15)"}else{"Esophagus (C15)"}
  icdgroup_c9195_code  <- if(en_gb){"Leukaemias (C91-C95)"}else{"Leukemias (C91-C95)"}
  icdgroup_c9195s_code <- if(en_gb){"Non-Myeloid Leukaemias (C91, C93-C95)"}else{"Non-Myeloid Leukemias (C91, C93-C95)"}
  icdgroup_c92_code    <- if(en_gb){"Myeloid leukaemia (C92)"}else{"Myeloid leukemia (C92)"}
  icdgroup_c96_code    <- if(en_gb){"Other lymphoid, haematopoietic tissue (C96)"}else{"Other lymphoid, hematopoietic tissue (C96)"}
  
  
  results_df %>%
    #second cancer smoking related according to @secretanReviewHumanCarcinogens2009
    #* asterisk highlights sites where differentiation on an ICD-O-3 level is not possible and all diagnoses under this code have been defined as smoking-related.
    #Source: Secretan B, Straif K, Baan R, Grosse Y, Ghissassi FE, Bouvard V, et al. A review of human carcinogens—Part E: tobacco, areca nut, alcohol, coal smoke, and salted fish. The Lancet Oncology [Internet]. 2009 Nov 1 [cited 2019 Aug 19];10(11):1033–4. Available from: https://www.thelancet.com/journals/lanonc/article/PIIS1470-2045(09)70326-2/abstract
    dplyr::mutate(t_smokeiarc = case_when(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 0,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 0,
      !!sitegroup_var == "Tonsil (C09)"              ~ 0,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code        ~ 1, #oesophagus
      !!sitegroup_var == "Stomach (C16)"          ~ 1, #stomach
      !!sitegroup_var == "Colorectum (C18-C20)"   ~ 1, #colorectum
      !!sitegroup_var == "Liver (C22)"            ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
      !!sitegroup_var == "Pancreas (C25)"         ~ 1, #pancreas
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Nasal cavity, middle ear (C30)" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
      !!sitegroup_var == "Accessory sinuses (C31)" ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"           ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"         ~ 1, #lung
      !!sitegroup_var == "Cervix (C53)"           ~ 1, #uterine cervix
      !!sitegroup_var == "Ovary (C56)"            ~ 1, #ovary (mucinous)
      !!sitegroup_var == "Kidney (C64)"           ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Renal pelvis (C65)"     ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Ureter (C66)"           ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Urinary bladder (C67)"  ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Other and unspecified urinary organs (C68)"  ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Urinary tract (C65-C68)"  ~ 1,
      !!sitegroup_var == icdgroup_c92_code        ~ 1, #bone marrow (myeloid leukaemia)
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smokeiarc="Smoking-related Cancer type (IARC definition)") %>%
    sjlabelled::set_labels(t_smokeiarc, labels = c("other cancer" = 0,
                                                   "smoking-related IARC" = 1)) %>%
    dplyr::mutate(dplyr::across(.cols = t_smokeiarc, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    #second cancer smoking related according to @barclayIncidenceSecondHigher2019
    #ICD-10-based definition; ** denotes additional restriction to morphology codes by the orginal authors which could not be applied due to missing morphology data
    #Source: Barclay ME, Lyratzopoulos G, Walter FM, Jefferies S, Peake MD, Rintoul RC. Incidence of second and higher order smoking-related primary cancers following lung cancer: a population-based cohort study. Thorax [Internet]. 2019 [cited 2019 Apr 4];74:466–72. Available from: https://thorax.bmj.com/content/early/2019/02/17/thoraxjnl-2018-212456
    dplyr::mutate(t_smoke_barclay = case_when(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 1,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 1,
      !!sitegroup_var == "Tonsil (C09)"              ~ 1,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code           ~ 1, #oesophagus**
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Accessory sinuses (C31)"   ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"              ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"            ~ 1, #lung
      !!sitegroup_var == "Urinary bladder (C67)"     ~ 1, #urinary bladder, ureter
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_barclay="Smoking-related Cancer type (Barclay definition)") %>%
    sjlabelled::set_labels(t_smoke_barclay, labels = c("other cancer" = 0,
                                                       "smoking-related cancer (Barclay)" = 1))%>%
    dplyr::mutate(dplyr::across(.cols = t_smoke_barclay, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))) %>%
    #second cancer smoking related according to \cite{boakyeTrendsRiskBurden2019}
    #ICD-10-based definition; top 10 smoking-related cancers 
    #Source: Boakye EA, Buchanan P, Hinyard L, Osazuwa‐Peters N, Simpson MC, Schootman M, et al. Trends in the risk and burden of second primary malignancy among survivors of smoking-related cancers in the United States. International Journal of Cancer. 2019;145(1):143–53. 
    dplyr::mutate(t_smoke_boakye = case_when(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 1,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 1,
      !!sitegroup_var == "Tonsil (C09)"              ~ 1,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code        ~ 1, #oesophagus
      !!sitegroup_var == "Stomach (C16)"          ~ 1, #stomach
      !!sitegroup_var == "Colorectum (C18-C20)"   ~ 1, #colorectum
      !!sitegroup_var == "Liver (C22)"            ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
      !!sitegroup_var == "Pancreas (C25)"         ~ 1, #pancreas
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Nasal cavity, middle ear (C30)" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
      !!sitegroup_var == "Accessory sinuses (C31)" ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"           ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"         ~ 1, #lung
      !!sitegroup_var == "Cervix (C53)"           ~ 1, #uterine cervix
      !!sitegroup_var == "Kidney (C64)"           ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Urinary bladder (C67)"  ~ 1, #urinary bladder, ureter
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_boakye="Smoking-related Cancer type (Boakye definition)") %>%
    sjlabelled::set_labels(t_smoke_boakye, labels = c("other cancer" = 0,
                                                      "smoking-related (Boakye)" = 1)) %>%
    dplyr::mutate(dplyr::across(.cols = t_smoke_boakye, .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE)))
  
  
}

#cr_icdgroup_vars_smoke: Create new variable that recodes site group var into smoke-related sites

cr_icdgroup_vars_smoke_tt <- function(results_df, sitegroup_var, en_gb = FALSE){
  sitegroup_var <- rlang::ensym(sitegroup_var)
  
  icdgroup_c15_code    <- if(en_gb){"Oesophagus (C15)"}else{"Esophagus (C15)"}
  icdgroup_c9195_code  <- if(en_gb){"Leukaemias (C91-C95)"}else{"Leukemias (C91-C95)"}
  icdgroup_c9195s_code <- if(en_gb){"Non-Myeloid Leukaemias (C91, C93-C95)"}else{"Non-Myeloid Leukemias (C91, C93-C95)"}
  icdgroup_c92_code    <- if(en_gb){"Myeloid leukaemia (C92)"}else{"Myeloid leukemia (C92)"}
  icdgroup_c96_code    <- if(en_gb){"Other lymphoid, haematopoietic tissue (C96)"}else{"Other lymphoid, hematopoietic tissue (C96)"}
  
  
  results_df %>%
    #second cancer smoking related according to @secretanReviewHumanCarcinogens2009
    #* asterisk highlights sites where differentiation on an ICD-O-3 level is not possible and all diagnoses under this code have been defined as smoking-related.
    #Source: Secretan B, Straif K, Baan R, Grosse Y, Ghissassi FE, Bouvard V, et al. A review of human carcinogens—Part E: tobacco, areca nut, alcohol, coal smoke, and salted fish. The Lancet Oncology [Internet]. 2009 Nov 1 [cited 2019 Aug 19];10(11):1033–4. Available from: https://www.thelancet.com/journals/lanonc/article/PIIS1470-2045(09)70326-2/abstract
    tidytable::mutate.(t_smokeiarc = tidytable::case_when.(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 0,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 0,
      !!sitegroup_var == "Tonsil (C09)"              ~ 0,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code        ~ 1, #oesophagus
      !!sitegroup_var == "Stomach (C16)"          ~ 1, #stomach
      !!sitegroup_var == "Colorectum (C18-C20)"   ~ 1, #colorectum
      !!sitegroup_var == "Liver (C22)"            ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
      !!sitegroup_var == "Pancreas (C25)"         ~ 1, #pancreas
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Nasal cavity, middle ear (C30)" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
      !!sitegroup_var == "Accessory sinuses (C31)" ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"           ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"         ~ 1, #lung
      !!sitegroup_var == "Cervix (C53)"           ~ 1, #uterine cervix
      !!sitegroup_var == "Ovary (C56)"            ~ 1, #ovary (mucinous)
      !!sitegroup_var == "Kidney (C64)"           ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Renal pelvis (C65)"     ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Ureter (C66)"           ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Urinary bladder (C67)"  ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Other and unspecified urinary organs (C68)"  ~ 1, #urinary bladder, ureter
      !!sitegroup_var == "Urinary tract (C65-C68)"  ~ 1,
      !!sitegroup_var == icdgroup_c92_code        ~ 1, #bone marrow (myeloid leukaemia)
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smokeiarc="Smoking-related Cancer type (IARC definition)") %>%
    sjlabelled::set_labels(t_smokeiarc, labels = c("other cancer" = 0,
                                                   "smoking-related IARC" = 1)) %>%
    #second cancer smoking related according to @barclayIncidenceSecondHigher2019
    #ICD-10-based definition; ** denotes additional restriction to morphology codes by the orginal authors which could not be applied due to missing morphology data
    #Source: Barclay ME, Lyratzopoulos G, Walter FM, Jefferies S, Peake MD, Rintoul RC. Incidence of second and higher order smoking-related primary cancers following lung cancer: a population-based cohort study. Thorax [Internet]. 2019 [cited 2019 Apr 4];74:466–72. Available from: https://thorax.bmj.com/content/early/2019/02/17/thoraxjnl-2018-212456
    tidytable::mutate.(t_smoke_barclay = tidytable::case_when.(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 1,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 1,
      !!sitegroup_var == "Tonsil (C09)"              ~ 1,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code           ~ 1, #oesophagus**
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Accessory sinuses (C31)"   ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"              ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"            ~ 1, #lung
      !!sitegroup_var == "Urinary bladder (C67)"     ~ 1, #urinary bladder, ureter
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_barclay="Smoking-related Cancer type (Barclay definition)") %>%
    sjlabelled::set_labels(t_smoke_barclay, labels = c("other cancer" = 0,
                                                       "smoking-related cancer (Barclay)" = 1))%>%
    #second cancer smoking related according to \cite{boakyeTrendsRiskBurden2019}
    #ICD-10-based definition; top 10 smoking-related cancers 
    #Source: Boakye EA, Buchanan P, Hinyard L, Osazuwa‐Peters N, Simpson MC, Schootman M, et al. Trends in the risk and burden of second primary malignancy among survivors of smoking-related cancers in the United States. International Journal of Cancer. 2019;145(1):143–53. 
    tidytable::mutate.(t_smoke_boakye = tidytable::case_when.(
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      #differentiation of C00-C14 not applicable to all sitegroup_vars
      !!sitegroup_var == "Lip (C00)"                 ~ 1,
      !!sitegroup_var == "Oral cavity, pharynx (C01-C06,C10-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx
      !!sitegroup_var == "Oral cavity (C01-C06)"     ~ 1,
      !!sitegroup_var == "Salivary glands (C07-C08)" ~ 1,
      !!sitegroup_var == "Tonsil (C09)"              ~ 1,
      !!sitegroup_var == "Pharynx (C10-C14)"         ~ 1,
      #end of differentiation for C00-C14
      !!sitegroup_var == "Lip, oral cavity and pharynx (C00-C14)" ~ 1, #Oral cavity, oropharynx, nasopharynx, and hypopharynx*
      !!sitegroup_var == icdgroup_c15_code        ~ 1, #oesophagus
      !!sitegroup_var == "Stomach (C16)"          ~ 1, #stomach
      !!sitegroup_var == "Colorectum (C18-C20)"   ~ 1, #colorectum
      !!sitegroup_var == "Liver (C22)"            ~ 1, #liver * (including intraphepatic bile ducts although not defined by IARC)
      !!sitegroup_var == "Pancreas (C25)"         ~ 1, #pancreas
      !!sitegroup_var == "Nasal cavity, middle ear, accessory sinuses (C30-C31)" ~ 1, #nasal cavity and paranasal sinuses
      !!sitegroup_var == "Nasal cavity, middle ear (C30)" ~ 1, #nasal cavity and paranasal sinuses * (middle ear included although not defined by IARC)
      !!sitegroup_var == "Accessory sinuses (C31)" ~ 1, #paranasal sinuses
      !!sitegroup_var == "Larynx (C32)"           ~ 1, #larynx
      !!sitegroup_var == "Lung (C33-C34)"         ~ 1, #lung
      !!sitegroup_var == "Cervix (C53)"           ~ 1, #uterine cervix
      !!sitegroup_var == "Kidney (C64)"           ~ 1, #kidney (body and pelvis)
      !!sitegroup_var == "Urinary bladder (C67)"  ~ 1, #urinary bladder, ureter
      !is.na(!!sitegroup_var)    ~ 0, #all other cancers
      TRUE ~ NA_real_)) %>%
    sjlabelled::var_labels(t_smoke_boakye="Smoking-related Cancer type (Boakye definition)") %>%
    sjlabelled::set_labels(t_smoke_boakye, labels = c("other cancer" = 0,
                                                      "smoking-related (Boakye)" = 1)) %>%
    tidytable::mutate_across.(.cols = c(t_smokeiarc, t_smoke_barclay, t_smoke_boakye), .fns = ~sjlabelled::as_label(.x , keep.labels=TRUE))  %>%
    {if (tidytable::is_tidytable(results_df)){.} else{tibble::as_tibble(.)}}
  
  
}



#plot_sir_byfutime: Create new GGPlots for figures in spc_lung publication

plot_sir_byfutime <- function(results_df, sites_to_plot , timecats_to_plot = c("6-12 months", "1-5 years", "5-10 years", "Total"), 
                              color = "black", y_lim = NA, vlab_x_off = -0.5, vlab_y_pos = 0.5, vlab_y_diff = .1,
                              ylab = element_blank(), xlab = element_blank()){
 
  n_sites <- length(sites_to_plot)
  
  pd <- position_dodge(-0.35)
  
  #enforce y_lim and adjust upper CI
  if(!is.na(y_lim)){
    results_df <- results_df %>% 
      mutate(sir_uci = case_when(sir_uci > y_lim ~ y_lim,
                                 TRUE ~ sir_uci))
  }
  
  
  df <- results_df %>% 
    #filter for most frequent sites
    filter(t_site %in% sites_to_plot) %>%
    #for small case numbers,create flag 
    mutate(fewcases = case_when(expected >= 5 ~ "many",
                                TRUE ~ "few"),
           sir = case_when(expected >= 5 ~ sir,
                           TRUE ~ 0.3),
           sir_lci = case_when(expected >= 5 & sir_lci > 0.3 ~ sir_lci,
                               TRUE ~ 0.3),
           sir_uci = case_when(expected >= 5 ~ sir_uci,
                               TRUE ~ 0.3)) %>%
    #rename Total fu_time to make label shorter
    mutate(fu_time = recode(fu_time, 
                            "Total 0.5 to Inf years" = "Total",
                            "Total 0 to Inf years" = "Total"
                            )) %>%
    #rename Total t_site
    mutate(t_site := recode(t_site, 
                           "smoking-related IARC" = "Smoking-related cancers (*1)",
                           "other cancer" = "Other cancers (*2)",
                           "Non-Hodgkin lymphoma (C82-C88)" = "Non-Hodgkin lymph. (C82-88)"
                           ))
  #start ggplot
  df %>%
    ggplot(data =., aes(x=fu_time, y=sir, colour=sex, group=sex, shape = fewcases)) + 
    geom_errorbar(aes(ymin=sir_lci, ymax=sir_uci), colour="black", width=.2, position=pd) +
    geom_point(position=pd, size=3.5) + 
    #add overall SIR for males
    geom_text(
      aes(x=fu_time, y=vlab_y_pos, colour=sex, group=sex,
          label=ifelse(
            (fu_time == "Total" & sex == "Male" & sir!=0.3), paste0(format(sir, nsmall = 2), " (", format(sir_lci, nsmall = 2), "–", format(sir_uci, nsmall = 2), ")"), #paste sir with ci
            "")),                                                                                                                                                       #or nothing for few_cases
      nudge_x = vlab_x_off, size = 2.5) +
    #add overall SIR for females
    geom_text(
      aes(x=fu_time, y=vlab_y_pos-vlab_y_diff, colour=sex, group=sex,
          label=ifelse(
            (fu_time == "Total" & sex == "Female" & sir!=0.3), paste0(format(sir, nsmall = 2), " (", format(sir_lci, nsmall = 2), "–", format(sir_uci, nsmall = 2), ")"), 
            "")),
      nudge_x = vlab_x_off, size = 2.5) +
    #define estimate categories (shape)
    scale_shape_manual(name="Estimate", 
                       values = c(many = 21, few = 4),                     # 21 is filled circle
                       breaks=c("many", "few"), 
                       labels=c("SIR", "no estimate (<5 expected cases)")) + 
    #define sex categories (group)
    scale_colour_hue(name="Sex",    # Legend label, use darker colors
                     breaks=c("Male", "Female"),
                     labels=c("Male", "Female"),
                     l=40) +                    # Use darker colors, lightness=40 
    geom_hline(yintercept = 1) +
    #y axis - make log scale (current scaling depends on minimum value set above for fewcases = few --> sir == 0.3)
    scale_y_log10(limits = c(0.3, y_lim)) +
    #facetting
    facet_grid(t_smokeiarc ~ t_site) +
    #x axis - select wanted categories for x-axis
    xlim(timecats_to_plot) +
    guides(x = guide_axis(n.dodge = 2)) +
    theme_bw() + 
    theme(legend.position="none",
          axis.title.x = xlab,
          axis.title.y = ylab)


}

#plot_sir_forest: Create new GGPlot of SIR and CIs to a simple line with point estimate marker

plot_sir_forest <- function(results_df, categories_to_plot, 
                            cat_var = t_site, x_var = sir, x_lci_var = sir_lci, x_uci_var = sir_uci,
                            show_site = TRUE, show_scale = TRUE, show_nulleffect = TRUE, color = "black", filter_cat = TRUE,
                            x_min = 0.3, x_max = 10, line_width = 8){
  
  #set width of standard line
  lw <- line_width
  
  #when function is not used inside map call, first filter for site
  if(filter_cat == TRUE){
  results_df <- results_df %>% 
    #filter for most frequent sites
    filter({{cat_var}} %in% categories_to_plot) 
  }

  
  #start ggplot
  p <- results_df %>%
    ggplot(.) + 
    geom_point(aes(x={{x_var}}, y={{cat_var}}), size=3*lw, shape = 15, colour = color) + 
    geom_errorbarh(aes(xmin = {{x_lci_var}}, xmax = {{x_uci_var}}, y={{cat_var}}),
                   colour = color, height = 0, size = lw) +
    theme_minimal() +
    scale_x_log10(limits = c(x_min, x_max)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size=10),
          legend.position="none") 
  
  if(show_nulleffect == TRUE){
    p <- p + 
      geom_vline(xintercept = 1, size = lw/2) 
  }

  if(show_site == FALSE){
  p <- p + 
    theme(axis.text.y=element_blank()) 
  }
  
  if(show_scale == FALSE){
    p <- p + 
      theme(axis.text.x=element_blank()) 
  }
  
  p
  
  
}

#plot_sir_col: Create new GGPlot of SIR and CIs to a simple line with point estimate marker

plot_sir_col <- function(results_df, categories_to_plot, 
                            cat_var = t_site, y_var = sir, y_lci_var = sir_lci, y_uci_var = sir_uci,
                            show_site = TRUE, show_scale = TRUE, show_nulleffect = TRUE, color = "black", filter_cat = TRUE,
                            y_min = 0.3, y_max = 10, line_width = 8){
  
  #set width of standard line
  lw <- line_width
  
  #when function is not used inside map call, first filter for site
  if(filter_cat == TRUE){
    results_df <- results_df %>% 
      #filter for most frequent sites
      filter({{cat_var}} %in% categories_to_plot) 
  }
  
  
  #start ggplot
  p <- results_df %>%
    ggplot(.) + 
    geom_col(aes(y={{y_var}}, x={{cat_var}}), size=3*lw, fill = color) + 
    geom_errorbar(aes(ymin = {{y_lci_var}}, ymax = {{y_uci_var}}, x={{cat_var}}),
                   colour = "black", width = lw / 2, size = lw) +
    theme_minimal() +
    coord_cartesian(ylim = c(y_min, y_max)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size=10),
          legend.position="none") 
  
  if(show_nulleffect == TRUE){
    p <- p + 
      geom_hline(yintercept = 1, size = lw/2) 
  }
  
  if(show_site == FALSE){
    p <- p + 
      theme(axis.text.x=element_blank()) 
  }
  
  if(show_scale == FALSE){
    p <- p + 
      theme(axis.text.y=element_blank()) 
  }
  
  p
  
  
}

#plot_sir_bar: 

plot_sir_bar <- function(results_df, categories_to_plot, 
                            cat_var = t_site, x_var = sir, x_lci_var = sir_lci, x_uci_var = sir_uci,
                            show_site = TRUE, show_scale = TRUE, show_nulleffect = TRUE, color = "black", filter_cat = TRUE,
                            x_min = 0.3, x_max = 10, line_width = 8, line_ci = 1.5){
  
  #set width of standard line
  lw <- line_width
  
  #when function is not used inside map call, first filter for site
  if(filter_cat == TRUE){
    results_df <- results_df %>% 
      #filter for most frequent sites
      filter({{cat_var}} %in% categories_to_plot) 
  }
  
  
  #start ggplot
  p <- results_df %>%
    ggplot(.) + 
    geom_col(aes(y={{cat_var}}, x={{x_var}}), width = .7, fill = color) +
    geom_errorbarh(aes(xmin = {{x_lci_var}}, xmax = {{x_uci_var}}, y={{cat_var}}),
                   colour = "black", height = .5, size = lw*line_ci) +
    theme_minimal() +
    coord_cartesian(xlim = c(x_min, x_max)) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.title.x=element_blank(),
          axis.text.x=element_text(size=10),
          axis.title.y=element_blank(),
          axis.text.y=element_text(size=10),
          legend.position="none") 
  
  if(show_nulleffect == TRUE){
    p <- p + 
      geom_vline(xintercept = 1, size = lw) 
  }
  
  if(show_site == FALSE){
    p <- p + 
      theme(axis.text.y=element_blank()) 
  }
  
  if(show_scale == FALSE){
    p <- p + 
      theme(axis.text.x=element_blank()) 
  }
  
  p
  
  
}

