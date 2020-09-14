## libraries
library(stringr)
library(phonics)
library(REDCapR)


## set REDCap url and token
url <- 'https://redcapwebdev.sanfordhealth.org/redcap/api/'
token <- '55E3A211C0983FAC4BE5E944945FE295'


## read in REDCap data
dat01 <- REDCapR::redcap_read(redcap_uri=url, 
                  token=token,
                  fields=c('record_id','pat_first_name','pat_last_name','birth_date',
                                'pat_first_name_nci','pat_last_name_nci','birth_date_nci',
                                'pin','pin_nci'),
                  filter_logic="[verification]='0' and
                                  [pat_last_name_nci]<>'' and
                                  [pat_first_name_nci]<>'' and
                                  [birth_date_nci]<>''")$data




## perform matching and set verification status

#last name match
dat01$pat_last_name_nci_clean <- tolower(str_replace_all(dat01$pat_last_name_nci, "[^[:alnum:]]", ""))
dat01$pat_last_name_clean <- tolower(str_replace_all(dat01$pat_last_name, "[^[:alnum:]]", ""))

#last name _valid status
for(i in 1:nrow(dat01)){
  dat01[i,"pat_last_name_valid"] <- ifelse(phonex(dat01[i,"pat_last_name_nci_clean"]) == phonex(dat01[i,"pat_last_name_clean"]), 1, 0)
}




#first name match
dat01$pat_first_name_nci_clean <- tolower(str_replace_all(dat01$pat_first_name_nci, "[^[:alnum:]]", ""))
dat01$pat_first_name_clean <- tolower(str_replace_all(dat01$pat_first_name, "[^[:alnum:]]", ""))

#first name _valid status
for(i in 1:nrow(dat01)){
  dat01[i,"pat_first_name_valid"] <- ifelse(phonex(dat01[i,"pat_first_name_nci_clean"]) == phonex(dat01[i,"pat_first_name_clean"]), 1, 0)
}




#DOB _valid status
dat01$birth_date_valid <- ifelse(dat01$birth_date == dat01$birth_date_nci, 1, 0)




#PIN _valid status 
dat01$pin_valid <- ifelse(dat01$pin == dat01$pin_nci, 1, 0)
# dat01$pin_valid <- 1




## make 0 if NA
dat01[c("pat_last_name_valid","pat_first_name_valid","birth_date_valid","pin_valid")][is.na(dat01[c("pat_last_name_valid","pat_first_name_valid","birth_date_valid","pin_valid")])] <- 0



## verified status based on the four binary valid fields/status
dat01$verification <- ifelse(dat01$pat_last_name_valid==1 & 
                               dat01$pat_first_name_valid==1 & 
                               dat01$birth_date_valid==1 & 
                               dat01$pin_valid==1, 1, 3)


## set status that this person went through automated verification process
dat01$auto_verify <- 1




## this is only selecting a test subject -- all rows would be pushed back############################
dat01a <- as.data.frame(dat01[1,c("record_id",
                                  "pat_first_name_valid","pat_last_name_valid",
                                  "birth_date_valid","pin_valid",
                                  "verification",
                                  "auto_verify")])
# dat01a <- as.data.frame(dat01[,c("record_id",
#                                   "pat_first_name_valid","pat_last_name_valid",
#                                   "birth_date_valid","pin_valid",
#                                   "verification")])




## push back into REDCap
REDCapR::redcap_write(ds_to_write=dat01a,
              redcap_uri=url,
              token=token)
