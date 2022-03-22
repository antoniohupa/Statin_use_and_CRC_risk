** Statins and Colorectal Cancer Risk: A Population-Based Case- Control Study and Synthesis of the Epidemiological Evidence **
** Rodríguez-Miguel A et al
** JCM. DOI: https://doi.org/10.3390/jcm11061528

** Data analyst: Rodríguez-Miguel A

*******************************************************************************************************************************
*******************************************************************************************************************************

** Multiple imputation by chained equations (MICE) ** 

*** All analyses were performed through MICE. To that end, we first must to generate the imputed databases for missing variables "BMI" and "Smoke" 

*** Reduction of the number of variables from the original dataset

keep caso edadfechaindice1 sexo añocalendario n_visitas_cat_1y imc_cat  alc_ab_cat tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat aestreñ_cat p_anrect_cat pdig_s_cat hdb_cat hipgot_cat ///
     cortic_cat agnoaas_cat2 acot_cat2 adiabetics_cat ssri_sari_cat gastroprotectors_cat adiarr_cat aasbd_cat2 opios_cat2 opid_cat ainesopi_cat syst_cat2 ca_vitd_t_cat2 est_ncomb_cat o_lip_low_cat ///
	 antihypertensives_cat imc_cont id diab3_cat diab_cat hipgot_cat hta_cat eap_cat ci2_cat acv_cat followup_años prevencion1a_2a est_ncomb_prev est_ncomb_durc3 est_ncomb_durc4 est_ncomb_durc5 ///
	 est_ncomb_dac4 est_ncomb_dura4 aasbdt_cat agnoaas_cat edad70 simva_s_cat ator_s_cat prav_s_cat lova_cat fluva_cat rosu_cat pita_cat ceriv_cat simva_s_durc5 lova_durc5 prav_s_durc5 fluva_durc5 ///
	 ator_s_durc5 rosu_durc5 pita_durc5 est_ncomb_durc5 est_ncomb_intens2_durc5 est_ncomb_hl_durc5 est_ncomb_intens_durc5 simva_s_cat_doseAHA lova_cat_doseAHA prav_s_cat_doseAHA fluva_cat_doseAHA ///
	 ator_s_cat_doseAHA ceriv_cat_doseAHA rosu_cat_doseAHA pita_cat_doseAHA simva_s_cat_doseBA lova_cat_doseBA prav_s_cat_doseBA fluva_cat_doseBA ator_s_cat_doseBA rosu_cat_doseBA pita_cat_doseBA ///
	 ceriv_cat_doseBA est_ncomb_intens_3cat est_ncomb_intens_durc5 est_ncomb_hl_cat est_ncomb_hl_durc5 est_ncomb_intens_pcprop_cat est_ncomb_intens_pcprop_2cat est_ncomb_intens_2cat ///
	 est_ncomb_intens2_durc5 est_ncomb_intens_dac4 est_ncomb_hl_dac4 smet_bil_ezet_cat smet_bil_cat ezet_cat dpp4s_glp1t_cat dpp4_s_cat glp_1_t_cat agluc_oado_cat inh_agluc_cat otr_ado_cat ///
	 agluc_oado_su_cat agluc_oado_cat sulfo_cat est_ncomb_alfab_int_cat est_ncomb_bbloq_int_cat est_ncomb_araii_s_int_cat est_ncomb_ieca_s_int_cat est_ncomb_ca_ant_s_int_cat est_ncomb_diub_ah_int_cat ///
	 est_ncomb_ainesopi_int_cat est_ncomb_aasbd_int_cat est_ncomb_clopis_int_cat est_ncomb_triflu_int_cat est_ncomb_ezet_int_cat est_ncomb_fibr_int_cat est_ncomb_smet_bil_int_cat ///
	 est_ncomb_o_mod_li_int_cat est_ncomb_dpp4_s_int_cat est_ncomb_glp_1_t_int_cat est_ncomb_otr_ado_int_cat est_ncomb_tiazoli_t_int_cat est_ncomb_ado_int_cat est_ncomb_insul_int_cat ///
	 est_ncomb_metfors_int_cat est_ncomb_sulfo_int_cat est_ncomb_inh_agluc_int_cat est_ncomb_syst_int_cat est_ncomb_ca_vitd_t_int_cat est_ncomb_ssri_sari_int_cat est_ncomb_smet_bil_ezet_int_cat /// 
	 est_ncomb_dpp4s_glp1t_int_cat est_ncomb_agluc_oado_int_cat est_ncomb_agluc_oado_su_int_cat est_antihta2_int est_aag_int est_o_lip_low_int est_adiab_noins_int est_insul_int ///
	 bbloq_cat araii_s_cat ieca_s_cat ca_ant_s_cat diub_ah_cat alfab_cat dipiri_cat triflu_cat cilost_cat ticlopi_cat clopis_cat o_mod_li_cat smet_bil_cat fibr_cat

*** Generate missing indicator variables and explore assumptions for missing at random (MAR)

***Smoking
gen tabac_cat_m = tabac_cat 
recode tabac_cat_m 0=.

***BMI
misstable sum imc_cont tabac_cat_m, gen(miss_)
logistic miss_tabac_cat_m  caso edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y alc_ab_cat i.imc_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat  i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2  i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat  i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat 
regress miss_imc_cont  caso edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y alc_ab_cat i.tabac_cat g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat  i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2  i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat  i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat 

***Set MICE
mi set mlong
mi register imputed tabac_cat_m imc_cont
mi impute chained (regress, include(caso edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y alc_ab_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat)) imc_cont ///
                  (mlogit, include(caso edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y alc_ab_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat)) tabac_cat_m, ///
				  add(20) rseed(13) burnin(10)

***Categorize BMI after imputation
mi passive: gen imc_cat_imputed = 0 if imc_cont < 24.99
recode imc_cat_imputed .= 1 if imc_cont >= 25 & imc_cont <= 30
recode imc_cat_imputed .= 2 if imc_cont > 30
recode imc_cat_imputed .= 0
label variable imc_cat_imputed "0:<24.99, 1:25-30, 2:>30"

*******************************************************************************************************************************
*******************************************************************************************************************************

** TABLE 1. Baseline characteristics of cases and controls **

*** Tables of contingency

tab caso

foreach v of varlist edadfechaindice1 sexo followup_años n_visitas_cat_1y imc_cat tabac_cat alc_ab_cat diab3_cat hipgot_cat hta_cat eap_cat ci2_cat acv_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat p_anrect_cat pdig_s_cat hdb_cat {

if (`v' != edadfechaindice1 & `v' != followup_años) {
display "`v'"
display "*************************************************************************"
tab `v' caso,col
display "*************************************************************************"
}
else if (`v' == edadfechaindice1 | `v' == followup_años) {
display "`v'"
display "*************************************************************************"
bysort caso: sum  `v', detail
display "*************************************************************************"
}
}


*** Non-adjusted logistic regression

foreach v of varlist followup_años imc_cat tabac_cat  {

if `v' == followup_años  {
display "`v'"
display "*************************************************************************"
logistic caso edadfechaindice1 sexo añocalendario c.`v'
display "*************************************************************************"
}

if `v' == imc_cat {
display "`v'"
display "*************************************************************************"
logistic caso edadfechaindice1 sexo añocalendario ib1.`v'
display "*************************************************************************"
}

if `v' == tabac_cat {
display "`v'"
display "*************************************************************************"
logistic caso edadfechaindice1 sexo añocalendario ib3.`v'
display "*************************************************************************"
}
}

foreach v of varlist n_visitas_cat_1y alc_ab_cat diab3_cat hipgot_cat hta_cat eap_cat ci2_cat acv_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat p_anrect_cat pdig_s_cat hdb_cat {
display "`v'"
display "*************************************************************************"
logistic caso edadfechaindice1 sexo añocalendario i.`v'
display "*************************************************************************"
}


*** Fully-adjusted logistic regression

logistic caso edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat

*** Including followup

logistic caso edadfechaindice1 c.followup_años i.adiabetics_cat i.antihypertensives_cat sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat  i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.ssri_sari_cat i.gastroprotectors_cat  i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat

*** Including diabetes

logistic caso edadfechaindice1 i.diab_cat i.adiabetics_cat i.antihypertensives_cat sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat  i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.ssri_sari_cat i.gastroprotectors_cat  i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat 

*** Including hypertension

logistic caso edadfechaindice1 i.hta_cat#i.antihypertensives_cat i.adiabetics_cat i.hta_cat#i.antihypertensives_cat sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat  i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.ssri_sari_cat i.gastroprotectors_cat  i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat 

*** Including perypheral artery disease

logistic caso eap_cat edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat 

*** Including ischemic heart diseases

logistic caso i.ci2_cat edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat 

*** Including cerebrovascular accidents

logistic caso i.acv_cat edadfechaindice1 sexo añocalendario i.n_visitas_cat_1y ib1.imc_cat  alc_ab_cat ib3.tabac_cat  g_croni_cat refluj_cat  eii_cat c_irrit_cat  estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.est_ncomb_cat i.o_lip_low_cat i.antihypertensives_cat 


*******************************************************************************************************************************
*******************************************************************************************************************************

** TABLE 2. Use of statins and risk of colorectal cancer **

***Recency and continuous duration
foreach v in est_ncomb_cat est_ncomb_durc4 est_ncomb_durc5 {
display "`v'"
display "*************************************************************************"
tab `v' caso, col
logistic caso edadfechaindice1 sexo añocalendario i.`v'

***Fully-adjusted logistic regression using MICE
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.`v' i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat
display "*************************************************************************"
}

***Excluding prior use of NSAIDs and/or antiplatelet drugs, among current users
****Ever
foreach v in est_ncomb_cat est_ncomb_durc5  {
display "`v'"
display "*************************************************************************"
tab `v' caso if (ainesopi_cat ==0) & (aasbdt_cat==0) & (agnoaas_cat==0), col
logistic caso edadfechaindice1 sexo añocalendario i.`v' if (ainesopi_cat ==0) & (aasbdt_cat==0) & (agnoaas_cat==0)
***Fully-adjusted logistic regression using MICE
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.`v' i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if (ainesopi_cat ==0) & (aasbdt_cat==0) & (agnoaas_cat==0)
display "*************************************************************************"
}

****In the prior 1 year**
foreach v in est_ncomb_cat est_ncomb_durc5 {
display "`v'"
display "*************************************************************************"
tab `v' caso if (ainesopi_cat ==0| ainesopi_cat ==3) & (aasbdt_cat==0|aasbdt_cat==3) & (agnoaas_cat==0|agnoaas_cat==3), col
logistic caso edadfechaindice1 sexo añocalendario i.`v' if (ainesopi_cat ==0| ainesopi_cat ==3) & (aasbdt_cat==0|aasbdt_cat==3) & (agnoaas_cat==0|agnoaas_cat==3)
***Fully-adjusted logistic regression using MICE
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.`v' i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if (ainesopi_cat ==0)| (ainesopi_cat ==3) & (aasbdt_cat==0)|(aasbdt_cat==3) & (agnoaas_cat==0)|(agnoaas_cat==3)
display "*************************************************************************"
}

*******************************************************************************************************************************
*******************************************************************************************************************************

**FIGURE 2.Current use of statins and risk of colorectal cancer, by gender, age and body mass index (BMI)**

***By sex
foreach v in est_ncomb_cat {
display "`v'"
display "*************************************************************************"
bysort sexo: tab `v' caso, col
bysort sexo: logistic caso edadfechaindice1 sexo añocalendario i.`v'
***Fully-adjusted logistic regression using MICE
mi estimate, or: logistic caso edadfechaindice1 añocalendario i.`v' i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if sexo==0
mi estimate, or: logistic caso edadfechaindice1 añocalendario i.`v' i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if sexo==1
display "*************************************************************************"
}

***By Age
foreach v in est_ncomb_cat {
display "`v'"
display "*************************************************************************"
bysort edad70: tab `v' caso, col
bysort edad70: logistic caso edadfechaindice1 sexo añocalendario i.`v'
***Fully-adjusted logistic regression using MICE
mi estimate, or: logistic caso  añocalendario sexo i.est_ncomb_cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if edad70==0
mi estimate, or: logistic caso  añocalendario sexo i.est_ncomb_cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if edad70==1
display "*************************************************************************"
}

***By BMI
foreach v in est_ncomb_cat  {
display "`v'"
display "*************************************************************************"
bysort imc_cat: tab `v' caso, col
bysort imc_cat: logistic caso edadfechaindice1 sexo añocalendario i.`v'
***Fully-adjusted logistic regression using MICE
mi estimate, or esampvaryok : logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_cat i.n_visitas_cat_1y alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if imc_cat_imputed ==0
mi estimate, or esampvaryok : logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_cat i.n_visitas_cat_1y alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if imc_cat_imputed ==1
mi estimate, or esampvaryok : logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_cat i.n_visitas_cat_1y alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if imc_cat_imputed ==2
display "*************************************************************************"
}

*******************************************************************************************************************************
*******************************************************************************************************************************

**TABLE 3. Current use of individual statins and risk of colorectal cancer**

***Simvastatin
tab simva_s_cat caso if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.simva_s_cat if  ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.simva_s_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0

***Atorvastatin
tab ator_s_cat caso if simva_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.ator_s_cat if simva_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.ator_s_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if simva_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0

***Pravastatin
tab prav_s_cat caso if ator_s_cat==0 & simva_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0, col
logistic caso edadfechaindice1 sexo añocalendario i.prav_s_cat if ator_s_cat==0 & simva_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.prav_s_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & simva_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0

***Lovastatin
tab lova_cat caso if  ator_s_cat==0 & prav_s_cat==0 & simva_s_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.lova_cat if ator_s_cat==0 & prav_s_cat==0 & simva_s_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.lova_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & prav_s_cat==0 & simva_s_cat==0 & fluva_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0

***Fluvastatin
tab fluva_cat caso if  ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & simva_s_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.fluva_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & simva_s_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.fluva_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & simva_s_cat==0 & rosu_cat==0 & pita_cat==0 & ceriv_cat==0

***Rosuvastatin
tab rosu_cat caso if  ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & simva_s_cat==0 & pita_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.rosu_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & simva_s_cat==0 & pita_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.rosu_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & simva_s_cat==0 & pita_cat==0 & ceriv_cat==0

***Pitavastatin
tab pita_cat caso if  ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & simva_s_cat==0 & ceriv_cat==0,col
logistic caso edadfechaindice1 sexo añocalendario i.pita_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & simva_s_cat==0 & ceriv_cat==0
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.pita_cat i.n_visitas_cat_1y i.imc_cat_imputed  alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat if ator_s_cat==0 & prav_s_cat==0 & lova_cat==0 & fluva_cat==0 & rosu_cat==0 & simva_s_cat==0 & ceriv_cat==0

*******************************************************************************************************************************
*******************************************************************************************************************************

**TABLE 4. Current use of statins and risk of colorectal cancer, by intensity and lipophilicity **

***Intensity

****Any duration
tab est_ncomb_intens_3cat caso,col
logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_intens_3cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_intens_3cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat

****Continous duration
tab est_ncomb_intens_durc5 caso,col
logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_intens_durc5 logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_intens_durc5 i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat 
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_intens_durc5 i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat 

***Lipophilicity

****Any duration
tab est_ncomb_hl_cat caso,col
logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_hl_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_hl_cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat

***Continous duration
tab est_ncomb_hl_durc5 caso,col
logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_hl_durc5
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_hl_durc5 i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat i.antihypertensives_cat

*******************************************************************************************************************************
*******************************************************************************************************************************

**TABLE 5. Interactions between current use of statins and current use of cardiovascular drugs or nonsteroidal anti-inflammatory drugs (NSAIDs), and risk of colorectal cancer **

***Any duration
foreach v in alfab bbloq araii_s ieca_s ca_ant_s diub_ah aasbd clopis smet_bil_ezet fibr ainesopi {
display "********************************************************************************************"
display "`v'"
display "********************************************************************************************"
tab est_ncomb_`v'_int_cat caso,col
logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_`v'_int_cat
}

****Fully-adjusted logistic regression using MICE

*****Antihypertensives

mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_alfab_int_cat i.bbloq_cat i.araii_s_cat i.ieca_s_cat i.ca_ant_s_cat i.diub_ah_cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_bbloq_int_cat i.alfab_cat i.araii_s_cat i.ieca_s_cat i.ca_ant_s_cat i.diub_ah_cat i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_araii_s_int_cat i.alfab_cat i.bbloq_cat i.ieca_s_cat i.ca_ant_s_cat i.diub_ah_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_ieca_s_int_cat i.alfab_cat i.bbloq_cat i.araii_s_cat i.ca_ant_s_cat i.diub_ah_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_ca_ant_s_int_cat i.alfab_cat i.bbloq_cat i.araii_s_cat i.ieca_s_cat i.diub_ah_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_diub_ah_int_cat i.alfab_cat i.bbloq_cat i.araii_s_cat i.ieca_s_cat i.ca_ant_s_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat

*****Antiplatelet drugs

mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_aasbd_int_cat i.ainesopi_cat i.antihypertensives_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_clopis_int_cat i.dipiri_cat i.triflu_cat i.cilost_cat i.ticlopi_cat i.aasbd_cat2 i.ainesopi_cat i.antihypertensives_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat

*****Other lipid-modifying agents

mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_fibr_int_cat i.o_mod_li_cat i.smet_bil_cat i.agnoaas_cat2 i.aasbd_cat2 i.ainesopi_cat i.antihypertensives_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 
mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_smet_bil_ezet_int_cat  i.n_visitas_cat_1y i.imc_cat_imputed alc_ab_cat ib3.tabac_cat_m  g_croni_cat refluj_cat  eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.ainesopi_cat i.syst_cat2 i.ca_vitd_t_cat2 i.fibr_cat i.antihypertensives_cat

*****NSAIDs

mi estimate, or: logistic caso edadfechaindice1 sexo añocalendario i.est_ncomb_ainesopi_int_cat i.antihypertensives_cat i.n_visitas_cat_1y ib1.imc_cat alc_ab_cat ib3.tabac_cat g_croni_cat refluj_cat eii_cat c_irrit_cat estreñ_cat#i.aestreñ_cat p_anrect_cat i.pdig_s_cat hdb_cat i.hipgot_cat i.cortic_cat i.agnoaas_cat2 i.acot_cat2 i.adiabetics_cat i.ssri_sari_cat i.gastroprotectors_cat i.adiarr_cat i.aasbd_cat2 i.opios_cat2 i.opid_cat i.syst_cat2 i.ca_vitd_t_cat2 i.o_lip_low_cat


*******************************************************************************************************************************
*******************************************************************************************************************************




