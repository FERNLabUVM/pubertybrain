libname pub 'C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data';
/***************************************/
	/* Girls' PDS youth reported */					
/***************************************/
proc import datafile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\ygirlsPDSclean_251230.dta"
    out=pub.yfPDS
    dbms=stata replace;
run;
proc contents data=pub.yfPDS;
run;
proc means n mean stddev data=pub.yfPDS; var age yfPDSm;
run;
PROC NLMIXED DATA = pub.yfPDS METHOD=FIRO;
	TITLE1 'Logistic model for girls PDS scores (child reported)';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL yfPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=pub.yfPDS_parm_nlmix_251230;
	PARMS 	m_a = .95             /*mean tempo*/
			m_lambda = 10        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.yfPDS_nlinmix_out_251230; 
	ODS OUTPUT FitStatistics=pub.yfPDS_idlogifit_251230 ParameterEstimates=pub.yfPDS_idlogiparms_251230;
RUN;
/*
Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.5925 0.003244 5468 182.64 <.0001 0.5861 0.5989 -0.01111 
m_lambda 12.2179 0.01562 5468 782.10 <.0001 12.1873 12.2486 0.002224 
v_e 0.1137 0.001127 5468 100.87 <.0001 0.1115 0.1159 -0.32737 
v_lambda 0.03369 0.001332 5468 25.29 <.0001 0.03108 0.03630 -0.62947 
cov_alam 0.03851 0.004153 5468 9.27 <.0001 0.03037 0.04665 -0.07047 
v_a 1.0871 0.02552 5468 42.60 <.0001 1.0371 1.1371 0.004398 

*/
*Cleaning up outputfile containing estimates of individual parameters;
PROC SORT DATA=pub.yfPDS_parm_nlmix_251230;
	BY ID;
DATA pub.yfPDS_parm_nlmix_251230;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET pub.yfPDS_parm_nlmix_251230;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.5925+estimate;
	IF effect = 'lambda' THEN parm[2] = 12.2179+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.yfPDS_parm_nlmix_251230
    outfile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\girls_childPDS_logisticmodels_251230.dta"
    dbms=stata replace;
run;
/***************************************/
/* Boys' PDS youth reported */					
/***************************************/
proc import datafile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\yboysPDSclean_251230.dta"
    out=pub.ymPDS
    dbms=stata replace;
run;
proc contents data=pub.ymPDS;
run;
proc means n mean stddev data=pub.ymPDS; var age ymPDSm;
run;
PROC NLMIXED DATA = pub.ymPDS METHOD=FIRO;
	TITLE1 'Logistic model for boys PDS scores (child reported)';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL ymPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=pub.ymPDS_parm_nlmix_251230;
	PARMS 	m_a = 1             /*mean tempo*/
			m_lambda = 13        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.ymPDS_nlinmix_out_251230; 
	ODS OUTPUT FitStatistics=pub.ymPDS_idlogifit_251230 ParameterEstimates=pub.ymPDS_idlogiparms_251230;
RUN;
/*
Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.3908 0.002604 6022 150.07 <.0001 0.3857 0.3959 -0.12714 
m_lambda 13.9929 0.01869 6022 748.60 <.0001 13.9562 14.0295 -0.00244 
v_e 0.1369 0.001280 6022 107.00 <.0001 0.1344 0.1395 0.17061 
v_lambda 0.01963 0.000803 6022 24.45 <.0001 0.01805 0.02120 -0.18038 
cov_alam -0.01622 0.003912 6022 -4.15 <.0001 -0.02389 -0.00855 -0.00827 
v_a 1.4595 0.04053 6022 36.01 <.0001 1.3800 1.5390 0.006841 
*/
*Cleaning up outputfile containing estimates of individual parameters;
PROC SORT DATA=pub.ymPDS_parm_nlmix_251230;
	BY ID;
DATA pub.ymPDS_parm_nlmix_251230;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET pub.ymPDS_parm_nlmix_251230;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.3908+estimate;
	IF effect = 'lambda' THEN parm[2] = 13.9929+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.ymPDS_parm_nlmix_251230
    outfile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\boys_childPDS_logisticmodels_251230.dta"
    dbms=stata replace;
run;
