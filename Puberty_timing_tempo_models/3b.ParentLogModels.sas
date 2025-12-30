libname pub 'C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data';
/***************************************/
	/* Girls' PDS parent reported */					
/***************************************/
proc import datafile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\pgirlsPDSclean_251230.dta"
    out=pub.pfPDS
    dbms=stata replace;
run;
proc contents data=pub.pfPDS;
run;
proc means n mean stddev data=pub.pfPDS; var age pfPDSm;
run;
PROC NLMIXED DATA = pub.pfPDS METHOD=FIRO;
	TITLE1 'Logistic model for girls PDS scores (parent reported)';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL pfPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=pub.pfPDS_parm_nlmix_251230;
	PARMS 	m_a = .95             /*mean tempo*/
			m_lambda = 10        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.pfPDS_nlinmix_out_251230; 
	ODS OUTPUT FitStatistics=pub.pfPDS_idlogifit_251230 ParameterEstimates=pub.pfPDS_idlogiparms_251230;
RUN;
/*
Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.5658 0.003044 5484 185.84 <.0001 0.5598 0.5717 0.14338 
m_lambda 11.9794 0.01699 5484 705.17 <.0001 11.9461 12.0127 0.051662 
v_e 0.08723 0.000859 5484 101.50 <.0001 0.08555 0.08892 -0.31398 
v_lambda 0.03600 0.001220 5484 29.51 <.0001 0.03361 0.03839 -0.32834 
cov_alam 0.05977 0.004381 5484 13.64 <.0001 0.05119 0.06836 -0.32173 
v_a 1.3511 0.03044 5484 44.39 <.0001 1.2915 1.4108 -0.01056 
*/
*Cleaning up outputfile containing estimates of individual parameters;
PROC SORT DATA=pub.pfPDS_parm_nlmix_251230;
	BY ID;
DATA pub.pfPDS_parm_nlmix_251230;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET pub.pfPDS_parm_nlmix_251230;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.5658+estimate;
	IF effect = 'lambda' THEN parm[2] = 11.9794+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.pfPDS_parm_nlmix_251230
    outfile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\girls_parentPDS_logisticmodels_251230.dta"
    dbms=stata replace;
run;
/***************************************/
/* Boys' PDS parent reported */					
/***************************************/
proc import datafile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\pboysPDSclean_251230.dta"
    out=pub.pmPDS
    dbms=stata replace;
run;
proc contents data=pub.pmPDS;
run;
proc means n mean stddev data=pub.pmPDS; var age pmPDSm;
run;
PROC NLMIXED DATA = pub.pmPDS METHOD=FIRO;
	TITLE1 'Logistic model for boys PDS scores (parent reported)';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL pmPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=pub.pmPDS_parm_nlmix_251230;
	PARMS 	m_a = 1             /*mean tempo*/
			m_lambda = 13        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.pmPDS_nlinmix_out_251230; 
	ODS OUTPUT FitStatistics=pub.pmPDS_idlogifit_251230 ParameterEstimates=pub.pmPDS_idlogiparms_251230;
RUN;
/*
Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.4853 0.002702 6022 179.61 <.0001 0.4800 0.4906 -0.13117 
m_lambda 13.9950 0.01704 6022 821.10 <.0001 13.9616 14.0284 -0.00482 
v_e 0.09366 0.000880 6022 106.40 <.0001 0.09194 0.09539 0.38867 
v_lambda 0.02602 0.000959 6022 27.13 <.0001 0.02414 0.02790 -0.07819 
cov_alam 0.01658 0.003785 6022 4.38 <.0001 0.009161 0.02400 -0.05736 
v_a 1.3333 0.03216 6022 41.45 <.0001 1.2702 1.3963 -0.01035 
*/
*Cleaning up outputfile containing estimates of individual parameters;
PROC SORT DATA=pub.pmPDS_parm_nlmix_251230;
	BY ID;
DATA pub.pmPDS_parm_nlmix_251230;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET pub.pmPDS_parm_nlmix_251230;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.4853+estimate;
	IF effect = 'lambda' THEN parm[2] = 13.9950+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.pmPDS_parm_nlmix_251230
    outfile="C:\Users\nchaku\Documents\GitHub\pubertybrain\Puberty_timing_tempo_models\Working data\boys_parentPDS_logisticmodels_251230.dta"
    dbms=stata replace;
run;
