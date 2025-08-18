libname pub 'Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels';
proc import datafile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\ygirlsclean.dta"
    out=pub.CHILD
    dbms=stata replace;
run;
proc contents data=pub.CHILD;
run;
proc means n mean stddev data=pub.CHILD; var age yfPDSm;
run;
/***************************************/
	/* Girls' PDS youth reported */					
/***************************************/
PROC NLMIXED DATA = pub.CHILD METHOD=FIRO;
	TITLE1 'Logistic model for girls PDS scores - child reported';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL yfPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=parm_nlmix;
	PARMS 	m_a = .95             /*mean tempo*/
			m_lambda = 10        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.childnlinmix_out; 
	ODS OUTPUT FitStatistics=pub.girlschildPDSidlogifit ParameterEstimates=pub.girlschildPDSidlogiparms;
RUN;
/*
Fit Statistics 
-2 Log Likelihood 17239 
AIC (smaller is better) 17251 
AICC (smaller is better) 17251 
BIC (smaller is better) 17290 

Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.6942 0.003726 4551 186.28 <.0001 0.6869 0.7015 -0.07772 
m_lambda 12.5304 0.01646 4551 761.09 <.0001 12.4981 12.5627 0.001147 
v_e 0.06649 0.000846 4551 78.63 <.0001 0.06483 0.06815 -0.13842 
v_lambda 0.04704 0.002025 4551 23.23 <.0001 0.04307 0.05101 -0.10191 
cov_alam 0.05983 0.005471 4551 10.94 <.0001 0.04910 0.07055 -0.03391 
v_a 1.0524 0.02711 4551 38.83 <.0001 0.9993 1.1056 -0.01829 
*/
PROC PRINT DATA=pub.girlschildPDSidlogiparms; RUN;
*Cleaning up outpufile containing estimates of individual parameters;
PROC SORT DATA=parm_nlmix;
	BY ID;
DATA pub.girlschildPDSparm_nlmix;
    SET parm_nlmix;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET parm_nlmix;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.5778+estimate;
	IF effect = 'lambda' THEN parm[2] = 11.6840+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.girlschildPDSparm_nlmix
    outfile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\girls_childPDS_logisticmodels_250818.dta"
    dbms=stata replace;
run;
/***************************************/
	/* BOYS NOW */					
/***************************************/
proc import datafile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\yboysclean.dta"
    out=pub.CHILD
    dbms=stata replace;
run;
proc contents data=pub.CHILD;
run;
/***************************************/
	/* Boys' PDS - child reported */					
/***************************************/
proc means n mean stddev data=pub.CHILD; var age ymPDSm;
run;
PROC NLMIXED DATA = pub.CHILD METHOD=FIRO;
	TITLE1 'Logistic model for boys PDS scores - child reported';
	*Model;
	g0 = 1;			*lowest level = 1 for all persons;
	g1 = 3;			*additional gain = 3 for all persons;
	traject = g0 + g1/(1 + exp(-a*(age-lambda)));
	MODEL ymPDSm ~ NORMAL(traject, v_e);
	RANDOM a lambda ~ NORMAL([m_a, m_lambda], [v_lambda, 
											 	    cov_alam /*cov_alam*/, v_a ]) 
	SUBJECT = ID OUT=parm_nlmix;
	PARMS 	m_a = .3             /*mean tempo*/
			m_lambda = 11        /*mean timing*/
			v_e = 1.0 
			v_lambda = 1.0
			cov_alam = .3
			v_a = 1.5;
	PREDICT traject OUT=pub.childnlinmix_out; 
	ODS OUTPUT FitStatistics=pub.boyschildPDSidlogifit ParameterEstimates=pub.boyschildPDSidlogiparms;
RUN;
/*
Fit Statistics 
-2 Log Likelihood 16506 
AIC (smaller is better) 16518 
AICC (smaller is better) 16518 
BIC (smaller is better) 16557 

Parameter Estimates 
Parameter Estimate Standard
Error DF t Value Pr > |t| 95% Confidence Limits Gradient 
m_a 0.5545 0.003494 4851 158.73 <.0001 0.5477 0.5614 0.088438 
m_lambda 14.1318 0.01674 4851 843.96 <.0001 14.0990 14.1646 -0.00323 
v_e 0.06335 0.000846 4851 74.89 <.0001 0.06169 0.06501 0.75407 
v_lambda 0.03801 0.001643 4851 23.13 <.0001 0.03479 0.04124 -0.28787 
cov_alam 0.04608 0.004717 4851 9.77 <.0001 0.03684 0.05533 -0.15249 
v_a 1.1228 0.02944 4851 38.14 <.0001 1.0651 1.1806 0.022717 
*/

PROC PRINT DATA=pub.boyschildPDSidlogiparms; RUN;
*Cleaning up outpufile containing estimates of individual parameters;
PROC SORT DATA=parm_nlmix;
	BY ID;
DATA pub.boyschildPDSparm_nlmix;
    SET parm_nlmix;
	ARRAY parm[2] p_a p_lambda;
	RETAIN p_a p_lambda;
	SET parm_nlmix;
	BY ID;
	IF effect = 'a' THEN parm[1] = 0.3414+estimate;
	IF effect = 'lambda' THEN parm[2] = 13.7742+estimate;
	IF LAST.id THEN OUTPUT;
	KEEP ID p_a p_lambda;
RUN;
proc export data=pub.boyschildPDSparm_nlmix
    outfile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\boys_childPDS_logisticmodels_250826.dta"
    dbms=stata replace;
run;
