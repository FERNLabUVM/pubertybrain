libname pub 'Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels';
proc import datafile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\ygirlsclean.dta"
    out=pub.CHILD
    dbms=stata replace;
run;
proc contents data=pub.CHILD;
run;
proc means n mean stddev data=pub.CHILD; var age yfPDSm;
run;
*Based on Ram & Grimm (2007, International Journal of Behavioral Development);
/* 1) Fit linear growth model (girls) */
proc nlmixed data=pub.CHILD;
    title1 "Linear Growth Model – girls youth report";
    maxiter = 5000;

    /* Trajectory */
    traject = g0 + g1*age;

    /* Likelihood */
    model yfPDSm ~ normal(traject, v_e);

    /* Random effects with subject-level BLUPs to OUT= */
    random g0 g1 ~ normal([m_g0, m_g1], [v_g0, c_g0g1, v_g1])
        subject=ID
        out=pub.girlsPDS_out_indlinear;

    /* Starting values */
    parms m_g0 = 1.8
          m_g1 = 0.5
          v_e  = 0.5
          v_g0 = 0.5
          c_g0g1 = 0
          v_g1 = 0.5;

    /* Predicted scores (optional) */
    predict traject out=pub.girlsPDS_out_linepredscore;

    ods output FitStatistics=pub.girlsPDS_fit;
run;
proc import datafile="Z:\Projects\Data\ABCD\Syntax\Puberty\PubertyGrowthModels\yboysclean.dta"
    out=pub.CHILD
    dbms=stata replace;
run;
proc contents data=pub.CHILD;
run;
proc means n mean stddev data=pub.CHILD; var age ymPDSm;
run;
*Based on Ram & Grimm (2007, International Journal of Behavioral Development);
/* 1) Fit linear growth model (girls) */
proc nlmixed data=pub.CHILD;
    title1 "Linear Growth Model – boys youth report";
    maxiter = 5000;

    /* Trajectory */
    traject = g0 + g1*age;

    /* Likelihood */
    model ymPDSm ~ normal(traject, v_e);

    /* Random effects with subject-level BLUPs to OUT= */
    random g0 g1 ~ normal([m_g0, m_g1], [v_g0, c_g0g1, v_g1])
        subject=ID
        out=pub.boysPDS_out_indlinear;

    /* Starting values */
    parms m_g0 = 2
          m_g1 = 0.5
          v_e  = 0.5
          v_g0 = 0.5
          c_g0g1 = 0
          v_g1 = 0.5;

    /* Predicted scores (optional) */
    predict traject out=pub.boysPDS_out_linepredscore;

    ods output FitStatistics=pub.boysPDS_fit;
run;
