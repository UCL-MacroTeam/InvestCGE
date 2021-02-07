***INVESTCGE
*14. MODEL DYNAMICS

***** scenario definition


set SIMT(T)    Simulation years
 /2009*2067/
;

set INVCAP(FCAP) Investment capital categories
/
         CAP-AGR Agricultural capital
         CAP-OTH General capital
/;

alias (INVCAP,INVCAPP)   ;

parameters       PFIN_TOT(FIN,APROJ)             Auxiliary variable for PFIN -- local currency
                 PLVALUE_TOT(APROJ)              Auxiliary variable for PLVALUE -- local currency
                 PCOST_TOT(AC,APROJ)             Auxiliary variable for PCOST -- local currency
                 REPAYMENT_AUX(APROJ,FIN)        Auxiliary variable for REPAYMENT -- local currency
                 PFIN_T(FIN,APROJ,T)             Project finance over time -- local currency
                 PROJEXP_T(APROJ,T)              Project expenditure over time - direct imports deducted  -- local currency
                 PLVALUE_T(APROJ,T)              Project investment over time -- local currency
                 PCOST_T(AC,APROJ,T)             Project cost over over time -- local currency
                 REPAYMENT_T(APROJ,FIN,T)        Repayment over time -- local currency
                 LOANPROD_T(APROJ,C,T)           Production for resource-secured loan
                 LOANPROD_AUX(APROJ,C)           Annual production for resource-secured loan
                 KSTOCK0                         Base year capital stock
                 KSTOCK_PREV                     Capital stock from previous model run
                 dKSTOCK                         Change in KSTOCK due to project investment
                 aINV                            KSTOCK to investment ratio      /11.5/
                 dep                             KSTOCK depreciation rate        /0.04/
                 GSAV_T(T)                       Government savings
                 PSAV_T(T)                       Total private savings
                 FSAV_T(T)                       Foreign savings
                 GADJ_T(T)                       Government spending adjustement
                 EG_T(T)                         Government expenditure
                 YG_T(T)                         Government income
                 YTAX_T(T)                       Government tax income
                 YGPROJ_T(T)                     Government factor income from project operation
                 EH_T(H,T)                       Household expenditure
                 YI_T(INSDNG,T)                  Household income
                 YIF_T(INSDNG,F,T)               Household income by factor
                 CPI_T(T)                        Consumer price index
                 INVSHR_T(T)                     Investment share
                 TABS_T(T)                       Total absorbtion over time
                 GOVSHR_T(T)                     Government spending share of total absorbtion
                 YF_T(F,T)                       Factor income
                 PA_T(A,T)                       Output price of activity
                 PE_T(C,T)                       Export price in local currency
                 PD_T(C,T)                       Domestic price of domestic commodity
                 PX_T(C,T)                       Output price of domestic commodity
                 PXAC_T(A,C,T)                   Domestic price of commodity
                 PQ_T(C,T)                       Consumer price of commodity
                 QA_T(A,T)                       Activity output level
                 QD_T(C,T)                       Domestic supply of commodity
                 QX_T(C,T)                       Domestic production of commodity
                 QM_T(C,T)                       Imported quantity
                 QE_T(C,T)                       Exported quantity
                 QINV_T(C,T)                     Investment demand of commodity
                 QINVTOT_T(T)                    Total investment demand
                 WF_T(F,T)                       Factor rent
                 EXR_T(T)                        Exchange rate local currency to international currency
                 WALRAS_T(T)                     Walras - savings-investment imbalance
                 QG_T(C,T)                       Government demand of commodity
                 QH_T(C,H,T)                     Household demand of commodity
                 MPS_T(INS,T)                    Marginal savings propensity
                 IADJ_T(T)                       Investment spending adjustment
                 QFS_T(F,T)                      Factor supply
                 QF_T(F,AC,T)                    Factor demand by activity
                 QINT_T(C,AC,T)                  Intermediate demand by activity
                 QFSPROJ_T(F,APROJ,T)            Factor supply by project
                 INSDNG_INCOME_T(INSDNG,*,T)     Private institutions income by source
                 pop_delta(T)                    Active population change
                 ror(INVCAP)     relative rate of return of INVCAP
                 delta_qfs(INVCAP)       change in capital stock of INVCAP
*                 rebase_qfs(INVCAP)       rebasing changes in capital stock of INVCAP
                 rebase_qfs       rebasing changes in capital stock of INVCAP
                 delta_stock     change in total general capital stock to be allocated across INVCAP
;


* add investment project with pre-defined cost breakdown

$if not set pvalue_A $set pvalue_A 0
$if not set interest_A $set interest_A 0.02
$if not set repayper_A $set repayper_A 20
$if not set loanprod_A $set loanprod_A 0
$if not set fin_gspend_A $set fin_gspend_A 0
$if not set fin_gsave_A $set fin_gsave_A 0
$if not set fin_gbor_A $set fin_gbor_A 0
$if not set fin_fbor_A $set fin_fbor_A 0
$if not set fin_grant_A $set fin_grant_A 0

$if not set pvalue_B $set pvalue_B 0
$if not set interest_B $set interest_B 0.045
$if not set repayper_B $set repayper_B 12
$if not set loanprod_B $set loanprod_B 0
$if not set fin_gspend_B $set fin_gspend_B 0
$if not set fin_gsave_B $set fin_gsave_B 0
$if not set fin_gbor_B $set fin_gbor_B 0
$if not set fin_fbor_B $set fin_fbor_B 0
$if not set fin_grant_B $set fin_grant_B 0

$if not set flabel $set flabel BASE
$if not set base $set base 1
$if not set results_dir $set results_dir "results/"

 sPFIN(FIN,APROJ)        = 0       ;

 PVALUE("A-PROJ")        = %pvalue_A%      ;
 PVALUE("B-PROJ")        = %pvalue_B%      ;

 PLVALUE_TOT(APROJ)      = PVALUE(APROJ) * REXR   ;

 sPFIN("GCSPEND","A-PROJ")        = %fin_gspend_A% ;
 sPFIN("GSSPEND","A-PROJ")        = %fin_gsave_A% ;
 sPFIN("GBOR","A-PROJ")           = %fin_gbor_A% ;
 sPFIN("FBOR","A-PROJ")           = %fin_fbor_A% ;
 sPFIN("GRANT","A-PROJ")          = %fin_grant_A% ;

 sPFIN("GCSPEND","B-PROJ")        = %fin_gspend_B% ;
 sPFIN("GSSPEND","B-PROJ")        = %fin_gsave_B% ;
 sPFIN("GBOR","B-PROJ")           = %fin_gbor_B% ;
 sPFIN("FBOR","B-PROJ")           = %fin_fbor_B% ;
 sPFIN("GRANT","B-PROJ")          = %fin_grant_B% ;


* default financing if all financing sources are set to zero
 sPFIN("GRANT",APROJ)$(not SUM(FIN,sPFIN(FIN,APROJ)))
                                 =       1 ;
 PCOST_TOT(AC,APROJ)$sPCOST(AC,APROJ)
                                 =       sPCOST(AC,APROJ)*PLVALUE_TOT(APROJ) ;

 PFIN_TOT(FIN,APROJ)             =       sPFIN(FIN,APROJ)*PLVALUE_TOT(APROJ) ;

 INVSTART(APROJ)         =       1 ;
 CONSPER(APROJ)          =       5 ;
 GRACEPER(APROJ,FIN)     =       0 ;

 REPAYPER("A-PROJ",FIN)  =       %repayper_A% ;
 REPAYPER("B-PROJ",FIN)  =       %repayper_B% ;

* default interest rate for government bonds
 r(APROJ,"GBOR")         =       0.025 ;


 r("A-PROJ","FBOR")      =       %interest_A% ;
 r("B-PROJ","FBOR")      =       %interest_B% ;



$gdxin ghana_population.gdx
$load pop_delta

 REPAYMENT_AUX(APROJ,FIN)$(PFIN_TOT(FIN,APROJ) and r(APROJ,FIN))
         =       ((1+r(APROJ,FIN))**GRACEPER(APROJ,FIN)*PFIN_TOT(FIN,APROJ)) * r(APROJ,FIN) /
                 (1-(1+r(APROJ,FIN))**(-REPAYPER(APROJ,FIN)))    ;

* loan-related exports
** total demand annual demand

 LOANPROD_AUX("A-PROJ","C-AGR")   = %loanprod_A%*REXR        ;

** export price reflective of market price in the base year
 LOANPRODPRICE("A-PROJ","C-AGR")  = 1             ;


* REPAYMENT_AUX(APROJ,FIN)=       REPAYMENT(APROJ,FIN) ;
 PFIN(FIN,APROJ) =       0       ;
 PLVALUE(APROJ)  =       0       ;
 REPAYMENT(APROJ,FIN)=   0       ;
 PCOST(AC,APROJ) =       0       ;

 KSTOCK0         =       aINV*SUM(C,QINV0(C))      ;
 KSTOCK_PREV     =       KSTOCK0 ;

* reading operation performance values

set run /RUN1*RUN50/ ;

parameter hydro_output(*,T,run)
          delta_hydro_output(*,T,run)  ;

$gdxin hydro_output.gdx
$load hydro_output=h_output, delta_hydro_output=delta_h_output


* starting multi-annual simulations
loop(SIMT,

 PFIN(FIN,APROJ) =       0       ;
 PLVALUE(APROJ)  =       0       ;
 REPAYMENT(APROJ,FIN)=   0       ;
 LOANPROD(APROJ,C)     =       0       ;
 PCOST(AC,APROJ) =       0       ;

 PLVALUE(APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))     =       PLVALUE_TOT(APROJ)/CONSPER(APROJ)      ;
 PFIN(FIN,APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))    =       PFIN_TOT(FIN,APROJ)/CONSPER(APROJ)      ;
 PCOST(AC,APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))    =       PCOST_TOT(AC,APROJ)/CONSPER(APROJ)      ;

* exports started in 2010
 LOANPROD(APROJ,C)$(ord(SIMT)> 1 AND(ord(SIMT)<(1+REPAYPER(APROJ,"FBOR"))))    =       LOANPROD_AUX(APROJ,C)      ;
 LOANPROD(APROJ,C)$(NOT PFIN_TOT("FBOR",APROJ)) =        0;

* setting up annual repayment values, activated after the project construction + grace period is ended and finished at the end of the repayment period
 REPAYMENT(APROJ, FIN)$(ord(SIMT)>=(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,FIN)) AND(ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,FIN)+REPAYPER(APROJ,FIN))))    =       REPAYMENT_AUX(APROJ,FIN)      ;

* scenarios with
if(%base%=0,
$if not set climate $set climate "run1"

         QFSPROJ.FX("CAP-HY","A-PROJ") = QFS0("CAP-HY")*(0 + delta_hydro_output("bui",SIMT,"%climate%")*delta_hydro_output("akosombo_base",SIMT,"%climate%")  )     ;
         QFS.FX("CAP-HY") = QFS0("CAP-HY")*(0 + delta_hydro_output("akosombo",SIMT,"%climate%")*delta_hydro_output("akosombo_base",SIMT,"%climate%")  )     ;

* un-comment below for average Bui hydropower output based on hydrological sequences
*        QFSPROJ.FX("CAP-HY","A-PROJ")$(ORD(SIMT)>8)   = QFS0("CAP-HY")*(0 + 0.249$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;
* un-comment below for average Akosombo hydropower output based on hydrological sequences
*        QFS.FX("CAP-HY")$(ORD(SIMT)>8)                = QFS0("CAP-HY")*(1 + 0.044$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;

 );

if(%base%=1,
$if not set climate $set climate "run1"

    QFS.FX("CAP-HY")$delta_hydro_output("akosombo_base",SIMT,"%climate%")  = QFS0("CAP-HY")*(0 + delta_hydro_output("akosombo_base",SIMT,"%climate%") )     ;

* un-comment below for average baseline hydropower output based on hydrological sequences
*    QFS.FX("CAP-HY")$(ORD(SIMT)>8)  = 1.0084*QFS0("CAP-HY")     ;

 );


 SOLVE INVESTCGE USING MCP ;

*** results at annual steps

 PLVALUE_T(APROJ,SIMT)           = PLVALUE(APROJ)  ;
 PFIN_T(FIN,APROJ,SIMT)          = PFIN(FIN,APROJ) ;
 PCOST_T(AC,APROJ,SIMT)          = PCOST(AC,APROJ) ;
 LOANPROD_T(APROJ,C,SIMT)        = LOANPROD(APROJ,C) ;
 REPAYMENT_T(APROJ,FIN,SIMT)     = REPAYMENT(APROJ,FIN) ;

 GSAV_T(SIMT)            =       GSAV.L ;
 PSAV_T(SIMT)            =       PSAV.L ;
 FSAV_T(SIMT)            =       FSAV.L ;
 GADJ_T(SIMT)            =       GADJ.L ;
 EG_T(SIMT)              =       EG.L ;
 YG_T(SIMT)              =       YG.L ;
 YTAX_T(SIMT)            =       YTAX.L     ;
 YGPROJ_T(SIMT)          =       YGPROJ.L ;
 EH_T(H,SIMT)            =       EH.L(H) ;
 YI_T(INSDNG,SIMT)       =       YI.L(INSDNG) ;
 YIF_T(INSDNG,F,SIMT)    =       YIF.L(INSDNG,F) ;
 CPI_T(SIMT)             =       CPI.L ;
 INVSHR_T(SIMT)          =       INVSHR.L ;
 GOVSHR_T(SIMT)          =       GOVSHR.L ;
 YF_T(F,SIMT)            =       YF.L(F) ;
 PA_T(A,SIMT)            =       PA.L(A) ;
 QA_T(A,SIMT)            =       QA.L(A) ;
 QX_T(C,SIMT)            =       QX.L(C) ;
 QD_T(C,SIMT)            =       QD.L(C) ;
 PQ_T(C,SIMT)            =       PQ.L(C) ;
 QINV_T(C,SIMT)          =       QINV.L(C) ;
 QINVTOT_T(SIMT)         =       QINVTOT.L ;
 WF_T(F,SIMT)            =       WF.L(F) ;
 EXR_T(SIMT)             =       EXR.L ;
 WALRAS_T(SIMT)          =       WALRAS.L   ;
 QG_T(C,SIMT)            =       QG.L(C) ;
 QH_T(C,H,SIMT)          =       QH.L(C,H) ;
 MPS_T(INS,SIMT)         =       MPS.L(INS) ;
 IADJ_T(SIMT)            =       IADJ.L ;
 QFS_T(F,SIMT)           =       QFS.L(F) ;
 QM_T(C,SIMT)            =       QM.L(C)  ;
 QE_T(C,SIMT)            =       QE.L(C)  ;
 PE_T(C,SIMT)            =       PE.L(C) ;
 PD_T(C,SIMT)            =       PDD.L(C) ;
 PX_T(C,SIMT)            =       PX.L(C) ;
 PXAC_T(A,C,SIMT)        =       PXAC.L(A,C)  ;
 QFSPROJ_T(F,APROJ,SIMT) =       QFSPROJ.L(F,APROJ) ;
 PROJEXP_T(APROJ,SIMT)   =       PROJEXP.L(APROJ)        ;
 QF_T(F,A,SIMT)          =       QF.L(F,A)  ;
 QF_T(F,APROJ,SIMT)      =       QFPROJ.L(F,APROJ) ;
 QINT_T(C,A,SIMT)        =       QINT.L(C,A) ;
 QINT_T(C,APROJ,SIMT)    =       QINTPROJ.L(C,APROJ) ;

 INSDNG_INCOME_T(INSDNG,F,SIMT)           =       YIF.L(INSDNG,F) ;
 INSDNG_INCOME_T(INSDNG,INSDNGP,SIMT)     =       TRII.L(INSDNG,INSDNGP) ;
 INSDNG_INCOME_T(INSDNG,'GOV',SIMT)       =       trnsfr(INSDNG,'GOV')*CPI.L;
 INSDNG_INCOME_T(INSDNG,'ROW',SIMT)       =       trnsfr(INSDNG,'ROW')*EXR.L;
 INSDNG_INCOME_T(INSDNG,'TOTAL',SIMT)     =       SUM(F,INSDNG_INCOME_T(INSDNG,F,SIMT) ) + SUM(INSDNGP,INSDNG_INCOME_T(INSDNG,INSDNGP,SIMT)) +  INSDNG_INCOME_T(INSDNG,'GOV',SIMT) + INSDNG_INCOME_T(INSDNG,'ROW',SIMT) ;

* ROR of different investment capital categories
 ror(INVCAP)             =       WF.L(INVCAP) / (SUM(INVCAPP, QFS.L(INVCAPP) * WF.L(INVCAPP)) /SUM(INVCAPP, QFS.L(INVCAPP)))       ;
* total new capital stock after depreciation
 delta_stock             =       (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV    ;
* ROR-based allocation of capital stock to investment capital categories
 delta_qfs(INVCAP)       =       delta_stock * ror(INVCAP)**0.5 * QFS.L(INVCAP)       ;

 rebase_qfs      =       1 ;

 QFS.FX(INVCAP)  =       delta_qfs(INVCAP) * rebase_qfs  ;

 QFS0(FLAB)      =       pop_delta(SIMT) * QFS0(FLAB)     ;

* increase in effective land supply to account for general yield improvements
 QFS0("LAND")    =       QFS0("LAND")*1.02  ;

* calculating new capital stock for the next time step
 KSTOCK_PREV     =       SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV ;

) ;



$if not set climate $set climate ""


 execute_unload "%results_dir%bui_results_%flabel%_%climate%.gdx" SAM, r, C,A,F,H,AC,INSDNG, PLVALUE_T, PROJEXP_T, sPCOST, sPFIN, PCOST_T, PFIN_T, REPAYMENT_T, LOANPROD_T, GSAV_T, PSAV_T, FSAV_T, EG_T, YG_T, YTAX_T, YGPROJ_T, EH_T, gammam, betam, gammah, betah YI_T, YIF_T, CPI_T, INVSHR_T, GOVSHR_T, YF_T, PA_T, QA_T, QX_T, QD_T, QM_T, QE_T, PQ_T, PE_T, PD_T, PX_T, PXAC_T, QINV_T, QINVTOT_T, WF_T, EXR_T, WALRAS_T, QG_T, PFIN_T, MPS_T, IADJ_T, GADJ_T, QF_T, QFS_T, QFSPROJ_T, QINT_T, INSDNG_INCOME_T
