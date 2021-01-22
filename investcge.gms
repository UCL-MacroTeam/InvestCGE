$TITLE InvestCGE model, version 1 (April 2020)
$STITLE Input file: investcge.gms. A modified CGE model to include investment, construction and operation of country-scale projects

$ontext


InvestCGE: Modifications to the IFPRI/TMD Standard CGE Model made by Victor Nechifor,
Alvaro Calzadilla and Mohammed Beshir with the assistance of Julien Harou and Emmanuel Obuobie
and documented in:

Nechifor, V., Basheer, M., Calzadilla, A., Harou, J. (under review). Economy-wide impacts of financing national scale
energy projects in developing countries - Application to Ghana's Bui Dam. Energy Economics

Modification to the original IFPRI/TMD model are labeled as follows:
*** INVESTCGE
 Added or modified model code
*** /INVESTCGE

Copyright (c) 2020, University College London Institute for Sustainable Resources

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public Licence, version 2, as
published by the Free Software Foundation.

This progrm is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public Licence for more details.

--------------------------------------------------------------------------------
This file is the core model file for the IFPRI/TMD Standard
CGE Model, documented in:

Lofgren, Hans, Rebecca Lee Harris, and Sherman Robinson, with the
assistance of Moataz El-Said and Marcelle Thomas. 2002. A Standard
Computable General Equilibrium (CGE) Model in GAMS. Microcomputers in
Policy Research, Vol. 5. Washington, D.C.: IFPRI.

Copyright (c) 2002, International Food Policy Research Institute (IFPRI),
Washington, DC.

For additional information on the model and the GAMS files, see
also the file README.TXT.

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public Licence, version 2, as
published by the Free Software Foundation.

This progrm is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
General Public Licence for more details.

Under the GNU General Public Licence, permission is granted to anyone to
use this software for any purpose, including commercial applications,
and to alter it and redistribute it freely, subject to the following
restrictions:
(1) The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowldgement in the product documentation would be
    appreciated.
(2) Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.
(3) This notice may not be removed or altered from any source distribution.

See the GNU General Public Licence for more details. A copy of the GNU
General Public Licence may be obtained from:
  Free Software Foundation, Inc.
  59 Temple Place, Suite 330
  Boston, MA 02111-1307

or from their website: http://www.gnu.org/copyleft/gpl.html

Only experienced users should make changes in files other than the
country-specific data sets. If changes are made, it is good modeling
practice to carefully document these so well that another user (or the
original user, one year later) can understand what was done.

The file MOD.GMS consists of the following segments (with
searchable headings):

1. SET DECLARATIONS

All sets are declared. (No sets are defined here).


2. DATABASE

SAM parameter is declared. Include file with data for a selected country
is read in.

In this file, the SAM structure is adjusted to fit the model structure.
Error messages alert the user to missing data. The user has the option
activating automatic rescaling of SAM and physical factor quantity data.


3. PARAMETER DECLARATIONS

All model parameters (including those used to initialize variables)
are DECLARED.


4. PARAMETER DEFINITIONS

All model parameters (including those used to initialize variables)
are DEFINED.


5. VARIABLE DECLARATIONS

All model variables are declared.


6. VARIABLE DEFINITIONS

All model variables are initialized.


7. EQUATION DECLARATIONS

The model equations are declared. They are divided into the following
four blocks: "Price block", "Production and trade block", "Institution
block", and "System constraint block".


8. EQUATION DEFINITIONS

The model equations are defined. They are divided into the same four
blocks as under 7.


9. MODEL DEFINITION

The model (a set of equations most of which are linked to variables)
is defined.


10. FIXING VARIABLES NOT IN MODEL AT ZERO

11. MODEL CLOSURE

Variables not included in the model are fixed. Macro and micro closure
rules are selected by fixing selected variables.


12. DISPLAY OF MODEL PARAMETERS AND VARIABLES

All model parameters and variables (initial levels) are displayed in
alphabetical order.


13. SOLUTION STATEMENT

The model is solved for the base with some optional tests of robustness


14. OPTIONAL NLP MODEL DEFINITION AND SOLUTION STATEMENT

Define a model that can be solved using a nonlinear programming (NLP)
solver.

15. SOLUTION REPORTS

$offtext


$ONSYMLIST ONSYMXREF OFFUPPER
*$OFFSYMLIST OFFSYMXREF
$ONEMPTY
*The dollar control option makes empty data initialization statements
*permissible (e.g. sets without elements or parameters without data)


*1. SET DECLARATIONS ################################################

$ontext

In this section, all sets are declared. They are divided into the
following groups:
a. model sets (appearing in the model equations)
b. calibration sets (used to initialize variables and define model
   parameters)
c. report sets (used in report files)

$offtext

SETS

*a. model sets

 AC           global set for model accounts - aggregated microsam accounts
 ACNT(AC)     all elements in AC except TOTAL
 A(AC)        activities
 ACES(A)      activities with CES fn at top of technology nest
 ALEO(A)      activities with Leontief fn at top of technology nest
*** INVESTCGE
 AVALEO(A)    activities with a CES fn in the VA nest
 AVACES(A)    activities with a Leontief fn in the VA nest
 AVAECES(A)   activities with a CES fn in the VAE nest
 AVAELEO(A)   activities with a Leontief fn in the VAE nest
*** /INVESTCGE

 C(AC)        commodities
 CD(C)        commodities with domestic sales of output
 CDN(C)       commodities without domestic sales of output
 CE(C)        exported commodities
 CEN(C)       non-export commodities
 CM(C)        imported commodities
 CMN(C)       non-imported commodities
 CX(C)        commodities with output

 F(AC)        factors
 INS(AC)      institutions
 INSD(INS)    domestic institutions
 INSDNG(INSD) domestic non-government institutions
 H(INSDNG)    households

*b. calibration sets
 CINV(C)      fixed investment goods
 CT(C)        transaction service commodities
 CTD(AC)      domestic transactions cost account
 CTE(AC)      export transactions cost account
 CTM(AC)      import transactions cost account

*c. report sets
 AAGR(A)      agricultural activities
 ANAGR(A)     non-agricultural activities
 CAGR(C)      agricultural commodities
 CNAGR(C)     non-agricultural commodities
*** INVESTCGE
 CENE(C)      energy commodities
 CNENE(C)     non-energy commodities
*** /INVESTCGE
 EN(INSDNG)   enterprises
 FLAB(F)      labor
 FLND(F)      land
 FCAP(F)      capital
 ;

*ALIAS statement to create identical cets
ALIAS
 (AC,ACP)  , (ACNT,ACNTP), (A,AP,APP)
 (C,CP,CPP), (CE,CEP)    , (CM,CMP)
*** INVESTCGE
 (CENE,CENEP)
*** /INVESTCGE
 (F,FP)    , (FLAB,FLABP), (FCAP,FCAPP)    , (FLND,FLNDP)
 (INS,INSP), (INSD,INSDP), (INSDNG,INSDNGP), (H,HP)
 ;


*2. DATABASE ##########################################################

PARAMETER
 SAM(AC,ACP)     standard SAM
 SAMBALCHK(AC)   column minus row total for SAM

 ;

*INCLUDE ONE COUNTRY DATA SET
*Remove asterisk in front of ONE (AND ONLY ONE) of the following lines
*or add a new line for new file with country data

$INCLUDE GHANA_DATA.GMS
*** INVESTCGE
**************************PROJECT INITIALISATION/CALIBRATION********************


set
 T       Time
 /2005*2100/

 FIN     Financing sources
 /
  BASE    No investment
  GCSPEND Government consumption spending
  GSSPEND Government savings
  GBOR    Bonds
  FBOR    Foreing lending
  GRANT   Foreign grants
 /;

* Mapping sectors involved as collateral in the project loan (for resource-secured loans)

set LOANACT(APROJ,A)
/
  A-PROJ.A-AGR
/;

alias(FIN,FFIN)  ;

parameters PVALUE(APROJ)                 Project value in USD
           REXR                          Real exchange rate
           PLVALUE(APROJ)                Project value in local currency
           sPFIN(FIN,APROJ)              Project finance - share by source
           PFIN(FIN,APROJ)               Project finance by source
           r(APROJ,FIN)                  Interest rate by source
           sPCOST(AC,APROJ)              Project cost share by cost category
           PCOST(AC,APROJ)               Project cost by category in local currency
           INVSTART(APROJ)               Investment start year
           CONSPER(APROJ)                Construction period
           OPSTART(APROJ)                Operation start
           OPPER(APROJ)                  Operation period
           GRACEPER(APROJ,FIN)           Grace period for loan repayment
           REPAYPER(APROJ,FIN)           Repayment period
           REPAYMENT(APROJ,FIN)          Annual amount to be repaid (inclusive of interest)
           LOANPROD(APROJ,C)             Annual demand for exports for resource-secured loan
           LOANPRODPRICE(APROJ,C)        Negotiated export price in USD
;

* initialising project parameters with zero values
 r(APROJ,FIN)            =      0 ;
 REPAYMENT(APROJ,FIN)    =      0 ;
 LOANPROD(APROJ,C)       =      0 ;
 LOANPRODPRICE(APROJ,C)  =      0 ;

* initialising dummy project with a very low project value
 PVALUE(APROJ)  =       0.000001     ;
* USD to local currency parity
 REXR    =       3.5       ;
 PLVALUE(APROJ) =       PVALUE(APROJ) * REXR   ;

* setting default financing through foreign loans. This can be altered at any point after the model calibration

 sPFIN("GCSPEND",APROJ)        =       0       ;
 sPFIN("GSSPEND",APROJ)        =       0       ;
 sPFIN("GBOR",APROJ)           =       0       ;
 sPFIN("FBOR",APROJ)           =       1       ;
 sPFIN("GRANT",APROJ)          =       0       ;

* calculating financed values by source
 PFIN(FIN,APROJ) =       sPFIN(FIN,APROJ)*PLVALUE(APROJ) ;

* initialising the cost structure of the project construction activity
 sPCOST("C-IND",APROJ)   = 0     ;
 sPCOST("C-CONS",APROJ)  = 0.4   ;
 sPCOST("C-SERV",APROJ)  = 0.07  ;
 sPCOST("C-ELEC",APROJ)  = 0     ;
 sPCOST("LABOR",APROJ)   = 0.23  ;
*> value share of other direct imports not available in the model SAM and/or commodities which require by-passing the Armington specification e.g. special equipment or services not available domestically
 sPCOST("ROW",APROJ) = 0.3    ;

 PCOST(AC,APROJ)$sPCOST(AC,APROJ)    =       sPCOST(AC,APROJ)*PLVALUE(APROJ)      ;

*** test project data validity
loop(APROJ,
         ABORT$(sum(FIN,SPFIN(FIN,APROJ)) <>1) "Project finance shares inconsistent" ;
         ABORT$(sum(AC,sPCOST(AC,APROJ)) <>1) "Project cost shares inconsistent" ;
) ;

********************************************************************************
*** /INVESTCGE


$TITLE Core model files. Standard CGE modeling system, Version 1.01
$STITLE Input file: MOD101.GMS. Standard CGE modeling system, Version 1.01

*SAM adjustments ====================================================

*In this section, some minor adjustments are made in the SAM (when
*needed) to fit the model structure.


*Adjustment for sectors with only exports and no domestic sales.
*If there is a very small value for domestic sales, add the discrepancy
*to exports.
 SAM(C,'ROW')$(ABS(SUM(A, SAM(A,C)) - (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                                    - SUM(CTE, SAM(CTE,C))) ) LT 1.E-6)
                 = SUM(A, SAM(A,C)) -  TAXPAR('EXPTAX',C)
                                    - SUM(CTE, SAM(CTE,C)) ;

*Netting transfers between domestic institutions and RoW.
 SAM(INSD,'ROW')   = SAM(INSD,'ROW') - SAM('ROW',INSD);
 SAM('ROW',INSD)   = 0;

*Netting transfers between factors and RoW.
 SAM('ROW',F)  = SAM('ROW',F) - SAM(F,'ROW');
 SAM(F,'ROW')  = 0;

*Netting transfers between government and domestic non-
*government institutions.
 SAM(INSDNG,'GOV') = SAM(INSDNG,'GOV') - SAM('GOV',INSDNG);
 SAM('GOV',INSDNG) = 0;

*Eliminating payments of any account to itself.
 SAM(ACNT,ACNT) = 0;


*Checking SAM balance=================================================
*Do NOT make any changes in the parameter SAM after this line!!!!!!!!!

*Account totals are recomputed. Check for SAM balance.
 SAM('TOTAL',ACNT) = SUM(ACNTP, SAM(ACNTP,ACNT));
 SAM(ACNT,'TOTAL') = SUM(ACNTP, SAM(ACNT,ACNTP));

 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');

 DISPLAY "SAM after final adjustments", SAMBALCHK;
 DISPLAY "SAM after final adjustments", SAM;


*Additional set definitions based on country SAM======================

*CD is the set for commodities with domestic sales of domestic output
*i.e., for which (value of sales at producer prices)
*              > (value of exports at producer prices)
 CD(C)  = YES$
    (SUM(A, SAM(A,C)) GT (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                                        - SUM(CTE, SAM(CTE,C))) );
 CDN(C) = NOT CD(C);

 CE(C)  = YES$(SAM(C,'ROW'));
 CEN(C) = NOT CE(C);

 CM(C)  = YES$(SAM('ROW',C));
 CMN(C) = NOT CM(C);

 CX(C) = YES$SUM(A, SAM(A,C));

 CT(C)
 $(SUM(CTD, SAM(C,CTD)) + SUM(CTE, SAM(C,CTE)) + SUM(CTM, SAM(C,CTM)))
  = YES;

 ALEO(A) = YES; ACES(A) = NO;

*If activity has no intermediate inputs, then Leontief function has to
*be used at the top of the technology nest
 ACES(A)$(NOT SUM(C, SAM(C,A))) = NO;
 ALEO(A)$(NOT ACES(A)) = YES;

DISPLAY
 CD, CDN, CE, CEN, CM, CMN, CX, CT, ACES, ALEO;

*Fine-tuning non-SAM data============================================

*Generating missing data for home consumption====

*If SAM includes home consumption but NO data were provided for SHRHOME,
*data are generated assuming that the value shares for home consumption
*are identical to activity output value shares.

IF(SUM((A,H), SAM(A,H)) AND NOT SUM((A,C,H), SHRHOME(A,C,H)),

 SHRHOME(A,C,H)$SAM(A,H)  = SAM(A,C)/SUM(CP, SAM(A,CP));

DISPLAY
 "Default data used for SHRHOME -- data missing"
 SHRHOME
 ;
*End IF statement
 );


*Eliminating superfluous elasticity data=========

 TRADELAS(C,'SIGMAT')$(CEN(C) OR (CE(C) AND CDN(C))) = 0;
 TRADELAS(C,'SIGMAQ')$(CMN(C) OR (CM(C) AND CDN(C))) = 0;

 PRODELAS(A)$(NOT SAM('TOTAL',A))     = 0;

 ELASAC(C)$(NOT SUM(A, SAM(A,C)))     = 0;

 LESELAS1(C,H)$(NOT SAM(C,H))         = 0;
 LESELAS2(A,C,H)$(NOT SHRHOME(A,C,H)) = 0;


*Diagnostics=====================================

*Include file that displays and generates information that may be
*useful when debugging data set.

$INCLUDE DIAGNOSTICS.INC

$STITLE Input file: MOD101.GMS. Standard CGE modeling system, Version 1.01

*Physical factor quantities======================

PARAMETER
 QF2BASE(F,A)  qnty of fac f employed by act a (extracted data)
 ;
*If there is a SAM payment from A to F and supply (but not
*demand) quantities have been defined in the country data file,
*then the supply values are used to compute demand quantities.
 QF2BASE(F,A)$(SAM(F,A)$((NOT QFBASE(F,A))$QFSBASE(F)))
   = QFSBASE(F)*SAM(F,A)/SUM(AP, SAM(F,AP));

*If there is a SAM payment from A to F and neither supply nor
*demand quantities have been defined in the country data file,
*then SAM values are used as quantities
 QF2BASE(F,A)$(SAM(F,A)$((QFBASE(F,A) EQ 0)$(QFSBASE(F) EQ 0)))
                                                    = SAM(F,A);

*If there is a SAM payment from A to F and demand quantities have
*been defined in the country data file, then this information is used.
 QF2BASE(F,A)$QFBASE(F,A) = QFBASE(F,A);

DISPLAY QF2BASE, QFBASE, QFSBASE;


*3. PARAMETER DECLARATIONS ##########################################
$ontext

This section is divided into the following subsections:
a. Parameters appearing in model equations
b. Parameters used for model calibration (to initialize variables and
   to define model parameters)

In each group, the parameters are declared in alphabetical order.

$offtext

PARAMETERS

*a. Parameters appearing in model equations================

*Parameters other than tax rates
 alphaa(A)      shift parameter for top level CES function
 alphaac(C)     shift parameter for domestic commodity aggregation fn
 alphaq(C)      shift parameter for Armington function
 alphat(C)      shift parameter for CET function
 alphava(A)     shift parameter for CES activity production function
*** INVESTCGE
 alphavae(A)    shift parameter for CES activity production function with energy - VAE bundle
 alpha2va(A)    shift parameter for CES activity production function with energy - VA bundle
 alpha2ene(A)   shift parameter for CES activity production function with energy - energy bundle
*** /INVESTCGE
 betah(A,C,H)   marg shr of hhd cons on home com c from act a
 betam(C,H)     marg share of hhd cons on marketed commodity c
 cwts(C)        consumer price index weights
 deltaa(A)      share parameter for top level CES function
 deltaac(A,C)   share parameter for domestic commodity aggregation fn
 deltaq(C)      share parameter for Armington function
 deltat(C)      share parameter for CET function

 deltava(F,A)   share parameter for CES activity production function

*** INVESTCGE
 deltavae_va(A) share parameter for CES activity production function with energy - energy bundle
 deltavae_ene(A) share parameter for CES activity production function with energy - VA bundle
 delta2va(F,A)  share parameter for CES activity production function - factor shares in VA bundle
 delta2ene(CENE,A)  share parameter for CES activity production function - energy commodity shares in ENE bundle
*** /INVESTCGE
 dwts(C)        domestic sales price weights
 gammah(A,C,H)  per-cap subsist cons for hhd h on home com c fr act a
 gammam(C,H)    per-cap subsist cons of marketed com c for hhd h
 ica(C,A)       intermediate input c per unit of aggregate intermediate
 inta(A)        aggregate intermediate input coefficient
 iva(A)         aggregate value added coefficient
*** INVESTCGE
 ica2(C,A)       intermediate input c per unit of aggregate intermediate - energy version
 inta2(A)        aggregate intermediate input coefficient - energy version
 ivae(A)        aggregate VA-Energy coefficient - energy version

 icaproj(C,APROJ)       intermediate input c per unit of aggregate intermediate in project activity APROJ
 ifact(F,APROJ)  factor input f per unite of value-added in project activity APROJ
*** /INVESTCGE
 icd(C,CP)      trade input of c per unit of comm'y cp produced & sold dom'ly
 ice(C,CP)      trade input of c per unit of comm'y cp exported
 icm(C,CP)      trade input of c per unit of comm'y cp imported
 mps01(INS)     0-1 par for potential flexing of savings rates
 mpsbar(INS)    marg prop to save for dom non-gov inst ins (exog part)
 qdst(C)        inventory investment by sector of origin
 qbarg(C)       exogenous (unscaled) government demand
 qbarinv(C)     exogenous (unscaled) investment demand
 rhoa(A)        CES top level function exponent
 rhoac(C)       domestic commodity aggregation function exponent
 rhoq(C)        Armington function exponent
 rhot(C)        CET function exponent
 rhova(A)       CES activity production function exponent
*** INVESTCGE
 rhovae(A)      CES activity production function exponent - VAE bundle
 rhoene(A)      CES activity production function exponent - ENE bundle
*** /INVESTCGE
 shif(INS,F)    share of dom. inst'on i in income of factor f
 shii(INS,INSP) share of inst'on i in post-tax post-sav income of inst ip
 supernum(H)    LES supernumerary income
 theta(A,C)     yield of commodity C per unit of activity A
 tins01(INS)    0-1 par for potential flexing of dir tax rates
 trnsfr(INS,AC) transfers fr. inst. or factor ac to institution ins

*Tax rates
 ta(A)          rate of tax on producer gross output value
 te(C)          rate of tax on exports
 tf(F)          rate of direct tax on factors (soc sec tax)
 tinsbar(INS)   rate of (exog part of) direct tax on dom inst ins
 tm(C)          rate of import tariff
 tq(C)          rate of sales tax
 tva(A)         rate of value-added tax

*** INVESTCGE
* Land supply parameters
 elasland        Land supply elasticity  /0.1   /
 landmax         Max agricultural land supply from baseline values / 2   /
 epsland         Baseyear position on supply on logistic supply curve
*** /INVESTCGE
*** INVESTCGE
* Dynamics
 depr            depreciation rate       /0.04 / ;
*** /INVESTCGE
*b. Parameters used for model calibration==================

$ontext

For model calibration, one parameter is created for each model variable
with the suffix "0" added to the variable name. 0 is also added to the
names of parameters whose values are changed in experiments.

$offtext

PARAMETERS
*Parameters for definition of model parameters
 alphava0(A)     shift parameter for CES activity production function
 qdst0(C)        stock change
 qbarg0(C)       exogenous (unscaled) government demand
 gammah0(A,C,H)  per-cap subsist cons for hhd h on home com c fr act a
 gammam0(C,H)    per-cap subsist cons of marketed com c for hhd h

 ta0(A)          rate of tax on producer gross output value
 te0(C)          rate of tax on exports
 tf0(F)          rate of direct tax on factors (soc sec tax)
 tins0(INS)      rate of direct tax on domestic institutions ins
 tm0(C)          rate of import tariff
 tq0(C)          rate of sales tax
 tva0(A)         rate of value-added tax

*Check parameters
  cwtschk        check that CPI weights sum to unity
  dwtschk        check that PDIND weights sum to unity
  shifchk        check that factor payment shares sum to unity

*Parameters for variable initialization
  CPI0           consumer price index (PQ-based)
  DPI0           index for domestic producer prices (PDS-based)
  DMPS0          change in marginal propensity to save for selected inst
  DTINS0         change in domestic institution tax share
  EG0            total current government expenditure
  EH0(H)         household consumption expenditure
  EXR0           exchange rate
  FSAV0          foreign savings
  GADJ0          government demand scaling factor
  GOVSHR0        govt consumption share of absorption
  GSAV0          government savings
  IADJ0          investment scaling factor (for fixed capital formation)
  INVSHR0        investment share of absorption
  MPS0(INS)      marginal propensity to save for dom non-gov inst ins
  MPSADJ0        savings rate scaling factor
  PA0(A)         output price of activity a
  PDD0(C)        demand price for com'y c produced & sold domestically
  PDS0(C)        supply price for com'y c produced & sold domestically
  PE0(C)         price of exports
  PINTA0(A)      price of intermediate aggregate
*** INVESTCGE - just for reporting?
  PINTA20(A)      price of intermediate aggregate
*** /INVESTCGE
  PM0(C)         price of imports
  PQ0(C)         price of composite good c
  PVA0(A)        value added price
*** INVESTCGE
  PVAE0(A)       value added and energy price
  PENE0(A)       energy bundle price
*** /INVESTCGE
  PWE0(C)        world price of exports
  PWM0(C)        world price of imports
  PX0(C)         average output price
  PXAC0(A,C)     price of commodity c from activity a
  QA0(A)         level of domestic activity
  QD0(C)         quantity of domestic sales
  QE0(C)         quantity of exports
  QF0(F,A)       quantity demanded of factor f from activity a
  QFS0(F)        quantity of factor supply
  QG0(C)         quantity of government consumption
  QH0(C,H)       quantity consumed of marketed commodity c by hhd h
  QHA0(A,C,H)    quantity consumed of home commodity c fr act a by hhd h
  QINT0(C,A)     quantity of intermediate demand for c from activity a
  QINTA0(A)      quantity of aggregate intermediate input
*** INVESTCGE
  QINTA20(A)     quantity of aggregate intermediate input - energy version
*** /INVESTCGE
  QINV0(C)       quantity of fixed investment demand
*** INVESTCGE
  QINVTOT0       quantity of total investment
  KSTOCK0        quantity of end of simulation capital stock
  KSTOCKP0       quantity of start of simulation capital stock
  CAPG0          capital stock growth rate
*** /INVESTCGE
  QM0(C)         quantity of imports
  QQ0(C)         quantity of composite goods supply
  QT0(C)         quantity of trade and transport demand for commodity c
  QVA0(A)        quantity of aggregate value added
*** INVESTCGE
  QVAE0(A)       quantity of value added and energy in activity a
  QENE0(A)       quantity of energy demand from activity a
*** /INVESTCGE
  QX0(C)         quantity of aggregate marketed commodity output
  QXAC0(A,C)     quantity of ouput of commodity c from activity a
  TABS0          total absorption
  TINS0(INS)     rate of direct tax on domestic institutions ins
  TINSADJ0       direct tax scaling factor
  TRII0(INS,INSP) transfers to dom. inst. insdng from insdngp
  WALRAS0        savings-investment imbalance (should be zero)
  WF0(F)         economy-wide wage (rent) for factor f
  WFDIST0(f,A)   factor wage distortion variable
  YF0(f)         factor income
  YG0            total current government income
  YIF0(INS,F)    income of institution ins from factor f
  YI0(INS)       income of (domestic non-governmental) institution ins
  ;


*4. PARAMETER DEFINITIONS ###########################################


*All parameters are defined, divided into the same blocks as the
*equations.

*Price block=====================================

$ontext

The prices PDS, PX, and PE  may be initialized at any desired price.
The user may prefer to initialize these prices at unity or, if
he/she is interested in tracking commodity flows in physical units, at
commodity-specific, observed prices (per physical unit). For any given
commodity, these three prices should be identical. Initialization at
observed prices may be attractive for disaggregated agricultural
commodities. If so, the corresponding quantity values reflect physical
units (given the initial price).

The remaining supply-side price, PXAC, and the non-commodity prices, EXR
and PA may be initizalized at any desired level. In practice, it may be
preferable to initialize PXAC at the relevant supply-side price and EXR
and PA at unity.

If physical units are used, the user should select the unit (tons vs.
'000 tons) so that initial price and quantity variables are reasonably
scaled (for example between 1.0E-2 and 1.0E+3) -- bad scaling may cause
solver problems. Initialization at unity should cause no problem as long
as the initial SAM is reasonably scaled.

$offtext

PARAMETER
 PSUP(C) initial supply-side market price for commodity c
;
 PSUP(C) = 1;

 PE0(C)$CE(C)        = PSUP(C);
 PX0(C)$CX(C)        = PSUP(C);
 PDS0(C)$CD(C)       = PSUP(C);
 PXAC0(A,C)$SAM(A,C) = PSUP(C);

 PA0(A)       = 1;

$ontext
The exchange rate may be initialized at unity, in which case all data are
in foreign currency units (FCU; e.g., dollars). Set the exchange rate at
another value to differentiate foreign exchange transactions, which will
be valued in FCU, and domestic transactions valued in local currency
units (LCU). The SAM is assumed to be valued in LCU, and the exchange rate
is then used to calculate FCU values for transactions with the rest of the
world.
$offtext

 EXR0          = 1 ;

*Activity quantity = payment to activity divided by activity price
*QA covers both on-farm consumption and marketed output
*output GROSS of tax
 QA0(A)        =  SAM('TOTAL',A)/PA0(A) ;

*Unit value-added price = total value-added / activity quantity
*define pva gross of tax
 QVA0(A)       =  (SUM(F, SAM(F,A))+ TAXPAR('VATAX',A)) ;
 PVA0(A)       =  (SUM(F, SAM(F,A))+ TAXPAR('VATAX',A))/QVA0(A);
 iva(A)        =  QVA0(A)/QA0(A) ;
 QXAC0(A,C)$SAM(A,C)
               = SAM(A,C) / PXAC0(A,C);

 QHA0(A,C,H)$SHRHOME(A,C,H) = SHRHOME(A,C,H)*SAM(A,H)/PXAC0(A,C);


*Output quantity = value received by producers divided by producer
*price
*QX covers only marketed output
 QX0(C)$SUM(A, SAM(A,C))
         =  SUM(A, SAM(A,C)) / PX0(C);

*Export quantity = export revenue received by producers
*(ie. minus tax and transactions cost) divided by
*export price.
 QE0(C)$SAM(C,'ROW')
    =  (SAM(C,'ROW') - TAXPAR('EXPTAX',C)
                - SUM(CTE, SAM(CTE,C)))/PE0(C);

*RoW export price = RoW export payment (in for curr) / export qnty
 PWE0(C)$QE0(C) = (SAM(C,'ROW')/EXR0) / QE0(C);

 te0(C)$SAM(C,'ROW') = TAXPAR('EXPTAX',C)/SAM(C,'ROW');
 te(C)               =  te0(C);


*Quantity of output sold domestically = output quantity less quantity
*exported = value of domestic sales divided by domestic supply price
*QD0 covers only marketed output
 QD0(C)$CD(C) =  QX0(C) - QE0(C);

*Domestic demander price = demander payment divided by quantity bought
 PDD0(C)$QD0(C)= (PDS0(C)*QD0(C) + SUM(CTD, SAM(CTD,C)))/QD0(C);

*Define import price to equal domestic price so that import and domestic
*units are the same to the purchaser. If no domestic good, set PM to 1.
 PM0(C)               = PDD0(C) ;
 PM0(C)$(QD0(C) EQ 0) = 1 ;

*Import quantity = demander payment for imports (including tariffs
*and marketing cost) divided by demander price.
 QM0(C)$CM(C) = (SAM('ROW',C) + TAXPAR('IMPTAX',C)
               + SUM(CTM, SAM(CTM,C)))/PM0(C);

*World price = import value (in foreign currency / import quantity
 PWM0(C)$QM0(C)= (SAM('ROW',C)/EXR0) / QM0(C);
 tm0(C)$SAM('ROW',C)
               = TAXPAR('IMPTAX',C) / SAM('ROW',C);
 tm(C)         = tm0(C);


*Composite supply is the sum of domestic market sales and imports
*(since they are initialized at the same price).
 QQ0(C)$(CD(C) OR CM(C)) = QD0(C) + QM0(C) ;
 PQ0(C)$QQ0(C) = (SAM(C,'TOTAL') - SAM(C,'ROW'))/QQ0(C);
*** INVESTCGE
* initialising domestic prices for project commodity (not present in the base SAM)
 PQ0(C) = PQ0(C) + 1$(NOT PQ0(C))     ;
*** /INVESTCGE
 TQ0(C)$QQ0(C) = TAXPAR('COMTAX',C)/(PQ0(C)*QQ0(C)) ;
 TQ(C)         = TQ0(C) ;

*The following code works when for any number of sectors providing
*transactions services, as well as for the case when they are not
*in the SAM.

PARAMETERS
 SHCTD(C)  share of comm'y ct in trans services for domestic sales
 SHCTM(C)  share of comm'y ct in trans services for imports
 SHCTE(C)  share of comm'y ct in trans services for exports
  ;
 SHCTD(CT) = SUM(CTD, SAM(CT,CTD)/SAM('TOTAL',CTD)) ;
 SHCTM(CT) = SUM(CTM, SAM(CT,CTM)/SAM('TOTAL',CTM)) ;
 SHCTE(CT) = SUM(CTE, SAM(CT,CTE)/SAM('TOTAL',CTE)) ;


*Transactions input coefficients
 icd(CT,C)$QD0(c)
   = (shctd(ct)*SUM(CTD, SAM(CTD,C))/PQ0(ct)) / QD0(C);
 icm(CT,C)$QM0(C)
  = (shctm(ct)*SUM(CTM, SAM(CTM,C))/PQ0(ct)) / QM0(C);
 ice(CT,C)$QE0(C)
  = (shcte(ct)*SUM(CTE, SAM(CTE,C))/PQ0(ct)) / QE0(C);


*Indirect activity tax rate = tax payment / output value
*Tax is here applied to total output value (incl. on-farm cons.)
 tva0(A)       = TAXPAR('VATAX',A) / (PVA0(A)*QVA0(A));
 tva(A)        = tva0(A);

*QA is GROSS of tax, so base for ta is as well
 ta0(A)        = TAXPAR('ACTTAX',A) / (SAM(A,'TOTAL'));
 ta(A)         = ta0(A);

*Yield coefficient
* = quantity produced (including home-consumed output)
*   /activity quantity
 theta(A,C)$PXAC0(A,C)
  = ( (SAM(A,C) + SUM(H, SHRHOME(A,C,H)*SAM(A,H)) ) / PXAC0(A,C) )
                                                              / QA0(A);

*Intermediate input coefficient = input use / output quantity
 QINTA0(A) = SUM(C$PQ0(C), SAM(C,A)  / PQ0(C)) ;

 ica(C,A)$(QINTA0(A)$PQ0(C))
               = SAM(C,A)/PQ0(C) / QINTA0(A) ;

 inta(A)       = QINTA0(A) / QA0(A) ;
 pinta0(A)     = SUM(C, ica(C,A)*PQ0(C)) ;

*CPI weight by comm'y = hhd cons value for comm'y / total hhd cons value
*CPI does not consider on-farm consumption.
 cwts(C)       = SUM(H, SAM(C,H)) / SUM((CP,H), SAM(CP,H));

*Domestic sales price index weight = dom sales value for comm'y
*/ total domestic salues value
*Domestic sales price index does not consider on-farm consumption.
 dwts(C)       = (SUM(A, SAM(A,C)) - (SAM(C,'ROW') -
                  SUM(cte, SAM(cte,C))))/
                  SUM(CP, SUM(A, SAM(A,CP)) - (SAM(CP,'ROW') -
                  SUM(cte, SAM(cte,CP))));

 CWTSCHK       = SUM(C, cwts(C));
 DWTSCHK       = SUM(C, dwts(C));

 CPI0          = SUM(C, cwts(C)*PQ0(C)) ;
 DPI0          = SUM(CD, dwts(CD)*PDS0(CD)) ;

DISPLAY
 CWTSCHK, DWTSCHK;

*Production and trade block==========================

*Compute exponents from elasticites
 rhoq(C)$(CM(C) AND CD(C)) = (1/TRADELAS(C,'SIGMAQ')) - 1;
 rhot(C)$(CE(C) AND CD(C))  = (1/TRADELAS(C,'SIGMAT')) + 1;
*** INVESTCGE
 rhovae(A)        = (1/PRODELAS_VAE(A)) - 1;
 rhoene(A)        = (1/PRODELAS_ENE(A)) - 1;
*** /INVESTCGE
 rhova(A)        = (1/PRODELAS(A)) - 1;
 rhoa(A)$ACES(A) = (1/PRODELAS2(A)) - 1;

*Aggregation of domestic output from different activities

 RHOAC(C)$ELASAC(C) = 1/ELASAC(C) - 1;

 deltaac(A,C)$ (SAM(A,C)$ELASAC(C))
               = (PXAC0(A,C)*QXAC0(A,C)**(1/ELASAC(C)))/
                 SUM(AP, PXAC0(AP,C)*QXAC0(AP,C)**(1/ELASAC(C)));

 alphaac(C)$SUM(A,deltaac(A,C))
               = QX0(C)/
                 (SUM(A$deltaac(A,C), deltaac(A,C) * QXAC0(A,C)
                 **(-RHOAC(C))) )**(-1/RHOAC(C));

PARAMETERS
 WFA(F,A)          wage for factor f in activity a (used for calibration)
 ;

*Demand computations=====

*Defining factor employment and supply.
 QF0(F,A)  = QF2BASE(F,A);
 QFS0(F)   = SUM(A, QF0(F,A));

*Activity-specific wage is activity labor payment over employment
 WFA(F,A)$SAM(F,A) = SAM(F,A)/QF0(F,A);


*Economy-wide wage average is total factor income over employment
 WF0(F) = SUM(A, SAM(F,A))/SUM(A, QF0(F,A));

DISPLAY
"If the value of WF0 for any factor is very different from one (< 0.1"
"or >10) the user may consider rescaling the initial values for QFBASE"
"or QFSBASE for this factor to get a value of WF0 such that"
"0.1 < WF0 < 10"
 WF0
 ;

*Wage distortion factor
 wfdist0(f,A)$SAM(F,A) = WFA(F,A)/WF0(F);

*CES activity production function

 deltava(F,A)$SAM(F,A)
            = (wfdist0(F,A) * WF0(F)
              * (QF0(F,A))**(1+rhova(A)) )
              / SUM(FP, wfdist0(FP,A) * WF0(FP)*(QF0(FP,A))**(1+rhova(A)));

 alphava0(A)= QVA0(A)/( SUM(F$(QF0(F,A)), deltava(F,A)*QF0(F,A)
               **(-rhova(A))) )**(-1/rhova(A));

 alphava(A) = alphava0(A);

*CES top level production function
PARAMETER
  predeltaa(A)  dummy used to define deltaa
  ;

 predeltaa(A)  = 0 ;
 predeltaa(A)$(ACES(A) AND QINTA0(A))
                = (PVA0(A)/PINTA0(A))*(QVA0(A)/QINTA0(A))**(1+rhoa(A)) ;
 deltaa(A)$ACES(A) = predeltaa(A)/(1 + predeltaa(A)) ;
 alphaa(A)$deltaa(A)
                = QA0(A)/((deltaa(A)*QVA0(A)**(-rhoa(A))
                  +(1-deltaa(A))*QINTA0(A)**(-rhoa(A)))**(-1/rhoa(A))) ;

*Intermediate demand
 QINT0(C,A)$PQ0(C) = SAM(C,A) / PQ0(C);

*** INVESTCGE

************ Investment project calibration

* Project activity production function

 icaproj(C,APROJ)$SUM(AC,PCOST(AC,APROJ)) = PCOST(C,APROJ) / SUM(AC,PCOST(AC,APROJ)) ;
 ifact(F,APROJ)$SUM(AC,PCOST(AC,APROJ))   = PCOST(F,APROJ) / SUM(AC,PCOST(AC,APROJ))  ;

display icaproj, ifact ;

************ Energy-version calibration

 AVAECES(A) = YES ;

 QENE0(A) = 0 ;
 QENE0(A) = SUM(CENE$PQ0(CENE), SAM(CENE,A)  / PQ0(CENE)) ;
 PENE0(A)$QENE0(A) = SUM(CENE$PQ0(CENE), PQ0(CENE)*QINT0(CENE,A)) / QENE0(A) ;
 QVAE0(A) = QENE0(A) + QVA0(A)   ;
* QVAE0(A) = QENE0(A)*QENE0(A) + PVA0(A)*QVA0(A) ;
 PVAE0(A) = (QENE0(A)*PENE0(A) + PVA0(A)*QVA0(A)) / QVAE0(A)    ;

*** energy-version intermediate demand bundle


 QINTA20(A) = SUM(CNENE$PQ0(CNENE), SAM(CNENE,A)  / PQ0(CNENE)) ;

 ica2(CNENE,A)$QINTA20(A)
               = QINT0(CNENE,A) / QINTA20(A) ;

 inta2(A)      = QINTA20(A) / QA0(A) ;
 pinta20(A)    = SUM(CNENE, ica2(CNENE,A)*PQ0(CNENE)) ;


 ivae(A)       =  QVAE0(A)/QA0(A) ;


**** energy-version VA bundle

 delta2va(F,A)$SAM(F,A)
            = (wfdist0(F,A) * WF0(F) * (QF0(F,A))**(1+rhova(A)) )
              / SUM(FP, wfdist0(FP,A) * WF0(FP)*(QF0(FP,A))**(1+rhova(A)));

 alpha2va(A)= QVA0(A)/( SUM(F$(QF0(F,A)), delta2va(F,A)*QF0(F,A)
               **(-rhova(A))) )**(-1/rhova(A));


*** energy-version ENE bundle

 delta2ene(CENE,A)$QINT0(CENE,A)
            = (PQ0(CENE) * (QINT0(CENE,A))**(1+rhoene(A)) )
              / SUM(CENEP, PQ0(CENEP)*(QINT0(CENEP,A))**(1+rhoene(A)));

 alpha2ene(A)$QENE0(A) = QENE0(A)/( SUM(CENE$(QINT0(CENE,A)), delta2ene(CENE,A)*QINT0(CENE,A)
               **(-rhoene(A))) )**(-1/rhoene(A));



*** energy-version VAE bundle

 deltavae_va(A)  = 1 ;
 alphavae(A)     = 1 ;

 deltavae_va(A)$(QVA0(A) AND QENE0(A))
            = (PVA0(A) * QVA0(A)**(1+rhovae(A)) )
              / (PVA0(A) * QVA0(A)**(1+rhovae(A)) + PENE0(A) * QENE0(A)**(1+rhovae(A)));

 deltavae_ene(A)$QENE0(A)
            = (PENE0(A) * QENE0(A)**(1+rhovae(A)) )
              / (PVA0(A) * QVA0(A)**(1+rhovae(A)) + PENE0(A) * QENE0(A)**(1+rhovae(A)));


 alphavae(A)$(QVA0(A) AND QENE0(A)) = QVAE0(A)/
                         ( deltavae_va(A) * QVA0(A)**(-rhovae(A)) + deltavae_ene(A) * QENE0(A)**(-rhovae(A)) ) **(-1/rhovae(A))   ;

 AVAELEO(A)$(NOT deltavae_ene(A)) = YES ;
 AVAECES(A)$(AVAELEO(A)) = NO ;

*Transactions demand
 QT0(CT) = ( SUM(CTD, SAM(CT,CTD)) + SUM(CTE, SAM(CT,CTE))
             + SUM(CTM, SAM(CT,CTM)) ) / PQ0(CT) ;

*CET transformation
 deltat(C)$(CE(C) AND CD(C))
   = 1 / (1 + PDS0(C)/PE0(C)*(QE0(C)/QD0(C))**(rhot(C)-1));

 alphat(C)$(CE(C) AND CD(C))
   = QX0(C) / (deltat(C)*QE0(C)**rhot(C) + (1-deltat(C))
                 *QD0(C)**rhot(C))**(1/rhot(C));


*Armington aggregation

PARAMETER
 predelta(C)  dummy used to define deltaq
 ;

 predelta(C)$(CM(C) AND CD(C))
   = (PM0(C)/(PDD0(C)))*(QM0(C)/QD0(C))**(1+rhoq(C)) ;

 deltaq(C)$(CM(C) AND CD(C))
   = predelta(C)/(1 + predelta(C)) ;

 alphaq(C)$(CM(C) AND CD(C))
               = QQ0(C)/(deltaq(C)*QM0(C)**(-rhoq(C))
                 +(1-deltaq(C))*QD0(C)**(-rhoq(C)))**(-1/rhoq(C)) ;


*** INVESTCGE
* Land supply parametrisation

 epsland = (landmax - 1) * exp(elasland) ;
*** INVESTCGE
*Institution block===============================

*Institutional income
 YI0(INSDNG) = SAM('TOTAL',INSDNG);

*Factor income by factor category
 YF0(F) = SUM(A, SAM(F,A));

*Institution income from factors
 YIF0(INSD,F) = SAM(INSD,F);

*Transfers to RoW from factors
 trnsfr('ROW',F) = SAM('ROW',F)/EXR0;

*Transfers from RoW to institutions
 trnsfr(INSD,'ROW') = SAM(INSD,'ROW')/EXR0;

*Government transfers
 trnsfr(INSD,'GOV') = SAM(INSD,'GOV')/CPI0;

*Factor taxes
 tf0(F)        = TAXPAR('FACTAX',F)/SAM('TOTAL',F);
 tf(F)         = tf0(F);

*Shares of domestic institutions in factor income (net of factor taxes
*and transfers to RoW).
 shif(INSD,F)  = SAM(INSD,F)/(SAM(F,'TOTAL') - TAXPAR('FACTAX',f)
                 - SAM('ROW',F));

 SHIFCHK(F)    = SUM(INSD, shif(INSD,F));

DISPLAY
 SHIFCHK;

*Inter-institution transfers
 TRII0(INSDNG,INSDNGP) = SAM(INSDNG,INSDNGP);

*Share of dom non-gov institution in income of other dom non-gov
*institutions (net of direct taxes and savings).
 shii(INSDNG,INSDNGP)
  = SAM(INSDNG,INSDNGP)
   /(SAM('TOTAL',INSDNGP) - TAXPAR('INSTAX',INSDNGP) - SAM('S-I',INSDNGP));

*Scaling factors for savings and direct tax shares
 MPSADJ0      = 0;
 TINSADJ0     = 0;

*Savings rates
 MPS0(INSDNG)  = SAM('S-I',INSDNG)/(SAM('TOTAL',INSDNG) - TAXPAR('INSTAX',INSDNG));
 mpsbar(INSDNG) = MPS0(INSDNG);

*Direct tax rates
 TINS0(INSDNG)   = TAXPAR('INSTAX',INSDNG) / SAM('TOTAL',INSDNG);
 tinsbar(INSDNG) = TINS0(INSDNG);

*"Point" change in savings and direct tax shares
 DMPS0  = 0;
 DTINS0 = 0;


*Selecting institutions for potential "point" change in savings and tax rates

*If DMPS or MPSADJ is flexible, institutions with a value of 1 for mps01
*change their savings rates.
 mps01(INSDNG)  = 1;

*If DTIMS is flexible, institutions with a value of 1 for tins01 change
*their savings rates.
 tins01(INSDNG) = 1;

*Household consumption spending and consumption quantities.
 EH0(H)        = SUM(C, SAM(C,H)) + SUM(A, SAM(A,H));
 QH0(C,H)$PQ0(C) = SAM(C,H)/PQ0(C);

*Government indicators
 YG0           = SAM('TOTAL','GOV');
 EG0           = SAM('TOTAL','GOV') - SAM('S-I','GOV');
 QG0(C)$PQ0(C) = SAM(C,'GOV')/PQ0(C);

 qbarg0(C)     = QG0(C);
 qbarg(C)      = qbarg0(C);
 GADJ0         = 1;
 GSAV0         = SAM('S-I','GOV');

*LES calibration===========================================

PARAMETERS
 BUDSHR(C,H)    budget share for marketed commodity c and household h
 BUDSHR2(A,C,H) budget share for home commodity c - act a - hhd h
 BUDSHRCHK(H)   check that budget shares some to unity
 ELASCHK(H)     check that expenditure elasticities satisfy Engel aggr
 ;

 BUDSHR(C,H)    = SAM(C,H)/(SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H)));

 BUDSHR2(A,C,H) = SAM(A,H)*SHRHOME(A,C,H)
                  /(SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H)));

 BUDSHRCHK(H)   = SUM(C, BUDSHR(C,H)) + SUM((A,C), BUDSHR2(A,C,H));

 ELASCHK(H)     = SUM(C, BUDSHR(C,H)*LESELAS1(C,H))
                  + SUM((A,C), BUDSHR2(A,C,H)*LESELAS2(A,C,H));

DISPLAY BUDSHR, BUDSHR2, BUDSHRCHK, LESELAS1, LESELAS2, ELASCHK;

 LESELAS1(C,H)   = LESELAS1(C,H)/ELASCHK(H);
 LESELAS2(A,C,H) = LESELAS2(A,C,H)/ELASCHK(H);

 ELASCHK(H)      = SUM(C, BUDSHR(C,H)*LESELAS1(C,H))
                   + SUM((A,C), BUDSHR2(A,C,H)*LESELAS2(A,C,H));

DISPLAY ELASCHK, LESELAS1, LESELAS2;

 betam(C,H)   = BUDSHR(C,H)*LESELAS1(C,H);
 betah(A,C,H) = BUDSHR2(A,C,H)*LESELAS2(A,C,H);

 gammam0(C,H)$BUDSHR(C,H)
     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H))) / PQ0(C) )
                      * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));

 gammah0(A,C,H)$BUDSHR2(A,C,H)
     =  ( (SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H))) / PXAC0(A,C) )
                      * ( BUDSHR2(A,C,H) + betah(A,C,H)/FRISCH(H));

 gammam(C,H)   =  gammam0(C,H);

 gammah(A,C,H) =  gammah0(A,C,H);

*Checking LES parameters===================================
PARAMETERS
 SUBSIST(H)  subsistence spending
 FRISCH2(H)  alt. defn of Frisch -- ratio of cons to supernumerary cons
 LESCHK(H)   check on LES parameter definitions (error mssg if error)

 LESELASP(H,*,C,*,CP) price elasticity bt c and cp for h (with c and cp labeled by source)
*LESELASP defines cross-price elasticities when c is different from cp and
*own-price elasticities when c and cp refer to the same commodity.
*Source: Dervis, de Melo and Robinson. 1982. General Equilibrium Models
*for Development Policy. Cambridge University Press, p. 483
 ;
 SUPERNUM(H)  = SUM((A,C), gammah(A,C,H)*PXAC0(A,C))
                + SUM(C, gammam(C,H)*PQ0(C)) ;
 FRISCH2(H)   = -EH0(H)/(EH0(H) - SUPERNUM(H));
 LESCHK(H)$(ABS(FRISCH(H) - FRISCH2(H)) GT 0.00000001) = 1/0;


*Cross-price elasticities

 LESELASP(H,'MRK',C,'MRK',CP)
    $((ORD(C) NE ORD(CP)) AND LESELAS1(C,H) AND LESELAS1(CP,H))
  = -LESELAS1(C,H)
    * PQ0(CP)*gammam(CP,H) / (SUM(CPP, SAM(CPP,H)) + SUM(APP, SAM(APP,H)));

 LESELASP(H,A,C,'MRK',CP)
    $((ORD(C) NE ORD(CP)) AND LESELAS2(A,C,H) AND LESELAS1(CP,H))
  = -LESELAS2(A,C,H)
    * PQ0(CP)*gammam(CP,H) / (SUM(CPP, SAM(CPP,H)) + SUM(APP, SAM(APP,H)));

 LESELASP(H,'MRK',C,A,CP)
    $((ORD(C) NE ORD(CP)) AND LESELAS1(C,H) AND LESELAS2(A,CP,H))
  = -LESELAS1(C,H)
    * PXAC0(A,CP)*gammah(A,CP,H) /(SUM(CPP, SAM(CPP,H)) + SUM(APP, SAM(APP,H)));

*Own-price elasticities

 LESELASP(H,'MRK',C,'MRK',C)
   = -LESELAS1(C,H)
     *( PQ0(C)*gammam(C,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H)))
                                                       - 1/FRISCH(H));

 LESELASP(H,A,C,A,C)
   = -LESELAS2(A,C,H)
     *( PXAC0(A,C)*gammah(A,C,H) / (SUM(CP, SAM(CP,H)) + SUM(AP, SAM(AP,H)))
                                                       - 1/FRISCH(H));

OPTION LESELASP:3:2:2;

DISPLAY
 SUPERNUM, FRISCH, FRISCH2, LESCHK, LESELASP
 ;


*System-constraint block =========================

*Fixed investment
 qbarinv(c)$CINV(C) = SAM(C,'S-I')/PQ0(C);
 QINV0(C)           = qbarinv(C);
 IADJ0              = 1;

*Stock changes
 qdst0(C)$PQ0(C) = (SAM(C,'S-I')$(NOT CINV(C)) + SAM(C,'DSTK'))/PQ0(C);
 qdst(C)         = qdst0(C);

 FSAV0         = SAM('S-I','ROW')/EXR0;

 TABS0         = SUM((C,H), SAM(C,H)) + SUM((A,H), SAM(A,H))
                 + SUM(C, SAM(C,'GOV')) + SUM(C, SAM(C,'S-I'))
                 + SUM(C, SAM(C,'DSTK'));

 INVSHR0       = SAM('TOTAL','S-I')/TABS0;
 GOVSHR0       = SUM(C, SAM(C,'GOV'))/TABS0;

 WALRAS0       = 0;

*** INVESTCGE
 QINVTOT0      = SUM(C, QINV0(C) + qdst(C))      ;
 KSTOCKP0      = QINVTOT0 * 11.5 ;
 KSTOCK0       = KSTOCKP0*(1-depr) + QINVTOT0     ;
 CAPG0         = KSTOCK0 / KSTOCKP0      ;
*** /INVESTCGE
*5. VARIABLE DECLARATIONS ###########################################
*This section only includes variables that appear in the model.
*The variables are declared in alphabetical order.

VARIABLES
  CPI           consumer price index (PQ-based)
  DPI           index for domestic producer prices (PDS-based)
  DMPS          change in marginal propensity to save for selected inst
  DTINS         change in domestic institution tax share
  EG            total current government expenditure
  EH(H)         household consumption expenditure
  EXR           exchange rate
  FSAV          foreign savings
  GADJ          government demand scaling factor
  GOVSHR        govt consumption share of absorption
  GSAV          government savings
*** INVESTCGE
  PSAV          total private savings
*** /INVESTCGE
  IADJ          investment scaling factor (for fixed capital formation)
  INVSHR        investment share of absorption
  MPS(INS)      marginal propensity to save for dom non-gov inst ins
  MPSADJ        savings rate scaling factor
  PA(A)         output price of activity a
  PDD(C)        demand price for com'y c produced & sold domestically
  PDS(C)        supply price for com'y c produced & sold domestically
  PE(C)         price of exports
  PINTA(A)      price of intermediate aggregate
  PM(C)         price of imports
  PQ(C)         price of composite good c
*** INVESTCGE
  PENE(A)       price of energy aggregate
  PVAE(A)       price of VA-Energy aggregate
*** /INVESTCGE
  PVA(A)        value added price
  PWE(C)        world price of exports
  PWM(C)        world price of imports
  PX(C)         average output price
  PXAC(A,C)     price of commodity c from activity a
  QA(A)         level of domestic activity
  QD(C)         quantity of domestic sales
  QE(C)         quantity of exports
  QF(F,A)       quantity demanded of factor f from activity a
  QFS(F)        quantity of factor supply
*** INVESTCGE
  QFSPROJ(F,APROJ) quantity of factor supply by investment project
*** /INVESTCGE
  QG(C)         quantity of government consumption
  QH(C,H)       quantity consumed of marketed commodity c by household h
  QHA(A,C,H)    quantity consumed of home commodity c fr act a by hhd h
  QINT(C,A)     quantity of intermediate demand for c from activity a
  QINTA(A)      quantity of aggregate intermediate input
*** INVESTCGE
  QENE(A)        quantity of aggregate energy input
  QVAE(A)        quantity of aggregate VA-Energy input
*** /INVESTCGE
  QVA(A)        quantity of aggregate value added
*** INVESTCGE
  QINTPROJ(C,APROJ) quantity of project demand for c from project APROJ
  QFPROJ(F,APROJ) quantity demanded of factor f from project APROJ
  PROJEXP(APROJ) project actual expenditure
*** /INVESTCGE
  QINV(C)       quantity of fixed investment demand
*** VNINVESTCGE
  QINVTOT       total investment demand  - added for model dynamics
  KSTOCK        end of period stock
  KSTOCKP       start of period stock
  CAPG          capital stock growth
*** /INVESTCGE
  QM(C)         quantity of imports
  QQ(C)         quantity of composite goods supply
  QT(C)         quantity of trade and transport demand for commodity c
  QX(C)         quantity of aggregate marketed commodity output
  QXAC(A,C)     quantity of ouput of commodity c from activity a
  TABS          total absorption
  TINS(INS)     rate of direct tax on domestic institutions ins
  TINSADJ       direct tax scaling factor
  TRII(INS,INSP) transfers to dom. inst. insdng from insdngp
  WALRAS        savings-investment imbalance (should be zero)
  WALRASSQR     Walras squared
  WF(F)         economy-wide wage (rent) for factor f
  WFDIST(F,A)   factor wage distortion variable
  YF(F)         factor income
*** INVESTCGE
  YGPROJ        government income from project capital rents
*** /INVESTCGE
  YG            total current government income
  YIF(INS,F)    income of institution ins from factor f
  YI(INS)       income of (domestic non-governmental) institution ins
*** INVESTCGE
  YTAX          government tax income
  TINV          total investment value
*** /INVESTCGE
  ;

*6. VARIABLE DEFINITIONS ############################################

*The initial levels of all model variables are defined in this file.
*** INVESTCGE
$INCLUDE VARINIT_ENE.INC
*** /INVESTCGE

*Optional include file that imposes lower limits for selected variables
*The inclusion of this file may improve solver performance.
*$INCLUDE VARLOW.INC


$STITLE IFPRI CGE modeling system, Energy version

*7. EQUATION DECLARATIONS ###########################################

EQUATIONS

*Price block===============================================
 PMDEF(C)       domestic import price
 PEDEF(C)       domestic export price
 PDDDEF(C)      dem price for com'y c produced and sold domestically
 PQDEF(C)       value of sales in domestic market
 PXDEF(C)       value of marketed domestic output
 PADEF(A)       output price for activity a
 PINTADEF(A)    price of aggregate intermediate input
*** INVESTCGE
 PINTADEFENE(A) price of aggregate intermediate input - energy version
*** /INVESTCGE
 PVADEF(A)      value-added price
*** INVESTCGE
 PVAEDEFENE(A)  VAE price - energy version
*** /INVESTCGE
 CPIDEF         consumer price index
 DPIDEF         domestic producer price index

*Production and trade block================================
 CESAGGPRD(A)    CES aggregate prod fn (if CES top nest)
 CESAGGFOC(A)    CES aggregate first-order condition (if CES top nest)
 LEOAGGINT(A)    Leontief aggreg intermed dem (if Leontief top nest)
*** INVESTCGE
 LEOAGGINTENE(A)    Leontief aggreg intermed dem (if Leontief top nest) - energy version
*** /INVESTCGE
 LEOAGGVA(A)     Leontief aggreg value-added dem (if Leontief top nest)
*** INVESTCGE
 LEOAGGVAE(A)     Leontief aggreg value-added dem (if Leontief top nest) - energy version
*** /INVESTCGE
 CESVAPRD(A)     CES value-added production function
 CESVAFOC(F,A)   CES value-added first-order condition
*** INVESTCGE
 CESVAPRDENE(A)     CES value-added production function - energy version
 CESVAFOCENE(F,A)   CES value-added first-order condition - energy version

 CESVAEPRD(A)     CES VA-Energy production function - energy version

 CESVAEFOC_ENE(A)   CES ENE VA-Energy first-order condition - energy version
 CESVAEFOC_VA(A)   CES VA VA-Energy first-order condition - energy version
 LEOAGGVA2(A)     Leontief aggreg value-added dem (if Leontief VAE nest)
 LEOAGGPVAE(A)     Leontief aggreg of PVAE (if Leontief VAE nest)

 CESENEPRD(A)     CES VA-Energy production function - energy version
 CESENEFOC(C,A)   CES VA-Energy first-order condition - energy version

 LEOAGGINTPROJ(C,APROJ) Leontief aggreg of interm dem in project activity APROJ
 LEOAGGFACTPROJ(F,APROJ) Leontief aggreg of factors in project activity APROJ
*** /INVESTCGE
 INTDEM(C,A)     intermediate demand for commodity c from activity a
*** INVESTCGE
 INTDEMENE(C,A)     intermediate demand for commodity c from activity a - energy version
*** /INVESTCGE
 COMPRDFN(A,C)   production function for commodity c and activity a
 OUTAGGFN(C)     output aggregation function
 OUTAGGFOC(A,C)  first-order condition for output aggregation function
 CET(C)          CET function
 CET2(C)         domestic sales and exports for outputs without both
 ESUPPLY(C)      export supply
 ARMINGTON(C)    composite commodity aggregation function
 COSTMIN(C)      first-order condition for composite commodity cost min
 ARMINGTON2(C)   comp supply for com's without both dom. sales and imports
 QTDEM(C)        demand for transactions (trade and transport) services

*Institution block ========================================
 YFDEF(F)        factor incomes
 YIFDEF(INS,F)   factor incomes to domestic institutions
 YIDEF(INS)      total incomes of domest non-gov't institutions
 EHDEF(H)        household consumption expenditures
 TRIIDEF(INS,INSP) transfers to inst'on ins from inst'on insp
 HMDEM(C,H)      LES cons demand by hhd h for marketed commodity c
 HADEM(A,C,H)    LES cons demand by hhd h for home commodity c fr act a
 INVDEM(C)       fixed investment demand
 GOVDEM(C)       government consumption demand
 EGDEF           total government expenditures
*** INVESTCGE
 YGPROJDEF       government income from project capital rents
*** /INVESTCGE
 YGDEF           total government income
*** INVESTCGE
 TAXINCOME       government tax income
 PROJEXPENDITURE(APROJ) Project actual expenditure
 PRIVSAV         total private savings
 INVVAL          total investment value
*** /INVESTCGE
*System constraint block===================================
 COMEQUIL(C)     composite commodity market equilibrium
 FACEQUIL(F)     factor market equilibrium
*** INVESTCGE
 LABSUPPLY(FLAB) labour factor supply
 LANDSUPPLY(FLND) land supply function
*** /INVESTCGE
 CURACCBAL       current account balance (of RoW)
 GOVBAL          government balance
 TINSDEF(INS)    direct tax rate for inst ins
 MPSDEF(INS)     marg prop to save for inst ins
*** INVESTCGE
 INVESTMENT      total investment
 KSTOCKEQ          capital stock post simulation
 CAPGROWTH       capital stock growth rate
*** /INVESTCGE
 SAVINVBAL       savings-investment balance
 TABSEQ          total absorption
 INVABEQ         investment share in absorption
 GDABEQ          government consumption share in absorption
 OBJEQ           Objective function
;


*8. EQUATION DEFINITIONS ############################################
*Notational convention inside equations:
*Parameters and "invariably" fixed variables are in lower case.
*"Variable" variables are in upper case.

*Price block===============================================

 PMDEF(C)$CM(C)..
  PM(C) =E= pwm(C)*(1 + tm(C))*EXR + SUM(CT, PQ(CT)*icm(CT,C));

 PEDEF(C)$CE(C)..
  PE(C) =E= pwe(C)*(1 - te(C))*EXR - SUM(CT, PQ(CT)*ice(CT,C));

 PDDDEF(C)$CD(C).. PDD(C) =E= PDS(C) + SUM(CT, PQ(CT)*icd(CT,C));

 PQDEF(C)$(CD(C) OR CM(C))..
       PQ(C)*(1 - tq(c))*QQ(C) =E= PDD(C)*QD(C) + PM(C)*QM(C);

 PXDEF(C)$CX(C)..  PX(C)*QX(C) =E= PDS(C)*QD(C) + PE(C)*QE(C);

 PADEF(A)..  PA(A) =E= SUM(C, PXAC(A,C)*theta(A,C));

 PINTADEF(A).. PINTA(A) =E= SUM(C, PQ(C)*ica(C,A)) ;

 PVADEF(A)..  PA(A)*(1-ta(A))*QA(A) =E= PVA(A)*QVA(A) + PINTA(A)*QINTA(A) ;

*** INVESTCGE
 PINTADEFENE(A).. PINTA(A) =E= SUM(CNENE, PQ(CNENE)*ica2(CNENE,A)) ;

 PVAEDEFENE(A)..  PA(A)*(1-ta(A))*QA(A) =E= PVAE(A)*QVAE(A) + PINTA(A)*QINTA(A) ;
*** /INVESTCGE

 CPIDEF..  CPI =E= SUM(C, cwts(C)*PQ(C)) ;

 DPIDEF..  DPI =E= SUM(CD, dwts(CD)*PDS(CD)) ;


*Production and trade block================================

*CESAGGPRD and CESAGGFOC apply to activities with CES function at
*top of technology nest.

 CESAGGPRD(A)$ACES(A)..
   QA(A) =E= alphaa(A)*(deltaa(A)*QVA(A)**(-rhoa(A))
               + (1-deltaa(A))*QINTA(A)**(-rhoa(A)))**(-1/rhoa(A)) ;

 CESAGGFOC(A)$ACES(A)..
   QVA(A) =E= QINTA(A)*((PINTA(A)/PVA(A))*(deltaa(A)/
                                 (1 - deltaa(A))))**(1/(1+rhoa(A))) ;


*LEOAGGINT and LEOAGGVA apply to activities with Leontief function at
*top of technology nest.

 LEOAGGINT(A)$ALEO(A)..  QINTA(A) =E= inta(A)*QA(A) ;

 LEOAGGVA(A)$ALEO(A)..  QVA(A) =E= iva(A)*QA(A) ;

*** INVESTCGE
 LEOAGGINTENE(A)$ALEO(A)..  QINTA(A) =E= inta2(A)*QA(A) ;

 LEOAGGVAE(A)$ALEO(A)..  QVAE(A) =E= ivae(A)*QA(A) ;
*** /INVESTCGE

*CESVAPRD, CESVAFOC, INTDEM apply at the bottom of the technology nest
*(for all activities).

*** INVESTCGE
 CESVAPRD(A)$AVACES(A)..
   QVA(A) =E= alphava(A)*(SUM(F,
                      deltava(F,A)*QF(F,A)**(-rhova(A))) )**(-1/rhova(A)) ;
*** /INVESTCGE

*** INVESTCGE
 CESVAPRDENE(A)$AVACES(A)..
   QVA(A) =E= alpha2va(A)*(SUM(F,
                      delta2va(F,A)*QF(F,A)**(-rhova(A))) )**(-1/rhova(A)) ;

 CESENEPRD(A)$QENE0(A)..
   QENE(A) =E= alpha2ene(A)*(SUM(CENE,
                      delta2ene(CENE,A)*QINT(CENE,A)**(-rhoene(A))) )**(-1/rhoene(A)) ;

 CESVAEPRD(A)$AVAECES(A)..
   QVAE(A) =E= alphavae(A)*(
                      deltavae_va(A)*QVA(A)**(-rhovae(A)) + deltavae_ene(A)*QENE(A)**(-rhovae(A)) )**(-1/rhovae(A)) ;

 CESVAFOC(F,A)$deltava(F,A)..
   WF(F)*wfdist(F,A) =E=
   PVA(A)*(1-tva(A))
   * QVA(A) * SUM(FP, deltava(FP,A)*QF(FP,A)**(-rhova(A)) )**(-1)
   *deltava(F,A)*QF(F,A)**(-rhova(A)-1);


 CESVAFOCENE(F,A)$delta2va(F,A)..
   WF(F)*wfdist(F,A) =E=
   PVA(A)*(1-tva(A))
   * QVA(A) * SUM(FP, delta2va(FP,A)*QF(FP,A)**(-rhova(A)) )**(-1)
   *delta2va(F,A)*QF(F,A)**(-rhova(A)-1);

 CESENEFOC(CENE,A)$delta2ene(CENE,A)..
   PQ(CENE) =E=
   PENE(A)
   * QENE(A) * SUM(CENEP, delta2ene(CENEP,A)*QINT(CENEP,A)**(-rhoene(A)) )**(-1)
   *delta2ene(CENE,A)*QINT(CENE,A)**(-rhoene(A)-1);

 CESVAEFOC_ENE(A)$AVAECES(A)..
   PENE(A) =E=
   PVAE(A)
   * QVAE(A) * (deltavae_ene(A)*QENE(A)**(-rhovae(A)) + deltavae_va(A)*QVA(A)**(-rhovae(A)) )**(-1)
   *deltavae_ene(A)*QENE(A)**(-rhovae(A)-1);

 CESVAEFOC_VA(A)$AVAECES(A)..
   PVA(A) =E=
   PVAE(A)
   * QVAE(A) * (deltavae_ene(A)*QENE(A)**(-rhovae(A)) + deltavae_va(A)*QVA(A)**(-rhovae(A)) )**(-1)
   *deltavae_va(A)*QVA(A)**(-rhovae(A)-1);

 LEOAGGVA2(A)$AVAELEO(A)..
   QVA(A) =E= deltavae_va(A) * QVAE(A) ;

 LEOAGGPVAE(A)$AVAELEO(A)..
   PVAE(A) =E= deltavae_ene(A) * PENE(A) + deltavae_va(A) * PVA(A) ;

 INTDEM(C,A)$ica(C,A).. QINT(C,A) =E= ica(C,A)*QINTA(A);

 INTDEMENE(CNENE,A)$ica2(CNENE,A).. QINT(CNENE,A) =E= ica2(CNENE,A)*QINTA(A);


* production function for project activity APROJ

 LEOAGGINTPROJ(C,APROJ)..     QINTPROJ(C,APROJ) =E= icaproj(C,APROJ)*plvalue(APROJ)  ;
 LEOAGGFACTPROJ(F,APROJ)..    QFPROJ(F,APROJ)=E= ifact(F,APROJ)*plvalue(APROJ)  ;

 PROJEXPENDITURE(APROJ)..       PROJEXP(APROJ) =E= SUM(C,QINTPROJ(C,APROJ)*PQ(C))+SUM(F,QFPROJ(F,APROJ)*WF(F))   ;


* production function of commodity c from activity a
* = market supply + home supply by households =e= share of output A representing commodity C

 COMPRDFN(A,C)$theta(A,C)..
    QXAC(A,C) + SUM(H, QHA(A,C,H)) =E= theta(A,C)*QA(A) ;

* output aggregation function of commodity C
* = CES aggregation of commodity C output from activities A
 OUTAGGFN(C)$CX(C)..
   QX(C) =E= alphaac(C)*SUM(A, deltaac(A,C)*QXAC(A,C)
                                        **(-rhoac(C)))**(-1/rhoac(C));

 OUTAGGFOC(A,C)$deltaac(A,C)..
   PXAC(A,C) =E=
   PX(C)*QX(C) * SUM(AP, deltaac(AP,C)*QXAC(AP,C)**(-rhoac(C)) )**(-1)
   *deltaac(A,C)*QXAC(A,C)**(-rhoac(C)-1);

 CET(C)$(CE(C) AND CD(C))..
    QX(C) =E= alphat(C)*(deltat(C)*QE(C)**rhot(C) +
                         (1 - deltat(C))*QD(C)**rhot(C))**(1/rhot(C)) ;

  ESUPPLY(C)$(CE(C) AND CD(C))..
   QE(C) =E=  QD(C)*((PE(C)/PDS(C))*
                ((1 - deltat(C))/deltat(C)))**(1/(rhot(C)-1)) ;

 CET2(C)$( (CD(C) AND CEN(C)) OR (CE(C) AND CDN(C)) )..
   QX(C)=E= QD(C) + QE(C);


 ARMINGTON(C)$(CM(C) AND CD(C))..
   QQ(C) =E= alphaq(C)*(deltaq(C)*QM(C)**(-rhoq(C)) +
                      (1 -deltaq(C))*QD(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

 COSTMIN(C)$(CM(C) AND CD(C))..
   QM(C) =E= QD(C)*((PDD(C)/PM(C))*(deltaq(C)/(1 - deltaq(C))))
                                                   **(1/(1 + rhoq(C)));

 ARMINGTON2(C)$( (CD(C) AND CMN(C)) OR (CM(C) AND CDN(C)) )..
   QQ(C) =E= QD(C) + QM(C);

 QTDEM(C)$CT(C)..
  QT(C) =E= SUM(CP, icm(C,CP)*QM(CP)+ ice(C,CP)*QE(CP)+ icd(C,CP)*QD(CP));


*Institution block ========================================

*** INVESTCGE
* income from factor F modification: accounting for factor income during project construction but deducting project generated factor income (assuming project income directed to government)
 YFDEF(F)..  YF(F) =E= SUM(A, WF(F)*wfdist(F,A)*QF(F,A)) + SUM(APROJ,QFPROJ(F,APROJ)*WF(F)) - SUM(APROJ,QFSPROJ(F,APROJ)*WF(F));
*** /INVESTCGE
 YIFDEF(INSD,F)$shif(INSD,F)..
  YIF(INSD,F) =E= shif(INSD,F)*((1-tf(f))*YF(F) - trnsfr('ROW',F)*EXR);

 YIDEF(INSDNG)..
  YI(INSDNG) =E=
   SUM(F, YIF(INSDNG,F))  + SUM(INSDNGP, TRII(INSDNG,INSDNGP))
   + trnsfr(INSDNG,'GOV')*CPI + trnsfr(INSDNG,'ROW')*EXR;

 TRIIDEF(INSDNG,INSDNGP)$(shii(INSDNG,INSDNGP))..
  TRII(INSDNG,INSDNGP) =E= shii(INSDNG,INSDNGP)
           * (1 - MPS(INSDNGP)) * (1 - TINS(INSDNGP))* YI(INSDNGP);

 EHDEF(H)..
  EH(H) =E= (1 - SUM(INSDNG, shii(INSDNG,H))) * (1 - MPS(H))
                                           * (1 - TINS(H)) * YI(H);

 HMDEM(C,H)$betam(C,H)..
   PQ(C)*QH(C,H) =E=
    PQ(C)*gammam(C,H)
        + betam(C,H)*( EH(H) - SUM(CP, PQ(CP)*gammam(CP,H))
                         - SUM((A,CP), PXAC(A,CP)*gammah(A,CP,H))) ;

 HADEM(A,C,H)$betah(A,C,H)..
   PXAC(A,C)*QHA(A,C,H) =E=
     PXAC(A,C)*gammah(A,C,H)
                + betah(A,C,H)*(EH(H) - SUM(CP, PQ(CP)*gammam(CP,H))
                       - SUM((AP,CP), PXAC(AP,CP)*gammah(AP,CP,H))) ;

 INVDEM(C)$CINV(C)..  QINV(C) =E= IADJ*qbarinv(C);

*** INVESTCGE
* government demand modification: accounting for loan-related commodity purchases LOANPROD by the government
 GOVDEM(C)..  QG(C) =E= GADJ*qbarg(C) + SUM(APROJ, LOANPROD(APROJ,C))    ;
* government project income
 YGPROJDEF..
   YGPROJ =E= SUM((F,APROJ),QFSPROJ(F,APROJ)*WF(F)) ;

* government income modification: adding government foreign sales of loan-related commodities LOANPROD at a fixed prices LOANPRODPRICE
* government income modification: adding project financing received as grants or loans
* government income modification: adding government project factor income YGPROJ
 YGDEF..
   YG =E= SUM(INSDNG, TINS(INSDNG)*YI(INSDNG))
          + SUM(f, tf(F)*YF(F))
          + SUM(A, tva(A)*PVA(A)*QVA(A))
          + SUM(A, ta(A)*PA(A)*QA(A))
          + SUM(C, tm(C)*pwm(C)*QM(C))*EXR
          + SUM(C, te(C)*pwe(C)*QE(C))*EXR
          + SUM(C, tq(C)*PQ(C)*QQ(C))
          + SUM(F, YIF('GOV',F))
* sales of resource-secured loan
          + SUM((APROJ,C),loanprod(APROJ,C)*loanprodprice(APROJ,C)*EXR)
          + (trnsfr('GOV','ROW')
* project non-governament financing
          + SUM(APROJ,pfin("GRANT",APROJ) + pfin("FBOR",APROJ)))*EXR + SUM(APROJ,pfin("GBOR",APROJ))
* income from project capital rents
          + YGPROJ;

* government tax income
 TAXINCOME..     YTAX =E= SUM(INSDNG, TINS(INSDNG)*YI(INSDNG))
          + SUM(f, tf(F)*YF(F))
          + SUM(A, tva(A)*PVA(A)*QVA(A))
          + SUM(A, ta(A)*PA(A)*QA(A))
          + SUM(C, tm(C)*pwm(C)*QM(C))*EXR
          + SUM(C, te(C)*pwe(C)*QE(C))*EXR
          + SUM(C, tq(C)*PQ(C)*QQ(C))    ;


* government expenditure modification: adding project spending on demand for project activity and direct project-related investments
* government expenditure modification: adding project-related loan repayments
 EGDEF..
   EG =E= SUM(C, PQ(C)*QG(C)) + SUM(INSDNG, trnsfr(INSDNG,'GOV'))*CPI +
                 SUM(APROJ, SUM(F,QFPROJ(F,APROJ)*WF(F)) + SUM(C,QINTPROJ(C,APROJ)*PQ(C))) +
                 SUM(APROJ,PCOST("ROW",APROJ))*EXR +
                 SUM(APROJ,repayment(APROJ,"FBOR")*EXR +
                 repayment(APROJ,"GBOR"));

*** /INVESTCGE

*System constraint block===================================

*** INVESTCGE
* modifying factor market to included new supply of project capital
 FACEQUIL(F)..  SUM(A, QF(F,A)) + SUM(APROJ, QFPROJ(F,APROJ)) =E= QFS(F) + SUM(APROJ,QFSPROJ(F,APROJ)) ;
*** /INVESTCGE

*** INVESTCGE
* adding a labour supply function
 LABSUPPLY(FLAB)..       QFS(FLAB) =E= (WF(FLAB)/WF0(FLAB))**etals(FLAB)*QFS0(FLAB) ;
*** /INVESTCGE

*** INVESTCGE
* adding a logistic agricultural land supply function
 LANDSUPPLY(FLND)..      QFS(FLND) =E= (1+epsland*exp(-elasland*WF0(FLND)/CPI0)) / (1+epsland*exp(-elasland*WF(FLND)/CPI)) * QFS0(FLND)  ;
*** /INVESTCGE

*** INVESTCGE
* commodity market equilibrium modification: adding project-related demand for intermediate goods
 COMEQUIL(C)..
   QQ(C) =E= SUM(A, QINT(C,A)) + SUM(H, QH(C,H)) + QG(C)
                + QINV(C) + qdst(C) + QT(C) + SUM(APROJ,QINTPROJ(C,APROJ))  ;
*** /INVESTCGE


*** INVESTCGE
* current account balance modification: adding foreign loan repayments and project direct imports
* current account balance modification: adding project financing from abroad in form of loans and grants
 CURACCBAL..
  SUM(C, pwm(C)*QM(C)) + SUM(F, trnsfr('ROW',F)) + SUM(APROJ,repayment(APROJ,"FBOR")) + SUM(APROJ,PCOST("ROW",APROJ)) =E=
         SUM((APROJ,C),loanprod(APROJ,C)*loanprodprice(APROJ,C)) +
         SUM(C, pwe(C)*QE(C)) +
         SUM(INSD, trnsfr(INSD,'ROW')) +
         FSAV +
         sum(APROJ,pfin("GRANT",APROJ)+pfin("FBOR",APROJ))  ;
*** /INVESTCGE

* gov balance
* = gov expenditure and gov savings
 GOVBAL.. YG =E= EG + GSAV  ;

 TINSDEF(INSDNG)..
  TINS(INSDNG) =E= tinsbar(INSDNG)*(1 + TINSADJ*tins01(INSDNG))
                   + DTINS*tins01(INSDNG);

 MPSDEF(INSDNG)..
  MPS(INSDNG)  =E= mpsbar(INSDNG)*(1 + MPSADJ*mps01(INSDNG))
                   + DMPS*mps01(INSDNG);

 INVESTMENT..    QINVTOT =e=  SUM(C, QINV(C) + qdst(C)) + WALRAS ;

 SAVINVBAL..
   SUM(INSDNG, MPS(INSDNG) * (1 - TINS(INSDNG)) * YI(INSDNG))
    + GSAV - sum(APROJ,pfin("GBOR",APROJ)) + FSAV*EXR + SUM(APROJ,repayment(APROJ,"GBOR")) =E=
            SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C)) + WALRAS;
 INVVAL..
         TINV =E=        SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C)) + WALRAS  ;

 PRIVSAV..
   PSAV =E= SUM(INSDNG, MPS(INSDNG) * (1 - TINS(INSDNG)) * YI(INSDNG))   ;
* capital stock accumulation
 KSTOCKEQ..        KSTOCK  =e=  KSTOCKP*(1-depr) + QINVTOT  ;
 CAPGROWTH..     CAPG    =e=  KSTOCK / KSTOCKP   ;

* total absorption
* =  =e= household market demand + household non-market demand + gov demand + investment demand + inventories
 TABSEQ..
  TABS =E=
   SUM((C,H), PQ(C)*QH(C,H)) + SUM((A,C,H), PXAC(A,C)*QHA(A,C,H))
  + SUM(C, PQ(C)*QG(C)) + SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C));

* investment share of absorbtion
* = =e= investment demand + invetories
 INVABEQ.. INVSHR*TABS =E= SUM(C, PQ(C)*QINV(C)) + SUM(C, PQ(C)*qdst(C)) ;

* government consumption share in absorbtion
* = =e= government demand
 GDABEQ..  GOVSHR*TABS =E= SUM(C, PQ(C)*QG(C)) + SUM(APROJ,pfin("GCSPEND",APROJ)) + SUM((APROJ,FIN),repayment(APROJ,FIN)) ;

 OBJEQ..   WALRASSQR   =E= WALRAS*WALRAS ;



*9. MODEL DEFINITION ###############################################

 MODEL STANDCGE  standard CGE model
 /
*Price block (10)
 PMDEF.PM
 PEDEF.PE
 PQDEF.PQ
 PXDEF.PX
 PDDDEF.PDD
 PADEF.PA
 PINTADEF.PINTA
 PVADEF.PVA
 CPIDEF
 DPIDEF

*Production and trade block (17)
 CESAGGPRD
 CESAGGFOC
 LEOAGGINT
 LEOAGGVA
 LEOAGGINTPROJ
 LEOAGGFACTPROJ
 CESVAPRD.QVA
 CESVAFOC
 INTDEM.QINT
 COMPRDFN.PXAC
 OUTAGGFN.QX
 OUTAGGFOC.QXAC
 CET
 CET2
 ESUPPLY
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM.QT

*Institution block (12)
 YFDEF.YF
 YIFDEF.YIF
 YIDEF.YI
 EHDEF.EH
 TRIIDEF.TRII
 HMDEM.QH
 HADEM.QHA
 EGDEF.EG
 YGDEF.YG
 YGPROJDEF.YGPROJ
 PROJEXPENDITURE.PROJEXP
 GOVDEM.QG
 GOVBAL
 INVDEM.QINV

*System-constraint block (9)
 FACEQUIL
 LABSUPPLY
 COMEQUIL
 CURACCBAL
 TINSDEF.TINS
 MPSDEF.MPS
 SAVINVBAL.WALRAS
 INVESTMENT.QINVTOT
 KSTOCKEQ.KSTOCK
 CAPGROWTH.CAPG
 TABSEQ.TABS
 INVABEQ
 GDABEQ
 /
 ;

$include energy_model_def.inc

*10. FIXING VARIABLES NOT IN MODEL AT ZERO ##########################

  PDD.FX(C)$(NOT CD(C)) = 0;
  PDS.FX(C)$(NOT CD(C)) = 0;
  PE.FX(C)$(NOT CE(C)) = 0;
  PM.FX(C)$(NOT CM(C)) = 0;
  PX.FX(C)$(NOT CX(C)) = 0;
  PXAC.FX(A,C)$(NOT SAM(A,C)) = 0;

  QD.FX(C)$(NOT CD(C)) = 0;
  QE.FX(C)$(NOT CE(C)) = 0;
  QF.FX(F,A)$(NOT SAM(F,A)) = 0;
*  QG.FX(C)$(NOT SAM(C,'GOV')) = 0;
  QH.FX(C,H)$(NOT SAM(C,H)) = 0;
  QHA.FX(A,C,H)$(NOT BETAH(A,C,H)) = 0;
  QINT.FX(C,A)$(NOT SAM(C,A)) = 0;
  QINV.FX(C)$(NOT CINV(C)) = 0;
  QM.FX(C)$(NOT CM(C)) = 0;
  QQ.FX(C)$(NOT (CD(C) OR CM(C))) = 0;
  QT.FX(C)$(NOT CT(C)) = 0;
  QX.FX(C)$(NOT CX(C)) = 0;
  QXAC.FX(A,C)$(NOT SAM(A,C)) = 0;
  TRII.FX(INSDNG,INSDNGP)$(NOT SAM(INSDNG,INSDNGP)) = 0;
  YI.FX(INS)$(NOT INSD(INS)) = 0;
  YIF.FX(INS,F)$((NOT INSD(INS)) OR (NOT SAM(INS,F))) = 0;

  KSTOCKP.FX = KSTOCKP0  ;

*11. MODEL CLOSURE ##################################################
$ontext
In the simulation file, SIM.GMS, the user chooses between
alternative closures. Those choices take precedence over the choices
made in this file.

In the following segment, closures is selected for the base model
solution in this file. The clearing variables for micro and macro
constraints are as follows:

FACEQUIL - WF: for each factor, the economywide wage is the
market-clearing variable in a setting with perfect factor mobility across
activities.

CURACCBAL - EXR: a flexible exchange rate clears the current account of
the RoW.

GOVBAL - GSAV: flexible government savings clears the government
account.

SAVINVBAL - SADJ: the savings rates of domestic institutions are scaled
to generate enough savings to finance exogenous investment quantities
(investment-driven savings).

The CPI is the model numeraire.
$offtext


* REPAYMENT("A-PROJ","GBOR") = 10 ;
* LOANPROD("A-PROJ","C-AGR") = 10 ;
* PLVALUE("A-PROJ") =     100       ;
* PCOST(AC,APROJ)$sPCOST(AC,APROJ)    =       sPCOST(AC,APROJ)*PLVALUE(APROJ)      ;

*Factor markets=======

  QFS.L(FLAB)      = QFS0(FLAB);
  QFS.L(FLND)      = QFS0(FLND);
  QFS.FX(F)$(not FLAB(F) and not FLND(F))        = QFS0(F);
  QFSPROJ.FX(F,APROJ)            = 0 ;
  WF.LO(F)        = -inf;
  WF.UP(F)        = +inf;
  WFDIST.FX(F,A)  = WFDIST0(F,A);
* WFDIST.LO(F,A)  = -INF;
* WFDIST.UP(F,A)  = +INF;


*Current account of RoW===========

* EXR.FX       = EXR0;
 FSAV.FX      = FSAV0;

*Import and export prices (in FCU) are fixed. A change in model
*specification is required if these prices are to be endogenous.
  PWM.FX(C)    = PWM0(C) ;
  PWE.FX(C)    = PWE0(C) ;


*Current government balance=======

*GSAV.FX     = GSAV0 ;
 TINSADJ.FX  = TINSADJ0;
 DTINS.FX    = DTINS0;
* GADJ.FX     = GADJ0;
GOVSHR.FX   = GOVSHR0 ;


*Savings-investment balance=======

 MPSADJ.FX = MPSADJ0;
 DMPS.FX   = DMPS0;
* IADJ.FX   = IADJ0;
* INVSHR.FX = INVSHR0 ;


*Numeraire price index============

 CPI.FX        = CPI0;
*DPI.FX        = DPI0;


*12. DISPLAY OF MODEL PARAMETERS AND VARIABLES ######################



$ontext

DISPLAY
*All parameters in this file and include files are displayed in
*alphabetical order.

ALPHAA   , ALPHAVA0  , ALPHAAC  , ALPHAQ   , ALPHAT    , ALPHAVA
BETAH    , BETAM     , BUDSHR   , BUDSHR2  , BUDSHRCHK , CPI0
CUTOFF   , CWTS      , CWTSCHK  , DELTAA   , DELTAAC   , DELTAQ
DELTAT   , DELTAVA   , DPI0     , DMPS0    , DTINS0    , DWTS
DWTSCHK  , EG0       , EH0      , ELASAC   , ELASCHK   , EXR0
FRISCH   , FSAV0     , GADJ0    , GAMMAH   , GAMMAM    , GOVSHR0
GSAV0    , IADJ0     , ICA      , ICD      , ICE       , ICM
INTA     , INVSHR0   , IVA      , LESELAS1 , LESELAS2  , MPS0
MPSADJ0  , MPSBAR    , PA0      , PDD0     , PDS0      , PE0
PINTA0   , PM0       , POP      , PQ0      , PRODELAS  , PRODELAS2
PVA0     , PWE0      , PWM0     , PX0      , PXAC0     , QA0
QBARG    , QBARG0    , QBARINV  , QD0      , QDST      , QDST0
QE0      , QF0       , QF2BASE  , QFBASE   , QFS0      , QFSBASE
QG0      , QH0       , QHA0     , QINT0    , QINTA0    , QINV0
QM0      , QQ0       , QT0      , QVA0     , QX0       , QXAC0
RHOA     , RHOAC     , RHOQ     , RHOT     , RHOVA     , SAM
SAMBALCHK, SHCTD     , SHCTE     , SHCTM    , SHIF     , SHIFCHK
SHII     , SHRHOME   , SUMABSDEV , SUPERNUM , TA       , TA0
TABS0    , TAXPAR   , TE        , TE0      , TF        , TF0
THETA    , TINS0     , TINSADJ0 , TINSBAR  , TM        , TM0
TQ       , TQ0       , TRADELAS , TRII0    , TRNSFR    , TVA
TVA0     , WALRAS0   , WF0      , WFA      , WFDIST0   , YF0
YG0      , YI0       , YIF0
;

$offtext
*13. SOLUTION STATEMENT #############################################

OPTIONS ITERLIM = 1000, LIMROW = 6, LIMCOL = 6, SOLPRINT=ON,
        MCP=PATH, NLP=CONOPT, CNS=IPOPT;

$ontext
These options are useful for debugging. When checking whether the
initial data represent a solution, set LIMROW to a value greater than
the number of equations and search for three asterisks in the listing
file. SOLPRINT=ON provides a complete listing file. The program also
has a number of display statements, so when running experiments it is
usually not necessary to provide a solution print as well.
$offtext

 STANDCGE.HOLDFIXED   = 1 ;
 STANDCGE.TOLINFREP   = .0001 ;

 ENERGYCGE.HOLDFIXED   = 1 ;
 ENERGYCGE.TOLINFREP   = .0001 ;

$ontext
The HOLDFIXED option converts all variables which are fixed (.FX) into
parameters. They are then not solved as part of the model.
The TOLINFREP parameter sets the tolerance for determinining whether
initial values of variables represent a solution of the model
equations. Whether these initial equation values are printed is
determimed by the LIMROW option. Equations which are not satsfied to
the degree TOLINFREP are printed with three asterisks next to their
listing.
$offtext


* SOLVE STANDCGE USING MCP ;
 SOLVE ENERGYCGE USING MCP ;

 display QA.L, QA0, QINT.L, QINT0, QVA.L, QVA0, PA.L, PA0, PINTA.L, PINTA0, PVA.L, PVA0, QINTA.L, QINTA0;

*$stop

*** base-mode post-simulation calculations

parameters dQQ(C)        Changes in commodity demand     ;


 dQQ(C)  =       QQ.L(C) - QQ0(C)        ;

display dQQ      ;

display WALRAS.L ;

ABORT$(WALRAS.L >10**(-6)) "WALRAS inconsistency - calibration run" ;


*$stop

SETS

 IGDPX  Items for GDP and national accounts
  /
  ABSORP   absorption
  PRVCON   private consumption
  FIXINV   fixed investment
  DSTOCK   stock change
  GOVCON   government consumption
  PROJINV  project investment
  EXPORTS  exports
  IMPORTS  imports
  GDPCP    GDP at constant prices (real GDP)
  GDPMP    GDP at market prices (alt. 1: spending)
  GDPMP2   GDP at market prices (alt. 2: value-added)
  NETITAX  net indirect taxes
  GDPFC2   GDP at factor cost
  /

 IGDPXX(IGDPX)  Items for GDPMP summation
  /
  PRVCON   private consumption
  FIXINV   fixed investment
  PROJINV  project investment
  DSTOCK   stock change
  GOVCON   government consumption
  EXPORTS  exports
  IMPORTS  imports
  /

 KGDPX  second index in GDP tables
 /
 VALUE, RVALUE, PERC-GDP, PERC-TOT
 /       ;

PARAMETERS
 GDPBASE0(IGDPX,KGDPX) Initial aggregate national accounts summary
 GDPERR                error if alt GDP definitions are not identical
 ;
*================================

PARAMETERS
         GDPBASE(IGDPX,KGDPX)  Simulation aggregate national accounts summary

         GDPBASE_T(IGDPX,KGDPX,T)  Simulation aggregate national accounts summary
         GDPERR                error if alt GDP definitions are not identical

         GDPBASE_index_T(IGDPX,KGDPX,T) Changes in aggregate national accounts summary
         QF_index_T(F,A,T)   Change in factor use by activity
         QFS_index_T(F,T)     Change in level of factor supply
         QA_index_T(A,T)     Change in level of activity
         QD_index_T(C,T)     Change in level of domestic supply
         QE_index_T(C,T)     Change in level of exports
         QM_index_T(C,T)     Change in level of imports
         QH_index_T(C,H,T)   Change in level of household demand
         YI_index_T(H,T)     Change in level of household income

         INSDNG_INCOME_T(INSDNG,*,T)    Non-government institution income

 ;
*================================

 GDPBASE0('PRVCON','VALUE')
   =  SUM((C,H), PQ.L(C)*QH.L(C,H))
      + SUM((A,C,H), PXAC.L(A,C)*QHA.L(A,C,H));

 GDPBASE0('FIXINV','VALUE')  = SUM(C, PQ.L(C)*QINV.L(C));
 GDPBASE0('DSTOCK','VALUE')   = SUM(C, PQ.L(C)*QDST(C));
 GDPBASE0('GOVCON','VALUE')  = SUM(C, PQ.L(C)*QG.L(C));
 GDPBASE0('PROJINV','VALUE')  = SUM(APROJ, SUM(F,QFPROJ.L(F,APROJ)*WF.L(F)) + SUM(C,QINTPROJ.L(C,APROJ)*PQ.L(C)));
 GDPBASE0('EXPORTS','VALUE') = SUM(CE, PWE.L(CE)*EXR.L*QE.L(CE));
 GDPBASE0('IMPORTS','VALUE') = -SUM(CM, PWM.L(CM)*EXR.L*QM.L(CM));
 GDPBASE0('GDPMP','VALUE') = SUM(IGDPXX, GDPBASE0(IGDPXX,'VALUE'));

 GDPBASE0('ABSORP','VALUE')
  = GDPBASE0('GDPMP','VALUE') - GDPBASE0('IMPORTS','VALUE')
    - GDPBASE0('EXPORTS','VALUE');

 GDPBASE0('GDPFC2','VALUE') = SUM(A, PVA.L(A)*(1-tva(A))*QVA.L(A)) + SUM((APROJ,F),QFPROJ.L(F,APROJ)*WF.L(F));

 GDPBASE0('NETITAX','VALUE') =
            SUM(A, ta(A)*PA.L(A)*QA.L(A))
          + SUM(A, tva(A)*PVA.L(A)*QVA.L(A))
          + SUM(CM, tm(CM)*QM.L(CM)*PWM.L(CM))*EXR.L
          + SUM(CE, te(CE)*QE.L(CE)*PWE.L(CE))*EXR.L
          + SUM(C, tq(C)*PQ.L(C)*QQ.L(C));

 GDPBASE0('GDPMP2','VALUE')
  = GDPBASE0('GDPFC2','VALUE') + GDPBASE0('NETITAX','VALUE');

 GDPERR$(ABS(GDPBASE0('GDPMP2','VALUE') - GDPBASE0('GDPMP','VALUE')) GT 0.00001)
  = 1/0;
*  = GDPBASE0('GDPMP2','VALUE') - GDPBASE0('GDPMP','VALUE');

 GDPBASE0(IGDPX,'PERC-GDP')$GDPBASE0('GDPMP','VALUE')
  = 100*GDPBASE0(IGDPX,'VALUE')/GDPBASE0('GDPMP','VALUE');


**** Real GDP calculations / GDP at constant prices

 GDPBASE0('PRVCON','RVALUE')
   =  SUM((C,H), PQ0(C)*QH.L(C,H))
      + SUM((A,C,H), PXAC0(A,C)*QHA.L(A,C,H));

 GDPBASE0('FIXINV','RVALUE')  = SUM(C, PQ0(C)*QINV.L(C));
 GDPBASE0('DSTOCK','RVALUE')   = SUM(C, PQ0(C)*QDST(C));
 GDPBASE0('GOVCON','RVALUE')  = SUM(C, PQ0(C)*QG.L(C));
 GDPBASE0('PROJINV','RVALUE')  = SUM(APROJ, SUM(F,QFPROJ.L(F,APROJ)*WF0(F)) + SUM(C,QINTPROJ.L(C,APROJ)*PQ0(C)));
 GDPBASE0('EXPORTS','RVALUE') = SUM(CE, PWE0(CE)*EXR0*QE.L(CE));
 GDPBASE0('IMPORTS','RVALUE') = -SUM(CM, PWM0(CM)*EXR0*QM.L(CM));
 GDPBASE0('GDPCP','RVALUE') = SUM(IGDPXX, GDPBASE0(IGDPXX,'RVALUE'));

 GDPBASE0('ABSORP','RVALUE')
  = GDPBASE0('GDPCP','RVALUE') - GDPBASE0('IMPORTS','RVALUE')
    - GDPBASE0('EXPORTS','RVALUE');

 GDPBASE0('GDPFC2','RVALUE') = SUM(A, PVA0(A)*(1-tva(A))*QVA.L(A)) + SUM((APROJ,F),QFPROJ.L(F,APROJ)*WF0(F));

 GDPBASE0('NETITAX','RVALUE') =
            SUM(A, ta(A)*PA0(A)*QA.L(A))
          + SUM(A, tva(A)*PVA0(A)*QVA.L(A))
          + SUM(CM, tm(CM)*QM.L(CM)*PWM0(CM))*EXR0
          + SUM(CE, te(CE)*QE.L(CE)*PWE0(CE))*EXR0
          + SUM(C, tq(C)*PQ0(C)*QQ.L(C));




*==================

OPTION GDPBASE0:6;
DISPLAY GDPERR, GDPBASE0;

*$stop

***** scenario definition

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

 sPFIN(FIN,APROJ)     =       0       ;
 PVALUE("A-PROJ")  =       %pvalue_A%      ;
 PVALUE("B-PROJ")  =       %pvalue_B%      ;

* REXR    =       3.5       ;
 PLVALUE(APROJ) =       PVALUE(APROJ) * REXR   ;

* sPFIN("GCSPEND","A-PROJ")        =       0       ;
* sPFIN("GSSPEND","A-PROJ")        =       0       ;
* sPFIN("GBOR","A-PROJ")           =       0       ;
* sPFIN("FBOR","A-PROJ")           =       0       ;
* sPFIN("GRANT","A-PROJ")          =       0       ;
*sPFIN(PFIN,"A-PROJ")        =       0       ;



 sPFIN("GCSPEND","A-PROJ")        =       %fin_gspend_A%       ;
 sPFIN("GSSPEND","A-PROJ")        =       %fin_gsave_A%       ;
 sPFIN("GBOR","A-PROJ")           =       %fin_gbor_A%       ;
 sPFIN("FBOR","A-PROJ")           =       %fin_fbor_A%       ;
 sPFIN("GRANT","A-PROJ")          =       %fin_grant_A%       ;

 sPFIN("GCSPEND","B-PROJ")        =       %fin_gspend_B%       ;
 sPFIN("GSSPEND","B-PROJ")        =       %fin_gsave_B%       ;
 sPFIN("GBOR","B-PROJ")           =       %fin_gbor_B%       ;
 sPFIN("FBOR","B-PROJ")           =       %fin_fbor_B%       ;
 sPFIN("GRANT","B-PROJ")          =       %fin_grant_B%       ;


* default financing if all financing sources are set to zero
 sPFIN("GRANT",APROJ)$(not SUM(FIN,sPFIN(FIN,APROJ)))    =       1       ;

 PCOST(AC,APROJ)$sPCOST(AC,APROJ)    =       sPCOST(AC,APROJ)*PLVALUE(APROJ)      ;
 PFIN(FIN,APROJ)       =       sPFIN(FIN,APROJ)*PLVALUE(APROJ)      ;

 INVSTART(APROJ)      =       1 ;
 CONSPER(APROJ)       =       5 ;
* OPSTART("A-PROJ")       =       2 ;
* OPPER("A-PROJ")         =       40 ;
 GRACEPER(APROJ,FIN)  =       0 ;

 REPAYPER("A-PROJ",FIN)  =       %repayper_A% ;
 REPAYPER("B-PROJ",FIN)  =       %repayper_B% ;

* default interest rate for government bonds
 r(APROJ,"GBOR")      =       0.025 ;


 r("A-PROJ","FBOR")      =       %interest_A% ;
 r("B-PROJ","FBOR")      =       %interest_B% ;

display r;

set SIMT(T)    Simulation years
 /2009*2067/
;

parameters       PFIN_AUX(FIN,APROJ)     Auxiliary variable for PFIN -- local currency
                 PLVALUE_AUX(APROJ)      Auxiliary variable for PLVALUE -- local currency
                 REPAYMENT_AUX(APROJ,FIN)        Auxiliary variable for REPAYMENT -- local currency
                 PCOST_AUX(AC,APROJ)        Auxiliary variable for PCOST -- local currency
                 PFIN_T(FIN,APROJ,T)       Project finance over time -- local currency
                 PROJEXP_T(APROJ,T)        Project expenditure over time - direct imports deducted  -- local currency
                 PLVALUE_T(APROJ,T)        Project investment over time -- local currency
                 PCOST_T(AC,APROJ,T)       Project cost over over time -- local currency
                 REPAYMENT_T(APROJ,FIN,T)  Repayment over time -- local currency
                 LOANPROD_T(APROJ,C,T)     Production for resource-secured loan
                 LOANPROD_AUX(APROJ,C)     Annual production for resource-secured loan
                 KSTOCK0                 Base year capital stock
                 KSTOCK_PREV             Capital stock from previous model run
                 dKSTOCK                 Change in KSTOCK due to project investment
                 aINV                    KSTOCK to investment ratio      /11.5/
                 dep                     KSTOCK depreciation rate        /0.04/

                 GSAV_T(T)               Government savings
                 PSAV_T(T)               Total private savings
                 FSAV_T(T)               Foreign savings
                 TINV_T(T)               Total investment value
                 GADJ_T(T)               Government spending adjustement
                 EG_T(T)                 Government expenditure
                 YG_T(T)                 Government income
                 YTAX_T(T)               Government tax income
                 YGPROJ_T(T)             Government factor income from project operation
                 EH_T(H,T)               Household expenditure
                 YI_T(INSDNG,T)          Household income
                 YIF_T(INSDNG,F,T)            Household income by factor
                 CPI_T(T)                Consumer price index
                 INVSHR_T(T)
                 TABS_T(T)               Total absorbtion over time
                 INVVAL_T(T)             Total investment value
                 GOVSHR_T(T)
                 YF_T(F,T)               Factor income
                 PA_T(A,T)               Output price of activity
                 PE_T(C,T)               Export price in local currency
                 PD_T(C,T)               Domestic price of domestic commodity
                 PX_T(C,T)               Output price of domestic commodity
                 PXAC_T(A,C,T)           Domestic price of commodity
                 PQ_T(C,T)               Consumer price of commodity
                 QA_T(A,T)               Activity output level
                 QD_T(C,T)               Domestic supply of commodity
                 QX_T(C,T)               Domestic production of commodity
                 QM_T(C,T)               Imported quantity
                 QE_T(C,T)               Exported quantity
                 QINV_T(C,T)             Investment demand of commodity
                 QINVTOT_T(T)            Toatl investment demand
                 WF_T(F,T)               Factor rent
                 EXR_T(T)                Exchange rate local currency to international currency
                 WALRAS_T(T)             Walras - savings-investment imbalance
                 QG_T(C,T)               Government demand of commodity
                 QH_T(C,H,T)             Household demand of commodity
                 MPS_T(INS,T)
                 IADJ_T(T)               Investment spending adjustment
                 QFS_T(F,T)              Factor supply
                 QF_T(F,AC,T)            Factor demand by activity
                 QINT_T(C,AC,T)          Intermediate demand by activity
                 QFSPROJ_T(F,APROJ,T)    Factor demand by project
                 pop_delta(T)            Active population change

;

set INVCAP(FCAP)       / CAP-AGR, CAP-OTH        /;
alias (INVCAP,INVCAPP)   ;

parameter        ror(INVCAP)     relative rate of return of INVCAP
                 delta_qfs(INVCAP)       change in capital stock of INVCAP
                 rebase_qfs(INVCAP)       rebasing changes in capital stock of INVCAP
                 delta_stock     change in total general capital stock to be allocated across INVCAP     ;



$gdxin ghana_population.gdx
$load pop_delta

display pop_delta;

 REPAYMENT(APROJ,FIN)$(PFIN(FIN,APROJ) and r(APROJ,FIN))
         =       ((1+r(APROJ,FIN))**GRACEPER(APROJ,FIN)*PFIN(FIN,APROJ)) * r(APROJ,FIN) /
                 (1-(1+r(APROJ,FIN))**(-REPAYPER(APROJ,FIN)))    ;

*> cocoa exports 34000tons above 835466tons prod in 2013. SAM 2013 for coco 4518.29 GHS => 5400 GHS/t, or 183.64M GHS exports at constant prices / scaled down to 18.364 to match SAM

 LOANPROD_AUX("A-PROJ","C-AGR")   = %loanprod_A%*REXR        ;

*> question: negotiated production export price reflective of market price?
 LOANPRODPRICE("A-PROJ","C-AGR")  = 1             ;


 PFIN_AUX(FIN,APROJ)     =       PFIN(FIN,APROJ) ;
 PLVALUE_AUX(APROJ)      =       PLVALUE(APROJ)  ;
 REPAYMENT_AUX(APROJ,FIN)=       REPAYMENT(APROJ,FIN) ;
 PCOST_AUX(AC,APROJ)     =       PCOST(AC,APROJ)     ;
* REPAYMENT(APROJ,FIN)    =       0       ;
 PFIN(FIN,APROJ) =       0       ;
 PLVALUE(APROJ)  =       0       ;
 REPAYMENT(APROJ,FIN)=   0       ;
 PCOST(AC,APROJ) =       0       ;

 KSTOCK0         =       aINV*SUM(C,QINV0(C))      ;
 KSTOCK_PREV     =       KSTOCK0 ;

*> reading operation performance values

set run /RUN1*RUN50/ ;

parameter hydro_output(*,T,run)
          delta_hydro_output(*,T,run)  ;
$gdxin hydro_output.gdx
$load hydro_output=h_output, delta_hydro_output=delta_h_output

display hydro_output, delta_hydro_output ;

*$stop
loop(SIMT,

 PFIN(FIN,APROJ) =       0       ;
 PLVALUE(APROJ)  =       0       ;
 REPAYMENT(APROJ,FIN)=   0       ;
 LOANPROD(APROJ,C)     =       0       ;
 PCOST(AC,APROJ) =       0       ;

 PLVALUE(APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))     =       PLVALUE_AUX(APROJ)/CONSPER(APROJ)      ;
 PFIN(FIN,APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))    =       PFIN_AUX(FIN,APROJ)/CONSPER(APROJ)      ;
 PCOST(AC,APROJ)$(ord(SIMT)>=INVSTART(APROJ) AND (ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ))))    =       PCOST_AUX(AC,APROJ)/CONSPER(APROJ)      ;

* LOANPROD(APROJ,C)$(ord(SIMT)>=(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,"FBOR")) AND(ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,"FBOR")+REPAYPER(APROJ,"FBOR"))))    =       LOANPROD_AUX(APROJ,C)      ;
* exports started in 2010
 LOANPROD(APROJ,C)$(ord(SIMT)> 1 AND(ord(SIMT)<(1+REPAYPER(APROJ,"FBOR"))))    =       LOANPROD_AUX(APROJ,C)      ;
 LOANPROD(APROJ,C)$(NOT PFIN_AUX("FBOR",APROJ)) =        0;

 REPAYMENT(APROJ, FIN)$(ord(SIMT)>=(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,FIN)) AND(ord(SIMT)<(INVSTART(APROJ)+CONSPER(APROJ)+GRACEPER(APROJ,FIN)+REPAYPER(APROJ,FIN))))    =       REPAYMENT_AUX(APROJ,FIN)      ;

*** deducting loan collateral exports
 REPAYMENT(APROJ, "FBOR")$PFIN_AUX("FBOR",APROJ) = REPAYMENT(APROJ, "FBOR") ;
* - SUM(C,LOANPROD(APROJ,C)*LOANPRODPRICE(APROJ,C)) ;
display repayment;

if(%base%=0,
$if not set climate $set climate "run1"

*         QFSPROJ.FX("CAP-HY","A-PROJ")   = QFS0("CAP-HY")*(0 + 0.249$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;
*         QFS.FX("CAP-HY")                = QFS0("CAP-HY")*(1 + 0.044$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;


         QFSPROJ.FX("CAP-HY","A-PROJ") = QFS0("CAP-HY")*(0 + delta_hydro_output("bui",SIMT,"%climate%")*delta_hydro_output("akosombo_base",SIMT,"%climate%")  )     ;
         QFS.FX("CAP-HY") = QFS0("CAP-HY")*(0 + delta_hydro_output("akosombo",SIMT,"%climate%")*delta_hydro_output("akosombo_base",SIMT,"%climate%")  )     ;

        QFSPROJ.FX("CAP-HY","A-PROJ")$(ORD(SIMT)>8)   = QFS0("CAP-HY")*(0 + 0.249$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;
        QFS.FX("CAP-HY")$(ORD(SIMT)>8)                = QFS0("CAP-HY")*(1 + 0.044$( (ord(SIMT)>=(INVSTART("A-PROJ")+CONSPER("A-PROJ"))) ) )     ;

display QFSPROJ.L, QFS.L ;
 );

if(%base%=1,
$if not set climate $set climate "run1"

         QFS.FX("CAP-HY")$delta_hydro_output("akosombo_base",SIMT,"%climate%")  = QFS0("CAP-HY")*(0 + delta_hydro_output("akosombo_base",SIMT,"%climate%") )     ;
         QFS.FX("CAP-HY")$(ORD(SIMT)>8)  = 1.0084*QFS0("CAP-HY")     ;

display QFSPROJ.L, QFS.L ;
 );

 PLVALUE_T(APROJ,SIMT)   =       PLVALUE(APROJ)  ;
 PFIN_T(FIN,APROJ,SIMT)  =       PFIN(FIN,APROJ) ;
 PCOST_T(AC,APROJ,SIMT)  =       PCOST(AC,APROJ) ;
 LOANPROD_T(APROJ,C,SIMT)      =       LOANPROD(APROJ,C) ;
 REPAYMENT_T(APROJ,FIN,SIMT) =   REPAYMENT(APROJ,FIN) ;

* SOLVE STANDCGE USING MCP ;
 SOLVE ENERGYCGE USING MCP ;

*** post-simulation calculations

                 GSAV_T(SIMT)       = GSAV.L ;
                 PSAV_T(SIMT)       = PSAV.L ;
                 FSAV_T(SIMT)       = FSAV.L ;
                 TINV_T(SIMT)       = TINV.L ;
                 GADJ_T(SIMT)       = GADJ.L ;
                 EG_T(SIMT)         = EG.L ;
                 YG_T(SIMT)         = YG.L ;
                 YTAX_T(SIMT)       = YTAX.L     ;
                 YGPROJ_T(SIMT)     = YGPROJ.L ;
                 EH_T(H,SIMT)       = EH.L(H) ;
                 YI_T(INSDNG,SIMT)       = YI.L(INSDNG) ;
                 YIF_T(INSDNG,F,SIMT)    = YIF.L(INSDNG,F) ;
                 CPI_T(SIMT)        = CPI.L ;
                 INVSHR_T(SIMT)     = INVSHR.L ;
                 GOVSHR_T(SIMT)     = GOVSHR.L ;
                 YF_T(F,SIMT)       = YF.L(F) ;
                 PA_T(A,SIMT)       = PA.L(A) ;
                 QA_T(A,SIMT)       = QA.L(A) ;
                 QX_T(C,SIMT)       = QX.L(C) ;
                 QD_T(C,SIMT)       = QD.L(C) ;
                 PQ_T(C,SIMT)       = PQ.L(C) ;
                 QINV_T(C,SIMT)     = QINV.L(C) ;
                 QINVTOT_T(SIMT)    = QINVTOT.L ;
                 WF_T(F,SIMT)       = WF.L(F) ;
                 EXR_T(SIMT)        = EXR.L ;
                 WALRAS_T(SIMT)     = WALRAS.L   ;
                 QG_T(C,SIMT)       = QG.L(C) ;
                 QH_T(C,H,SIMT)     = QH.L(C,H) ;
                 MPS_T(INS,SIMT)    = MPS.L(INS) ;
                 IADJ_T(SIMT)       = IADJ.L ;
                 QFS_T(F,SIMT)      = QFS.L(F) ;
                 QM_T(C,SIMT)       = QM.L(C)  ;
                 QE_T(C,SIMT)       = QE.L(C)  ;
                 PE_T(C,SIMT)       = PE.L(C) ;
                 PD_T(C,SIMT)       = PDD.L(C) ;
                 PX_T(C,SIMT)       = PX.L(C) ;
                 PXAC_T(A,C,SIMT)   = PXAC.L(A,C)  ;
                 QFSPROJ_T(F,APROJ,SIMT) = QFSPROJ.L(F,APROJ) ;
                 QF_T(F,A,SIMT)     = QF.L(F,A)  ;
                 QF_T(F,APROJ,SIMT) = QFPROJ.L(F,APROJ) ;
                 QINT_T(C,A,SIMT)      = QINT.L(C,A) ;
                 QINT_T(C,APROJ,SIMT)      = QINTPROJ.L(C,APROJ) ;


                 INSDNG_INCOME_T(INSDNG,F,SIMT)      =       YIF.L(INSDNG,F) ;
                 INSDNG_INCOME_T(INSDNG,INSDNGP,SIMT)      =       TRII.L(INSDNG,INSDNGP) ;
                 INSDNG_INCOME_T(INSDNG,'GOV',SIMT)       =       trnsfr(INSDNG,'GOV')*CPI.L;
                 INSDNG_INCOME_T(INSDNG,'ROW',SIMT)       =       trnsfr(INSDNG,'ROW')*EXR.L;
                 INSDNG_INCOME_T(INSDNG,'TOTAL',SIMT)     =       SUM(F,INSDNG_INCOME_T(INSDNG,F,SIMT) ) + SUM(INSDNGP,INSDNG_INCOME_T(INSDNG,INSDNGP,SIMT)) +  INSDNG_INCOME_T(INSDNG,'GOV',SIMT) + INSDNG_INCOME_T(INSDNG,'ROW',SIMT) ;


* QFS.FX("CAPITAL") =       (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV * QFS.L("CAPITAL") ;


ror(INVCAP) = WF.L(INVCAP) / (SUM(INVCAPP, QFS.L(INVCAPP) * WF.L(INVCAPP)) /SUM(INVCAPP, QFS.L(INVCAPP)))       ;
delta_stock = (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV    ;
delta_qfs(INVCAP)  = delta_stock * ror(INVCAP)**0.5 * QFS.L(INVCAP)       ;

display ror;

 rebase_qfs(INVCAP)       =      1       ;
*(SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV * QFS.L(INVCAP)

 QFS.FX(INVCAP)  =       delta_qfs(INVCAP) * rebase_qfs(INVCAP)  ;


* QFS.FX("CAP-AGR") =       (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV * QFS.L("CAP-AGR") ;
* QFS.FX("CAP-OTH") =       (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) /KSTOCK_PREV * QFS.L("CAP-OTH") ;
 QFS0(FLAB)      = pop_delta(SIMT) * QFS0(FLAB)     ;

* DMPS.FX = 1 - (1.002)**(ord(SIMT)-1);

display DMPS.L   ;

 QFS0("LAND")  = QFS0("LAND")*1.02  ;
* FSAV.FX         = 1.02 * FSAV.L         ;

* dKSTOCK = (SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV) / (SUM(C,QINV0(C)) + (1-dep)*KSTOCK0) ;
* QFS.FX("CAPITAL") = QFS0("CAPITAL")*dKSTOCK ;
* QFS0("CAPITAL")   = QFS.L("CAPITAL") ;
 KSTOCK_PREV = SUM(C,QINV.L(C)) + (1-dep)*KSTOCK_PREV ;


 GDPBASE('PRVCON','VALUE')
          =  SUM((C,H), PQ.L(C)*QH.L(C,H))
             + SUM((A,C,H), PXAC.L(A,C)*QHA.L(A,C,H));

 GDPBASE('FIXINV','VALUE')  = SUM(C, PQ.L(C)*QINV.L(C));
 GDPBASE('DSTOCK','VALUE')   = SUM(C, PQ.L(C)*QDST(C));
 GDPBASE('GOVCON','VALUE')  = SUM(C, PQ.L(C)*QG.L(C));
 GDPBASE('PROJINV','VALUE')  = SUM(APROJ, SUM(F,QFPROJ.L(F,APROJ)*WF.L(F)) + SUM(C,QINTPROJ.L(C,APROJ)*PQ.L(C)));
 GDPBASE('EXPORTS','VALUE') = SUM(CE, PWE.L(CE)*EXR.L*QE.L(CE));
 GDPBASE('IMPORTS','VALUE') = -SUM(CM, PWM.L(CM)*EXR.L*QM.L(CM));
 GDPBASE('GDPMP','VALUE') = SUM(IGDPXX, GDPBASE(IGDPXX,'VALUE'));

 GDPBASE('ABSORP','VALUE')
 = GDPBASE('GDPMP','VALUE') - GDPBASE('IMPORTS','VALUE')
 - GDPBASE('EXPORTS','VALUE');

 GDPBASE('GDPFC2','VALUE') = SUM(A, PVA.L(A)*(1-tva(A))*QVA.L(A));

 GDPBASE('NETITAX','VALUE') =
                   SUM(A, ta(A)*PA.L(A)*QA.L(A))
                 + SUM(A, tva(A)*PVA.L(A)*QVA.L(A))
                 + SUM(CM, tm(CM)*QM.L(CM)*PWM.L(CM))*EXR.L
                 + SUM(CE, te(CE)*QE.L(CE)*PWE.L(CE))*EXR.L
                 + SUM(C, tq(C)*PQ.L(C)*QQ.L(C));

 GDPBASE('GDPMP2','VALUE')
         = GDPBASE('GDPFC2','VALUE') + GDPBASE('NETITAX','VALUE');

* GDPERR$(ABS(GDPBASE('GDPMP2','VALUE') - GDPBASE('GDPMP','VALUE')) GT 0.00001)
*  = 1/0;

* GDPBASE('GDPMP2','VALUE') = 0;


 GDPBASE(IGDPX,'PERC-GDP')$GDPBASE('GDPMP','VALUE')
         = 100*GDPBASE(IGDPX,'VALUE')/GDPBASE('GDPMP','VALUE');


**** Real GDP calculations / GDP at constant prices

 GDPBASE('PRVCON','RVALUE')
   =  SUM((C,H), PQ0(C)*QH.L(C,H))
      + SUM((A,C,H), PXAC0(A,C)*QHA.L(A,C,H));

 GDPBASE('FIXINV','RVALUE')  = SUM(C, PQ0(C)*QINV.L(C));
 GDPBASE('DSTOCK','RVALUE')   = SUM(C, PQ0(C)*QDST(C));
 GDPBASE('GOVCON','RVALUE')  = SUM(C, PQ0(C)*QG.L(C));
 GDPBASE('PROJINV','RVALUE')  = SUM(APROJ, SUM(F,QFPROJ.L(F,APROJ)*WF0(F)) + SUM(C,QINTPROJ.L(C,APROJ)*PQ0(C)));
 GDPBASE('EXPORTS','RVALUE') = SUM(CE, PWE0(CE)*EXR0*QE.L(CE));
 GDPBASE('IMPORTS','RVALUE') = -SUM(CM, PWM0(CM)*EXR0*QM.L(CM));
 GDPBASE('GDPCP','RVALUE') = SUM(IGDPXX, GDPBASE(IGDPXX,'RVALUE'));

 GDPBASE('ABSORP','RVALUE')
  = GDPBASE('GDPCP','RVALUE') - GDPBASE('IMPORTS','RVALUE')
    - GDPBASE('EXPORTS','RVALUE');

 GDPBASE('GDPFC2','RVALUE') = SUM(A, PVA0(A)*(1-tva(A))*QVA.L(A)) + SUM((APROJ,F),QFPROJ.L(F,APROJ)*WF0(F));

 GDPBASE('NETITAX','RVALUE') =
            SUM(A, ta(A)*PA0(A)*QA.L(A))
          + SUM(A, tva(A)*PVA0(A)*QVA.L(A))
          + SUM(CM, tm(CM)*QM.L(CM)*PWM0(CM))*EXR0
          + SUM(CE, te(CE)*QE.L(CE)*PWE0(CE))*EXR0
          + SUM(C, tq(C)*PQ0(C)*QQ.L(C));

 GDPBASE_T(IGDPX,KGDPX,SIMT)  = GDPBASE(IGDPX,KGDPX)     ;

*==================

 OPTION GDPBASE:6;
 DISPLAY GDPERR, GDPBASE;

*** post-simulation reporting

 PROJEXP_T(APROJ,SIMT)   =       PROJEXP.L(APROJ)        ;


*** post-simulation index calculation

 QA_index_T(A,SIMT)$QA0(A)       = (QA.L(A) / QA0(A) - 1) * 100       ;
 QD_index_T(C,SIMT)$QD0(C)       = (QD.L(C) / QD0(C) - 1) * 100       ;
 QE_index_T(C,SIMT)$QE0(C)       = (QE.L(C) / QE0(C) - 1) * 100       ;
 QM_index_T(C,SIMT)$QM0(C)       = (QM.L(C) / QM0(C) - 1) * 100       ;
 QH_index_T(C,H,SIMT)$QH0(C,H)       = (QH.L(C,H) / QH0(C,H) - 1) * 100       ;
 QF_index_T(F,A,SIMT)$QF0(F,A)   = (QF.L(F,A) / QF0(F,A) - 1) * 100       ;
 QFS_index_T(F,SIMT)$QFS0(F)     = (QFS.L(F) / QFS0(F) - 1) * 100       ;
 GDPBASE_index_T(IGDPX,KGDPX,SIMT)$GDPBASE0(IGDPX,KGDPX) =  (GDPBASE(IGDPX,KGDPX) / GDPBASE0(IGDPX,KGDPX) - 1)*100  ;
 YI_index_T(H,SIMT)$YI0(H)        = (YI.L(H) / YI0(H) - 1) * 100       ;

) ;


$if not set climate $set climate ""


 execute_unload "%results_dir%bui_results_%flabel%_%climate%.gdx" SAM, r, C,A,F,H,AC,INSDNG,GDPBASE_T, QA_index_T, QD_index_T, QE_index_T, QM_index_T, QF_index_T, GDPBASE_index_T, QFS_index_T, QH_index_T, PLVALUE_T, PROJEXP_T, PCOST_T, PFIN_T, REPAYMENT_T, LOANPROD_T, REPAYMENT_AUX, GSAV_T, PSAV_T, FSAV_T, TINV_T, EG_T, YG_T, YTAX_T, YGPROJ_T, EH_T, gammam, betam, gammah, betah YI_T, YIF_T, CPI_T, INVSHR_T, GOVSHR_T, YF_T, PA_T, QA_T, QX_T, QD_T, QM_T, QE_T, PQ_T, PE_T, PD_T, PX_T, PXAC_T, QINV_T, QINVTOT_T, WF_T, EXR_T, WALRAS_T, QG_T, PFIN_T, MPS_T, IADJ_T, GADJ_T, QF_T, QFS_T, QFSPROJ_T, QINT_T, YI_index_T, INSDNG_INCOME_T


*14. OPTIONAL NLP MODEL DEFINITION AND SOLUTION STATEMENT ###########

$ontext
Define a model that can be solved using a nonlinear programming (NLP)
solver. The model includes the equation OBJEQ which defines the
variable WALRASSQR, which is the square of the Walras' Law variable,
which must be zero in equilibrium.
$offtext

 MODEL NLPCGE  standard CGE model for NLP solver
 /
*Price block (10)
 PMDEF
 PEDEF
 PQDEF
 PXDEF
 PDDDEF
 PADEF
 PINTADEF
 PVADEF
 CPIDEF
 DPIDEF

*Production and trade block (17)
 CESAGGPRD
 CESAGGFOC
 LEOAGGINT
 LEOAGGVA
 CESVAPRD
 CESVAFOC
 INTDEM
 COMPRDFN
 OUTAGGFN
 OUTAGGFOC
 CET
 CET2
 ESUPPLY
 ARMINGTON
 COSTMIN
 ARMINGTON2
 QTDEM

*Institution block (12)
 YFDEF
 YIFDEF
 YIDEF
 EHDEF
 TRIIDEF
 HMDEM
 HADEM
 EGDEF
 YGDEF
 GOVDEM
 GOVBAL
 INVDEM

*System-constraint block (9)
 FACEQUIL
 COMEQUIL
 CURACCBAL
 TINSDEF
 MPSDEF
 SAVINVBAL
 TABSEQ
 INVABEQ
 GDABEQ
 OBJEQ
 /
 ;

 NLPCGE.HOLDFIXED   = 1 ;
 NLPCGE.TOLINFREP   = .0001 ;

*SOLVE NLPCGE MINIMIZING WALRASSQR USING NLP ;



*15. SOLUTION REPORTS ###############################################

*Optional include file defining report parameters summarizing economic
*data for the base year.

*$INCLUDE REPBASE.INC


$STITLE Input file: MOD101.GMS. Standard CGE modeling system, Version 1.01

 STANDCGE.MODELSTAT = 0;
 STANDCGE.SOLVESTAT = 0;
 STANDCGE.NUMREDEF  = 0;

 NLPCGE.MODELSTAT = 0;
 NLPCGE.SOLVESTAT = 0;
 NLPCGE.NUMREDEF  = 0;



*#*#*#*#*# THE END OF MOD101.GMS #*#*#*#*
