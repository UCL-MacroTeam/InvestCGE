$STITLE Input file: TEST.DAT. Standard CGE modeling system, Version 1.01

$ontext

This data file is used to test robustness to different
specifications. The SAM that is used has the following
characteristics:
1--exports without domestic demand
2--imports without domestic supply
3--home consumption
4--multiple outputs for multiple activities
5--transactions costs with multiple commodities
   used to provide transactions services
6--the presence of all tax types that currently can
   be handled by the standard model


The file includes signals to the user who is constructing his/her
own data set:
"!!" -- read carefully; perhaps need to supply information

The file is divided into the following searchable blocks:
1. SET DEFINITIONS
2. SAM
3. ELASTICITIES
4. PHYSICAL FACTOR QUANTITIES
5. COMMODITY VALUE SHARES FOR HOME CONSUMPTION
6. INITIALIZATION OF TAX DATA

$offtext

*1. SET DEFINITIONS #################################################
$ontext

Instructions:

!!-1. In this section, the user should DEFINE all sets (by entering the
names of set elements). The only exception (which is signalled when
relevant) is for sets that are not invariably included in the SAM --
their element lists may be left blank. The user should NOT change set
names.


!!-2. The set AC has to include the following elements:
 GOV            government
 ROW            rest of the world
 S-I            savings-investment
 DSTK           stock changes

 TRNCSTDOM     domestic transactions cost account
 TRNCSTEXP     export transactions cost account
 TRNCSTIMP     import transactions cost account

 INSTAX               direct taxes on domestic institutions
 FACTAX        direct factor taxes
 IMPTAX        import taxes
 EXPTAX        export taxes
 VATAX         value-added taxes
 ACTTAX        indirect taxes on activity revenue
 COMTAX        indirect taxes on commodity sales in domestic market
 DUM           dummy

 TOTAL         total


Comments on the above list:

a. The name TOTAL is also compulsory in the SAM.

b. If the SAM includes a government,the rest of the world, savings-
investment and/or stock changes, then the above account names (GOV, ROW,
S-I, DSTK) have to be used for these entities.

c. The elements that are related to transactions costs and taxes as well
as the dummy account are used for reports. In the SAM, any other names
may be used (subject to the restrictions imposed by GAMS syntax).

d. In addition, the tax elements are used for model calibration. Later
in this file, the user will be prompted to supply the names of the SAM
accounts that correspond to these elements (see the parameter TAXPAR).
Taxes have to be paid to special tax accounts that, in their turn, pass on
their total receipts to the account GOV; it is illegal to have taxes paid
directly to GOV.

$offtext



SET
*!!-start entering information here
 AC  global set for model accounts - aggregated microsam accounts
 /
*activities
A-AGR
A-IND
A-LIND
A-CONS
A-TRAN
A-ELNH
A-ELHY
A-ENE
A-SERV
A-PSRV
A-PROJ
B-PROJ
C-AGR
C-CCO
C-IND
C-LIND
C-MACH
C-CONS
C-TRAN
C-ELEC
C-ENE
C-SERV
C-PSRV
C-PROJ
TRCDOM
TRCEXP
LABOR
CAP-AGR
CAP-OTH
CAP-HY
LAND
RUR-HH
URB-HH
ent
gov
dtax
exptax
imptax
ftax
stax
s-i
dstk
row
TOTAL

 /

 A(AC)  activities
 /
A-AGR
A-IND
A-LIND
A-CONS
A-TRAN
A-ELNH
A-ELHY
A-ENE
A-SERV
A-PSRV
 /

 APROJ(AC) project investment activity
 /
A-PROJ
B-PROJ
 /

*Note: if your SAM does not include elements for this set, leave it empty.
 AAGR(A)  agricultural activities
 /
A-AGR

 /

 AENE(A)  energy activities
 /
A-ELNH
A-ELHY
A-ENE
 /

;

 ACES(A) = NO;
 ALEO(A)$(NOT ACES(A)) = YES;
 ANAGR(A) = NOT AAGR(A);

 AVALEO(A) = NO ;
 AVACES(A)$(NOT AVALEO(A)) = YES ;

 AVAELEO(A) = NO ;
 AVAECES(A)$(NOT AVAELEO(A)) = YES ;

SET

 C(AC)  commodities
 /
C-AGR
C-IND
C-LIND
C-CONS
C-TRAN
C-ELEC
C-ENE
C-SERV
C-PSRV
 /

 CPROJ(AC) project investment commodity
 /
C-PROJ
 /
*Note: if your SAM does not include elements for this set, leave it empty.
 CAGR(C)    agricultural commodities
 /
C-AGR
 /

 CENE(C)    energy commodities
/
C-ELEC
C-ENE
/
;

 CNAGR(C)      = NOT CAGR(C);

 CNENE(C)      = NOT CENE(C);


 SETS
*Note: Each of these three sets identify the element in the SAM
*that receives payments from commodity accounts for transactions
*services (for domestic sales, exports, and imports, respectively).
*If your SAM does not include these payments, leave the
*sets empty.
 CTD(AC)      domestic transactions cost account /TRCDOM/
 CTE(AC)      export transactions cost account   /TRCEXP/
 CTM(AC)      import transactions cost account   / /


 F(AC)  factors
 /
LABOR
*LAB-RUR
*LAB-URB
*CAPITAL
CAP-AGR
CAP-OTH
*CAP-NH
CAP-HY
LAND
 /

*Note: in the unlikely event that your SAM does not include labor
*factors, leave the following set empty.
 FLAB(F) labor
 /
 LABOR       labor
*  LAB-RUR
*  LAB-URB
 /

*Note: if your SAM does not include land factors, leave the following
*set empty.
 FLND(F)   land
 /
LAND
 /

*Note: in the unlikely event that your SAM does not include capital
*factors, leave the following set empty.
 FCAP(F) capital
 /
*   CAPITAL
*   CAP-NH
    CAP-AGR
    CAP-OTH
    CAP-HY
 /

 INS(AC)  institutions
 /
 ENT       enterprises
 RUR-HH
 URB-HH
 GOV       government
 ROW       rest of the world
 /

 INSD(INS)  domestic institutions
 /
 ENT       enterprises
 RUR-HH
 URB-HH
 GOV       government
 /

 INSDNG(INSD) domestic non-government institutions
 /
 ENT       enterprises
 RUR-HH
 URB-HH
/

*Note: if your SAM does not include elements for this set, leave it empty.
 EN(INSDNG)  enterprises
 /
 ENT       enterprises
 /

 H(INSDNG)  households
 /
 RUR-HH
 URB-HH
 /
 ;
 ACNT(AC) = YES; ACNT('TOTAL') = NO;

DISPLAY
 ACES, ALEO, AAGR, ANAGR, CAGR, CNAGR
 ;


*2. SAM #############################################################

$ontext

!!-In this section, a SAM is included and balanced (if needed).

The SAM can be included in two ways:
1. put directly in the file in the space below (as in this file).
A maximum of 6 columns is feasible; or
2. The SAM can be imported with an import program such as XLLINK or
SSLINK.

$offtext


*!! The user includes a SAM and selects its name (subject to GAMS
*syntactical rules).

parameter TESTSAM(AC,ACP)        ;
$gdxin GHANA_AGG_SAM.gdx
$load TESTSAM=SAM

OPTION TESTSAM:3;
DISPLAY TESTSAM;


*!! : The user transfers the data of the above table (using the
*selected parameter name) in the parameter SAM.
*To avoid scaling problems, the SAM may be scaled.
 SAM(AC,ACP) = 0.1*0.528*TESTSAM(AC,ACP);


*Account totals are recomputed. Check for SAM balance.

 SAM('TOTAL',AC) = 0;
 SAM(AC,'TOTAL') = 0;

 SAM('TOTAL',AC) = SUM(ACNT, SAM(ACNT,AC));
 SAM(AC,'TOTAL') = SUM(ACNT, SAM(AC,ACNT));

 SAMBALCHK(AC)   = SAM('TOTAL',AC) - SAM(AC,'TOTAL');

 DISPLAY "Read in SAM", SAMBALCHK;
 DISPLAY "Read in SAM", SAM;



*If account balances exceed a critical maximum value, SAMBAL.INC will
*balance the SAM exactly.
$INCLUDE SAMBAL.INC


$STITLE Input file: GHANA_DATA.GMS. Standard CGE modeling system, Version 1.01


*Defining CINV using SAM data with potential user input.

$ontext
*!!
In the model, there are two ways of separating stock changes from fixed
investment:

a. If the SAM includes the account S-I (in its column making payments to
commodities for fixed investments and an aggregate payment to DSTK for
stock changes) and DSTK (in its row paid by S-I, in its column making
payments, positive or negative, to commodities, representing stock
change values), then no user action is needed -- the set CINV will by
default include all elements in C that receive payments from S-I.

b. If, the account DSTK does not appear in the SAM, then the set CINV
should only include the commodities that receive payments from S-I for
fixed investment; commodities receiving payments for stock changes
should be excluded. The default is that all commodities receiving
payments are included; the user may exclude selected commodities from
CINV. This should be done for all commodities that receive
a negative payments from S-I. (An example of how to do this is provided
just below.)

$offtext

*All commodities receiving payments from S-I are included in the set CINV.
 CINV(C)$SAM(C,'S-I') = YES;

*!!- User option to exclude selected commodities from the set CINV. Only
*relevant for SAMs without the account DSTK.
*Example:
*If the set C includes a commodity called CWHEAT and payments in the cell
*SAM('CWHEAT','S-I') are for stock changes, the user should include
*the following line in the program:
*CINV('CWHEAT')  = NO;

*Commodities receiving negative payments from S-I should be removed from
*CINV since such payments refer to stock changes:
* CINV(C)$(SAM(C,'S-I') LT 0) = NO;


DISPLAY CINV;


*3. ELASTICITIES ####################################################
$ontext

!!- In this section, the user inputs elasticities for trade, production,
and household consumption. If the user does not supply all required
data, missing data will be generated in STDMOD.GMS using simple
assumptions.

$offtext

*Trade elasticities========================================

*SIGMAQ is the elasticity of substitution between imports
*and domestic output in domestic demand.
*SIGMAT is the elasticity of transformation for domestic
*marketed output between exports and domestic supplies.

SET
 TRDELAS  trade elasticities
 /
 SIGMAQ  Armington elasticity
 SIGMAT  CET elasticity
 /


PARAMETER
 TRADELAS(AC,TRDELAS)   Armington and CET elasticities by commodity
;
 TRADELAS(C,'SIGMAQ')$SAM('ROW',C) = 0.8;
 TRADELAS(C,'SIGMAT')$SAM(C,'ROW') = 1.6;
* TRADELAS(C,'SIGMAT')$SAM("C-CCO",'ROW') = 50;



*Production elasticities===================================

PARAMETER
 PRODELAS(A)  elas of substit bt. factors - bottom of technology nest
 PRODELAS_VAE(A)  elas of substit bt. factors - bottom of technology nest
 PRODELAS_ENE(A)  elas of substit bt. factors - bottom of technology nest

 PRODELAS2(A) elas of substit bt. agg fac & intermed - top of tech nest
 ELASAC(C)    output aggregation elasticity for commodity C
 ETALS(FLAB)  labour supply elasticity
 ;

 PRODELAS(A)  = 0.8;
 PRODELAS("A-ELHY")  = 0.1;
 PRODELAS("A-ELNH")  = 0.1;

 PRODELAS_VAE(A)  = 1.25;

 PRODELAS_VAE("A-ELHY")  = 0.1;
 PRODELAS_VAE("A-ELNH")  = 0.1;
 PRODELAS_VAE("A-AGR")  = 0.25;

* PRODELAS("A-ELHY")  = 0.1;
* PRODELAS("A-ELNH")  = 0.1;

 PRODELAS_ENE(A)  = 1.25 ;
 PRODELAS_ENE("A-ENE")  = 0.1;
 PRODELAS_ENE("A-ELNH")  = 0.1;
 PRODELAS_ENE("A-ELHY")  = 0.1;
 PRODELAS_ENE("A-AGR")  = 0.1;



* PRODELAS("A-ELHY") = 0.1        ;
 PRODELAS2(A) = 0.6;
 ELASAC(C)    = 6;
* ELASAC("C-ELEC")    = 3;
 ETALS(FLAB)  = 0.1 ;


*Household population data=================================

*Note: opulation data are optional. They may be useful for verification
*of household model parameters and for report parameters.


PARAMETER
 POP(H) Base-year population for household h (units)
 /
 /


*Household consumption elasticities========================
*Note: The Frisch parameter is included in this section.

$ontext
TABLE  LESELAS1(C,H) Exp'e elasticity of market dem for com c by hhd h

            HURB        HRUR

CAGR1       1.10        0.62
CAGR2       0.95        0.80
CAGR3-EX    0.38        0.42
CIND        1.20        1.35
CTTRA       1.20        1.35
COSER       0.80        0.76
CIMP        1.10        0.80
 ;
$offtext
TABLE  LESELAS1(C,H) Exp'e elasticity of market dem for com c by hhd h

            RUR-HH      URB-HH

C-AGR       1.10        0.62
*C-CCO       1.10        0.62
C-IND       1.20        1.35
C-LIND       1.20        1.35
C-TRAN      1.20        1.35
*C-MACH      1.20        1.35
C-CONS      1.20        1.35
C-ELEC      1.20        1.35
C-SERV      0.80        0.76
C-PSRV      0.80        0.76
 ;

PARAMETERS
 FRISCH(H)        Frisch parameter for household LES demand
 LESELAS2(A,C,H)  Exp'e elasticity of home dem by com - act - hhd
 ;

 FRISCH(H)       = -2;
*For an econometrically estimated functional relationship between the
*Frisch parameter and per-capita income based on cross-country data, see
*p. 248 in Lluch, Powell and Williams (1977). Patterns in Household
*Demand and Savings. London: Oxford University Press.

*The LES demand system permits the user to specify commodity-specific
*expenditure elasticities and household-specific Frisch parameters.
*(The Frisch parameter measures the elasticity of the marginal utility
*of income with respect to income.)
*If such information is not available the user may alternative specify
*all elasticities to be unity (as we do just below). If, in addition,
*the Frisch parameter is set at -1, the LES system collapses to a
*Cobb-Douglas system.
*LESELAS1(C,H)   =  1;

*Note: Data for LESELAS2 are only needed (and used) for the commodity-
*activity-household combinations that are relevant to home consumption.
 LESELAS2(A,C,H) =  1;


*4. PHYSICAL FACTOR QUANTITIES ######################################

$ontext

!!: If you have data on physical factor DEMANDS, add them in this
section. If so, there is no need to include supply quantities --
the model code in STDMOD.GMS will not use the supply information.

If data are provided for SUPPLY quantities (but not for demand), the
model code in STDMOD.GMS will define disaggregated activity demand
quantities for each factor as:
(total factor supply) TIMES (activity share in total activity payments
to the factor).
This amounts to assuming that, for each factor, wages are uniform across
activities. The data on factor payments are from the SAM.

If you don't have any data on physical factor quantities, leave
this section blank.

$offtext

PARAMETER
 QFSBASE(F)   base-year qnty of supply for factor f
*For each factor, specify the units in a comment.
 /

 /

 QFBASE(F,A)  qnty of factor f employed by activity a
*For each factor, specify the units in a comment.
 ;

 QFBASE(F,A)             = 0;


*5. COMMODITY VALUE SHARES FOR HOME CONSUMPTION #####################

$ontext

!!-User input is needed only if the SAM includes household home
consumption, reflected in payments from households to activities, and
if these activities produce multiple outputs.

If this condition is met, the user should define the parameter SHRHOME,
using extraneous data on commodity value shares which, for each
household-activity combination, should sum to unity.

In the absence of user input, the program will compute these value
shares in the file STDMOD.GMS, using the OUTPUT value shares for
each commodity. (For single-output activities, the computed shares
will quite correctly be at unity.)

Note that elasticities are needed for the parameter LESELAS2(A,C,H)
(above in the elasticity section) for the identified combinations

$offtext


PARAMETER
 shrhome(A,C,H) value share for comm'y c in home cons of hhd h from act a
 ;

*!!: If needed, manually define shrhome.
 shrhome(A,C,H) = 0;

display shrhome;

$ontext

TABLE
 SHRHOME(A,C,H)  value share for comm'y c in home cons of hhd h from act a

                   HURB        HRUR
AAGR1.CAGR1       0.800       0.200
AAGR1.CAGR2       0.200       0.800
AAGR2.CAGR2       1.000       1.000
AIND.CIND         1.000       1.000
$offtext

*6. INITIALIZATION OF TAX DATA ######################################
$ontext
!!: In this section, the user HAS TO supply the names of the tax accounts
in the SAM into the definition of TAXPAR. In STDMOD.GMS, TAXPAR is used
for model calibration. A proper definition of TAXPAR is necessary for
the functioning of the model if the SAM includes taxes.

$offtext

SET
 TX  taxes in the model
 /
 INSTAX         direct taxes on domestic institutions
 FACTAX         direct factor taxes
 IMPTAX         import taxes
 EXPTAX         export taxes
 VATAX          value-added taxes
 ACTTAX         taxes on activity revenue
 COMTAX         taxes on commodity sales in domestic market

 /
;

PARAMETER
 TAXPAR(TX,AC)   payment by account ac to tax account tx
 ;

ALIAS(TX,TXP);


*!!: Starting from here, the user should supply names of matching tax
*accounts in the SAM on the right-hand side.

*direct taxes on domestic institutions
 TAXPAR('INSTAX',INSD)  = SAM('DTAX',INSD);

*direct factor taxes
 TAXPAR('FACTAX',F)     = SAM('FTAX',F);

*import taxes
 TAXPAR('IMPTAX',C)   = SAM('IMPTAX',C);

*export taxes
 TAXPAR('EXPTAX',C)     = SAM('EXPTAX',C);

*value-added taxes
 TAXPAR('VATAX',A)      = SAM('STAX',A);

*taxes on activity revenue
 TAXPAR('ACTTAX',A)     = SAM('DTAX',A);

*taxes on commodity sales in domestic market
 TAXPAR('COMTAX',C)     = SAM('STAX',C) ; ;

*#*#*#*#*# THE END OF TEST.DAT #*#*#*#*
