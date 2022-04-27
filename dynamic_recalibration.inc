***INVESTCGE
*BETWEEN-PERIOD PARAMETER RECALIBRATION


*Household demand

 betam(C,H)   = BUDSHR(C,H)*LESELAS1(C,H);
 betah(A,C,H) = BUDSHR2(A,C,H)*LESELAS2(A,C,H);

 gammam(C,H)$BUDSHR(C,H)
     =  EH.L(H) / PQ.L(C)
         * ( BUDSHR(C,H) + betam(C,H)/FRISCH(H));


 gammah(A,C,H)$BUDSHR2(A,C,H)
     =  EH.L(H) / PXAC.L(A,C)
                      * ( BUDSHR2(A,C,H) + betah(A,C,H)/FRISCH(H));

*CES activity production function

 deltava(F,A)$SAM(F,A)
            = (wfdist0(F,A) * WF0(F)
              * (QF.L(F,A))**(1+rhova(A)) )
              / SUM(FP, wfdist0(FP,A) * WF0(FP)*(QF.L(FP,A))**(1+rhova(A)));

 alphava0(A)= QVA.L(A)/( SUM(F$(QF.L(F,A)), deltava(F,A)*QF.L(F,A)
               **(-rhova(A))) )**(-1/rhova(A));

 alphava(A) = alphava0(A);

*CES top level production function

 predeltaa(A)  = 0 ;
 predeltaa(A)$(ACES(A) AND QINTA0(A))
                = (PVA0(A)/PINTA0(A))*(QVA.L(A)/QINTA.L(A))**(1+rhoa(A)) ;
 deltaa(A)$ACES(A) = predeltaa(A)/(1 + predeltaa(A)) ;
 alphaa(A)$deltaa(A)
                = QA.L(A)/((deltaa(A)*QVA.L(A)**(-rhoa(A))
                  +(1-deltaa(A))*QINTA.L(A)**(-rhoa(A)))**(-1/rhoa(A))) ;

$ontext

************ Energy-version calibration

**** energy-version VA bundle

 delta2va(F,A)$SAM(F,A)
            = (wfdist0(F,A) * WF.L(F) * (QF.L(F,A))**(1+rhova(A)) )
              / SUM(FP, wfdist0(FP,A) * WF0(FP)*(QF.L(FP,A))**(1+rhova(A)));

 alpha2va(A)= QVA.L(A)/( SUM(F$(QF.L(F,A)), delta2va(F,A)*QF.L(F,A)
               **(-rhova(A))) )**(-1/rhova(A));

*** energy-version ENE bundle

 delta2ene(CENE,A)$QINT0(CENE,A)
            = (PQ.L(CENE) * (QINT.L(CENE,A))**(1+rhoene(A)) )
              / SUM(CENEP, PQ.L(CENEP)*(QINT.L(CENEP,A))**(1+rhoene(A)));

 alpha2ene(A)$QENE0(A) = QENE.L(A)/( SUM(CENE$(QINT.L(CENE,A)), delta2ene(CENE,A)*QINT.L(CENE,A)
               **(-rhoene(A))) )**(-1/rhoene(A));



*** energy-version VAE bundle


 deltavae_va(A)  = 1 ;
 alphavae(A)     = 1 ;

 deltavae_va(A)$(QVA0(A) AND QENE0(A))
            = (PVA.L(A) * QVA.L(A)**(1+rhovae(A)) )
              / (PVA.L(A) * QVA.L(A)**(1+rhovae(A)) + PENE.L(A) * QENE.L(A)**(1+rhovae(A)));

 deltavae_ene(A)$(QENE0(A) AND)
            = (PENE.L(A) * QENE.L(A)**(1+rhovae(A)) )
              / (PVA.L(A) * QVA.L(A)**(1+rhovae(A)) + PENE.L(A) * QENE.L(A)**(1+rhovae(A)));


 alphavae(A)$(QVA0(A) AND QENE0(A)) = QVAE.L(A)/
                         ( deltavae_va(A) * QVA.L(A)**(-rhovae(A)) + deltavae_ene(A) * QENE.L(A)**(-rhovae(A)) ) **(-1/rhovae(A))   ;



*CET transformation
 deltat(C)$(CE(C) AND CD(C))
   = 1 / (1 + PDS.L(C)/PE0(C)*(QE.L(C)/QD.L(C))**(rhot(C)-1));

 alphat(C)$(CE(C) AND CD(C))
   = QX.L(C) / (deltat(C)*QE.L(C)**rhot(C) + (1-deltat(C))
                 *QD.L(C)**rhot(C))**(1/rhot(C));


*Armington aggregation


 predelta(C)$(CM(C) AND CD(C))
   = (PM.L(C)/(PDD.L(C)))*(QM.L(C)/QD.L(C))**(1+rhoq(C)) ;

 deltaq(C)$(CM(C) AND CD(C))
   = predelta(C)/(1 + predelta(C)) ;

 alphaq(C)$(CM(C) AND CD(C))
               = QQ.L(C)/(deltaq(C)*QM.L(C)**(-rhoq(C))
                 +(1-deltaq(C))*QD.L(C)**(-rhoq(C)))**(-1/rhoq(C)) ;

$offtext

 deltava_T(F,A,SIMT)     =       deltava(F,A)            ;
 alphava_T(A,SIMT)       =       alphava(A)              ;
 deltaa_T(A,SIMT)        =       deltaa(A)               ;
 alphaa_T(A,SIMT)        =       alphaa(A)               ;
 delta2va_T(F,A,SIMT)    =       delta2va(F,A)           ;
 alpha2va_T(A,SIMT)      =       alpha2va(A)             ;
 delta2ene_T(CENE,A,SIMT)=       delta2ene(CENE,A)       ;
 alpha2ene_T(A,SIMT)     =       alpha2ene(A)            ;
 deltavae_va_T(A,SIMT)   =       deltavae_va(A)          ;
 deltavae_ene_T(A,SIMT)  =       deltavae_ene(A)         ;
 alphavae_T(A,SIMT)      =       alphavae(A)             ;
 deltat_T(C,SIMT)        =       deltat(C)               ;
 alphat_T(C,SIMT)        =       alphat(C)               ;
 deltaq_T(C,SIMT)        =       deltaq(C)               ;
 alphaq_T(C,SIMT)        =       alphaq(C)               ;
 betam_T(C,H,SIMT)       =       betam(C,H)              ;
 betah_T(A,C,H,SIMT)     =       betah(A,C,H)            ;
 gammam_T(C,H,SIMT)      =       gammam(C,H)             ;
 gammah_T(A,C,H,SIMT)    =       gammah(A,C,H)           ;


