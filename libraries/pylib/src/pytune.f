C*********************************************************************
 
C...PYTUNE
C...Presets for a few specific underlying-event and min-bias tunes
C...Note some tunes require external pdfs to be linked (e.g. 105:QW),
C...others require particular versions of pythia (e.g. the SCI and GAL
C...models). See below for details.
      SUBROUTINE PYTUNE(ITUNE)
C
C ITUNE    NAME (detailed descriptions below)
C     0 Default : No settings changed => defaults.
C
C ====== Old UE, Q2-ordered showers ====================================
C   100       A : Rick Field's CDF Tune A                     (Oct 2002)
C   101      AW : Rick Field's CDF Tune AW                    (Apr 2006)
C   102      BW : Rick Field's CDF Tune BW                    (Apr 2006)
C   103      DW : Rick Field's CDF Tune DW                    (Apr 2006)
C   104     DWT : As DW but with slower UE ECM-scaling        (Apr 2006)
C   105      QW : Rick Field's CDF Tune QW using CTEQ6.1M            (?)
C   106 ATLAS-DC2: Arthur Moraes' (old) ATLAS tune ("Rome")          (?)
C   107     ACR : Tune A modified with new CR model           (Mar 2007)
C   108      D6 : Rick Field's CDF Tune D6 using CTEQ6L1             (?)
C   109     D6T : Rick Field's CDF Tune D6T using CTEQ6L1            (?)
C ---- Professor Tunes : 110+ (= 100+ with Professor's tune to LEP) ----
C   110   A-Pro : Tune A, with LEP tune from Professor        (Oct 2008)
C   111  AW-Pro : Tune AW, -"-                                (Oct 2008)
C   112  BW-Pro : Tune BW, -"-                                (Oct 2008)
C   113  DW-Pro : Tune DW, -"-                                (Oct 2008)
C   114 DWT-Pro : Tune DWT, -"-                               (Oct 2008)
C   115  QW-Pro : Tune QW, -"-                                (Oct 2008)
C   116 ATLAS-DC2-Pro: ATLAS-DC2 / Rome, -"-                  (Oct 2008)
C   117 ACR-Pro : Tune ACR, -"-                               (Oct 2008)
C   118  D6-Pro : Tune D6, -"-                                (Oct 2008)
C   119 D6T-Pro : Tune D6T, -"-                               (Oct 2008)
C ---- Professor's Q2-ordered Perugia Tune : 129 -----------------------
C   129 Pro-Q20 : Professor Q2-ordered tune                   (Feb 2009)
C
C ====== Intermediate and Hybrid Models ================================
C   200    IM 1 : Intermediate model: new UE, Q2-ord. showers, new CR
C   201     APT : Tune A w. pT-ordered FSR                    (Mar 2007)
C   211 APT-Pro : Tune APT, with LEP tune from Professor      (Oct 2008)
C   221 Perugia APT  : "Perugia" update of APT-Pro            (Feb 2009)
C   226 Perugia APT6 : "Perugia" update of APT-Pro w. CTEQ6L1 (Feb 2009)
C
C ====== New UE, interleaved pT-ordered showers, annealing CR ==========
C   300      S0 : Sandhoff-Skands Tune using the S0 CR model  (Apr 2006)
C   301      S1 : Sandhoff-Skands Tune using the S1 CR model  (Apr 2006)
C   302      S2 : Sandhoff-Skands Tune using the S2 CR model  (Apr 2006)
C   303     S0A : S0 with "Tune A" UE energy scaling          (Apr 2006)
C   304    NOCR : New UE "best try" without col. rec.         (Apr 2006)
C   305     Old : New UE, original (primitive) col. rec.      (Aug 2004)
C   306 ATLAS-CSC: Arthur Moraes' (new) ATLAS tune w. CTEQ6L1 (?)
C ---- Professor Tunes : 310+ (= 300+ with Professor's tune to LEP)
C   310   S0-Pro : S0 with updated LEP pars from Professor    (Oct 2008)
C   311   S1-Pro : S1 -"-                                     (Oct 2008)
C   312   S2-Pro : S2 -"-                                     (Oct 2008)
C   313  S0A-Pro : S0A -"-                                    (Oct 2008)
C   314 NOCR-Pro : NOCR -"-                                   (Oct 2008)
C   315  Old-Pro : Old -"-                                    (Oct 2008)
C ---- Peter's Perugia Tunes : 320+ ------------------------------------
C   320 Perugia 0 : "Perugia" update of S0-Pro                (Feb 2009)
C   321 Perugia HARD : More ISR, More FSR, Less MPI, Less BR, Less HAD
C   322 Perugia SOFT : Less ISR, Less FSR, More MPI, More BR, More HAD
C   323 Perugia 3 : Alternative to Perugia 0, with different ISR/MPI
C                   balance & different scaling to LHC & RHIC (Feb 2009)
C   324 Perugia NOCR : "Perugia" update of NOCR-Pro           (Feb 2009)
C   325 Perugia * : "Perugia" Tune w. (external) MRSTLO* PDFs (Feb 2009)
C   326 Perugia 6 : "Perugia" Tune w. (external) CTEQ6L1 PDFs (Feb 2009)
C ---- Professor's pT-ordered Perugia Tune : 329 -----------------------
C   329 Pro-pT0   : Professor pT-ordered tune w. S0 CR model  (Feb 2009)
C
C ======= The Uppsala models ===========================================
C   ( NB! must be run with special modified Pythia 6.215 version )
C   ( available from http://www.isv.uu.se/thep/MC/scigal/        )
C   400   GAL 0 : Generalized area-law model. Org pars        (Dec 1998)
C   401   SCI 0 : Soft-Colour-Interaction model. Org pars     (Dec 1998)
C   402   GAL 1 : GAL 0. Tevatron MB retuned (Skands)         (Oct 2006)
C   403   SCI 1 : SCI 0. Tevatron MB retuned (Skands)         (Oct 2006)
C
C More details;
C
C Quick Dictionary:
C      BE : Bose-Einstein
C      BR : Beam Remnants
C      CR : Colour Reconnections
C      HAD: Hadronization
C      ISR/FSR: Initial-State Radiation / Final-State Radiation
C      FSI: Final-State Interactions (=CR+BE)
C      MB : Minimum-bias
C      MI : Multiple Interactions
C      UE : Underlying Event
C
C=======================================================================
C TUNES OF OLD FRAMEWORK (Q2-ORDERED ISR AND FSR, NON-INTERLEAVED UE)
C=======================================================================
C
C   A (100) and AW (101). CTEQ5L parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...* Large starting scale for ISR (PARP(67)=4)
C...* AW has even more radiation due to smaller mu_R choice in alpha_s.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   BW (102). CTEQ5L parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...NB: Can also be run with Pythia 6.2 or 6.312+
C...* Small starting scale for ISR (PARP(67)=1)
C...* BW has more radiation due to smaller mu_R choice in alpha_s.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   DW (103) and DWT (104). CTEQ5L parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: extensively compared to CDF data (R.D. Field).
C...NB: Can also be run with Pythia 6.2 or 6.312+
C...* Intermediate starting scale for ISR (PARP(67)=2.5)
C...* DWT has a different reference energy, the same as the "S" models
C...  below, leading to more UE activity at the LHC, but less at RHIC.
C...* See: http://www.phys.ufl.edu/~rfield/cdf/
C
C   QW (105). CTEQ61 parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: uses CTEQ61 (external pdf library must be linked)
C
C   ATLAS-DC2 (106). CTEQ5L parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.2 (e.g. 6.228) ***
C...***      CAN ALSO BE RUN WITH PYTHIA 6.406+
C...Key feature: tune used by the ATLAS collaboration.
C
C   ACR (107). CTEQ5L parton distributions
C...*** NB : SHOULD BE RUN WITH PYTHIA 6.412+    ***
C...Key feature: Tune A modified to use annealing CR.
C...NB: PARP(85)=0D0 and amount of CR is regulated by PARP(78).
C
C   D6 (108) and D6T (109). CTEQ6L parton distributions
C...Key feature: Like DW and DWT but retuned to use CTEQ6L PDFs.
C
C   A-Pro, BW-Pro, etc (111, 112, etc). CTEQ5L parton distributions
C   Old UE model, Q2-ordered showers.
C...Key feature: Rick Field's family of tunes revamped with the
C...Professor Q2-ordered final-state shower and fragmentation tunes
C...presented by Hendrik Hoeth at the Perugia MPI workshop in Oct 2008.
C...Key feature: improved descriptions of LEP data.
C
C   Pro-Q20 (129). CTEQ5L parton distributions
C   Old UE model, Q2-ordered showers.
C...Key feature: Complete retune of old model by Professor, including
C...large amounts of both LEP and Tevatron data.
C...Note that PARP(64) (ISR renormalization scale pre-factor) is quite
C...extreme in this tune, corresponding to using mu_R = pT/3 .
C
C=======================================================================
C INTERMEDIATE/HYBRID TUNES (MIX OF NEW AND OLD SHOWER AND UE MODELS)
C=======================================================================
C
C   IM1 (200). Intermediate model, Q2-ordered showers,
C   CTEQ5L parton distributions
C...Key feature: new UE model w Q2-ordered showers and no interleaving.
C...* "Rap" tune of hep-ph/0402078, modified with new annealing CR.
C...* See: Sjostrand & Skands: JHEP 03(2004)053, hep-ph/0402078.
C
C   APT (201). Old UE model, pT-ordered final-state showers,
C   CTEQ5L parton distributions
C...Key feature: Rick Field's Tune A, but with new final-state showers
C
C   APT-Pro (211). Old UE model, pT-ordered final-state showers,
C   CTEQ5L parton distributions
C...Key feature: APT revamped with the Professor pT-ordered final-state
C...shower and fragmentation tunes presented by Hendrik Hoeth at the
C...Perugia MPI workshop in October 2008.
C
C   Perugia-APT (221). Old UE model, pT-ordered final-state showers,
C   CTEQ5L parton distributions
C...Key feature: APT-Pro with final-state showers off the MPI,
C...lower ISR renormalization scale to improve agreement with the
C...Tevatron Drell-Yan pT measurements and with improved energy scaling
C...to min-bias at 630 GeV.
C
C   Perugia-APT6 (226). Old UE model, pT-ordered final-state showers,
C   CTEQ6L1 parton distributions.
C...Key feature: uses CTEQ6L1 (external pdf library must be linked),
C...with a slightly lower pT0 (2.0 instead of 2.05) due to the smaller
C...UE activity obtained with CTEQ6L1 relative to CTEQ5L.
C
C=======================================================================
C TUNES OF NEW FRAMEWORK (PT-ORDERED ISR AND FSR, INTERLEAVED UE)
C=======================================================================
C
C   S0 (300) and S0A (303). CTEQ5L parton distributions
C...Key feature: large amount of multiple interactions
C...* Somewhat faster than the other colour annealing scenarios.
C...* S0A has a faster energy scaling of the UE IR cutoff, borrowed
C...  from Tune A, leading to less UE at the LHC, but more at RHIC.
C...* Small amount of radiation.
C...* Large amount of low-pT MI
C...* Low degree of proton lumpiness (broad matter dist.)
C...* CR Type S (driven by free triplets), of medium strength.
C...* See: Pythia6402 update notes or later.
C
C   S1 (301). CTEQ5L parton distributions
C...Key feature: large amount of radiation.
C...* Large amount of low-pT perturbative ISR
C...* Large amount of FSR off ISR partons
C...* Small amount of low-pT multiple interactions
C...* Moderate degree of proton lumpiness
C...* Least aggressive CR type (S+S Type I), but with large strength
C...* See: Sandhoff & Skands: FERMILAB-CONF-05-518-T, in hep-ph/0604120.
C
C   S2 (302). CTEQ5L parton distributions
C...Key feature: very lumpy proton + gg string cluster formation allowed
C...* Small amount of radiation
C...* Moderate amount of low-pT MI
C...* High degree of proton lumpiness (more spiky matter distribution)
C...* Most aggressive CR type (S+S Type II), but with small strength
C...* See: Sandhoff & Skands: FERMILAB-CONF-05-518-T, in hep-ph/0604120.
C
C   NOCR (304). CTEQ5L parton distributions
C...Key feature: no colour reconnections (NB: "Best fit" only).
C...* NB: <pT>(Nch) problematic in this tune.
C...* Small amount of radiation
C...* Small amount of low-pT MI
C...* Low degree of proton lumpiness
C...* Large BR composite x enhancement factor
C...* Most clever colour flow without CR ("Lambda ordering")
C
C   ATLAS-CSC (306). CTEQ6L parton distributions
C...Key feature: 11-parameter ATLAS tune of the new framework.
C...* Old (pre-annealing) colour reconnections a la 305.
C...* Uses CTEQ6 Leading Order PDFs (must be interfaced externally)
C
C   S0-Pro, S1-Pro, etc (310, 311, etc). CTEQ5L parton distributions.
C...Key feature: the S0 family of tunes revamped with the Professor
C...pT-ordered final-state shower and fragmentation tunes presented by
C...Hendrik Hoeth at the Perugia MPI workshop in October 2008.
C...Key feature: improved descriptions of LEP data.
C
C   Perugia-0 (320). CTEQ5L parton distributions.
C...Key feature: S0-Pro retuned to more Tevatron data. Better Drell-Yan
C...pT spectrum, better <pT>(Nch) in min-bias, and better scaling to
C...630 GeV than S0-Pro. Also has a slightly smoother mass profile, more
C...beam-remnant breakup (more baryon number transport), and suppression
C...of CR in high-pT string pieces.
C
C   Perugia-HARD (321). CTEQ5L parton distributions.
C...Key feature: More ISR, More FSR, Less MPI, Less BR
C...Uses pT/2 as argument of alpha_s for ISR, and a higher Lambda_FSR.
C...Has higher pT0, less intrinsic kT, less beam remnant breakup (less
C...baryon number transport), and more fragmentation pT.
C...Multiplicity in min-bias is LOW, <pT>(Nch) is HIGH,
C...DY pT spectrum is HARD.
C
C   Perugia-SOFT (322). CTEQ5L parton distributions.
C...Key feature: Less ISR, Less FSR, More MPI, More BR
C...Uses sqrt(2)*pT as argument of alpha_s for ISR, and a lower
C...Lambda_FSR. Has lower pT0, more beam remnant breakup (more baryon
C...number transport), and less fragmentation pT.
C...Multiplicity in min-bias is HIGH, <pT>(Nch) is LOW,
C...DY pT spectrum is SOFT
C
C   Perugia-3 (323). CTEQ5L parton distributions.
C...Key feature: variant of Perugia-0 with more extreme energy scaling
C...properties while still agreeing with Tevatron data from 630 to 1960.
C...More ISR and less MPI than Perugia-0 at the Tevatron and above and
C...allows FSR off the active end of dipoles stretched to the remnant.
C
C   Perugia-NOCR (324). CTEQ5L parton distributions.
C...Key feature: Retune of NOCR-Pro with better scaling properties to
C...lower energies and somewhat better agreement with Tevatron data
C...at 1800/1960.
C
C   Perugia-* (325). MRST LO* parton distributions for generators
C...Key feature: first attempt at using the LO* distributions
C...(external pdf library must be linked).
C
C   Perugia-6 (326). CTEQ6L1 parton distributions
C...Key feature: uses CTEQ6L1 (external pdf library must be linked).
C
C   Pro-pT0 (329). CTEQ5L parton distributions
C...Key feature: Complete retune of new model by Professor, including
C...large amounts of both LEP and Tevatron data. Similar to S0A-Pro.
C
C=======================================================================
C OTHER TUNES
C=======================================================================
C
C...The GAL and SCI models (400+) are special and *SHOULD NOT* be run
C...with an unmodified Pythia distribution.
C...See http://www.isv.uu.se/thep/MC/scigal/ for more information.
C
C ::: + Future improvements?
C        Include also QCD K-factor a la M. Heinz / ATLAS TDR ? RDF's QK?
C       (problem: K-factor affects everything so only works as
C        intended for min-bias, not for UE ... probably need a
C        better long-term solution to handle UE as well. Anyway,
C        Mark uses MSTP(33) and PARP(31)-PARP(33).)
 
C...Global statements
      IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      INTEGER PYK,PYCHGE,PYCOMP
 
C...Commonblocks.
      COMMON/PYDAT1/MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/PYPARS/MSTP(200),PARP(200),MSTI(200),PARI(200)
 
C...SCI and GAL Commonblocks
      COMMON /SCIPAR/MSWI(2),PARSCI(2)
 
C...SAVE statements
      SAVE /PYDAT1/,/PYPARS/
      SAVE /SCIPAR/

C...Internal parameters
      PARAMETER(MXTUNS=500)
      CHARACTER*8 CHVERS, CHDOC
      PARAMETER (CHVERS='1.015   ',CHDOC='Jan 2009')
      CHARACTER*16 CHNAMS(0:MXTUNS), CHNAME
      CHARACTER*42 CHMSTJ(50), CHMSTP(51:100), CHPARP(61:100),
     &    CHPARJ(1:100), CH40
      CHARACTER*60 CH60
      CHARACTER*70 CH70
      DATA (CHNAMS(I),I=0,1)/'Default',' '/
      DATA (CHNAMS(I),I=100,119)/
     &    'Tune A','Tune AW','Tune BW','Tune DW','Tune DWT','Tune QW',
     &    'ATLAS DC2','Tune ACR','Tune D6','Tune D6T',
     1    'Tune A-Pro','Tune AW-Pro','Tune BW-Pro','Tune DW-Pro',
     1    'Tune DWT-Pro','Tune QW-Pro','ATLAS DC2-Pro','Tune ACR-Pro',
     1    'Tune D6-Pro','Tune D6T-Pro'/
      DATA (CHNAMS(I),I=120,129)/
     &     9*' ','Pro-Q20'/
      DATA (CHNAMS(I),I=300,309)/
     &    'Tune S0','Tune S1','Tune S2','Tune S0A','NOCR','Old',
     5    'ATLAS-CSC Tune','Yale Tune','Yale-K Tune',' '/
      DATA (CHNAMS(I),I=310,315)/
     &    'Tune S0-Pro','Tune S1-Pro','Tune S2-Pro','Tune S0A-Pro',
     &    'NOCR-Pro','Old-Pro'/
      DATA (CHNAMS(I),I=320,329)/
     &    'Perugia 0','Perugia HARD','Perugia SOFT',
     &    'Perugia 3','Perugia NOCR','Perugia LO*',
     &    'Perugia 6',2*' ','Pro-pT0'/
      DATA (CHNAMS(I),I=200,229)/
     &    'IM Tune 1','Tune APT',8*' ',
     &    ' ','Tune APT-Pro',8*' ',
     &    ' ','Perugia APT',4*' ','Perugia APT6',3*' '/
      DATA (CHNAMS(I),I=400,409)/
     &    'GAL Tune 0','SCI Tune 0','GAL Tune 1','SCI Tune 1',6*' '/
      DATA (CHMSTJ(I),I=11,20)/
     &    'HAD choice of fragmentation function(s)',4*' ',
     &    'HAD treatment of small-mass systems',4*' '/
      DATA (CHMSTJ(I),I=41,50)/
     &    'FSR type (Q2 or pT) for old framework',9*' '/
      DATA (CHMSTP(I),I=51,100)/
     5    'PDF set','PDF set internal (=1) or pdflib (=2)',8*' ',
     6    'ISR master switch',2*' ','ISR alphaS type',2*' ',
     6    'ISR coherence option for 1st emission',
     6    'ISR phase space choice & ME corrections',' ',
     7    'ISR IR regularization scheme',' ',
     7    'ISR scheme for FSR off ISR',8*' ',
     8    'UE model',
     8    'UE hadron transverse mass distribution',5*' ',
     8    'BR composite scheme','BR colour scheme',
     9    'BR primordial kT compensation',
     9    'BR primordial kT distribution',
     9    'BR energy partitioning scheme',2*' ',
     9    'FSI colour (re-)connection model',5*' '/
      DATA (CHPARP(I),I=61,100)/
     6    ' ','ISR IR cutoff',' ','ISR renormalization scale prefactor',
     6    2*' ','ISR Q2max factor',3*' ',
     7    'FSR Q2max factor for non-s-channel procs',5*' ',
     7    'FSI colour reco high-pT dampening strength',
     7    'FSI colour reconnection strength',
     7    'BR composite x enhancement','BR breakup suppression',
     8    2*'UE IR cutoff at reference ecm',
     8    2*'UE mass distribution parameter',
     8    'UE gg colour correlated fraction','UE total gg fraction',
     8    2*' ',
     8    'UE IR cutoff reference ecm','UE IR cutoff ecm scaling power',
     9    'BR primordial kT width <|kT|>',' ',
     9    'BR primordial kT UV cutoff',7*' '/
      DATA (CHPARJ(I),I=1,30)/
     &    'HAD diquark suppression','HAD strangeness suppression',
     &    'HAD strange diquark suppression',
     &    'HAD vector diquark suppression',6*' ',
     1    'HAD P(vector meson), u and d only',
     1    'HAD P(vector meson), contains s',
     1    'HAD P(vector meson), heavy quarks',7*' ',
     2    'HAD fragmentation pT',' ',' ',' ',
     2    'HAD eta0 suppression',"HAD eta0' suppression",4*' '/
      DATA (CHPARJ(I),I=41,90)/
     4    'HAD string parameter a','HAD string parameter b',3*' ',
     4    'HAD Lund(=0)-Bowler(=1) rQ (rc)',
     4    'HAD Lund(=0)-Bowler(=1) rb',3*' ',
     5    3*' ','HAD charm parameter','HAD bottom parameter',5*' ',
     6    10*' ',10*' ',
     8    'FSR Lambda_QCD scale','FSR IR cutoff',8*' '/
 
C...1) Shorthand notation
      M13=MSTU(13)
      M11=MSTU(11)
      IF (ITUNE.LE.MXTUNS.AND.ITUNE.GE.0) THEN
        CHNAME=CHNAMS(ITUNE)
        IF (ITUNE.EQ.0) GOTO 9999
      ELSE
        CALL PYERRM(9,'(PYTUNE:) Tune number > max. Using defaults.')
        GOTO 9999
      ENDIF
 
C...2) Hello World
      IF (M13.GE.1) WRITE(M11,5000) CHVERS, CHDOC
 
C...3) Tune parameters
 
C=======================================================================
C...S0, S1, S2, S0A, NOCR, Rap,
C...S0-Pro, S1-Pro, S2-Pro, S0A-Pro, NOCR-Pro, Rap-Pro
C...Perugia 0, HARD, SOFT, Perugia 3, Perugia LO*, Perugia 6
C...Pro-pT0
      IF ((ITUNE.GE.300.AND.ITUNE.LE.305)
     &    .OR.(ITUNE.GE.310.AND.ITUNE.LE.315)
     &    .OR.(ITUNE.GE.320.AND.ITUNE.LE.326).OR.ITUNE.EQ.329) THEN
        IF (M13.GE.1) WRITE(M11,5010) ITUNE, CHNAME
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.405))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')
        ELSEIF(ITUNE.GE.320.AND.ITUNE.LE.326.AND.ITUNE.NE.324.AND.
     &        (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.419)))
     &        THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')
        ENDIF
 
C...Use Professor's LEP pars if ITUNE >= 310
C...(i.e., for S0-Pro, S1-Pro etc, and for Perugia tunes)
        IF (ITUNE.LT.310) THEN
C...# Old default flavour parameters
 
        ELSEIF (ITUNE.GE.310) THEN
C...# Tuned flavour parameters:
          PARJ(1)  = 0.073
          PARJ(2)  = 0.2
          PARJ(3)  = 0.94
          PARJ(4)  = 0.032
          PARJ(11) = 0.31
          PARJ(12) = 0.4
          PARJ(13) = 0.54
          PARJ(25) = 0.63
          PARJ(26) = 0.12
C...# Always use pT-ordered shower:
          MSTJ(41) = 12
C...# Switch on Bowler:
          MSTJ(11) = 5
C...# Fragmentation
          PARJ(21) = 0.313
          PARJ(41) = 0.49
          PARJ(42) = 1.2
          PARJ(47) = 1.0
          PARJ(81) = 0.257
          PARJ(82) = 0.8
        ENDIF
 
C...Remove middle digit now for Professor variants, since identical pars
        ITUNEB=ITUNE
        IF (ITUNE.GE.310.AND.ITUNE.LE.319) THEN
          ITUNEB=(ITUNE/100)*100+MOD(ITUNE,10)
        ENDIF
 
C...PDFs: all use CTEQ5L as starting point
        MSTP(52)=1
        MSTP(51)=7
        IF (ITUNE.EQ.325) THEN
C...MRST LO* for 325
          MSTP(52)=2
          MSTP(51)=20650
        ELSEIF (ITUNE.EQ.326) THEN
C...CTEQ6L1 for 326
          MSTP(52)=2
          MSTP(51)=10042
        ENDIF
 
C...ISR: use Lambda_MSbar with default scale for S0(A)
        MSTP(64)=2
        PARP(64)=1D0
        IF (ITUNE.EQ.320.OR.ITUNE.EQ.323.OR.ITUNE.EQ.324.OR.
     &      ITUNE.EQ.326) THEN
C...Use Lambda_MC with muR^2=pT^2 for most central Perugia tunes
          MSTP(64)=3
          PARP(64)=1D0
        ELSEIF (ITUNE.EQ.321) THEN
C...Use Lambda_MC with muR^2=(1/2pT)^2 for Perugia HARD
          MSTP(64)=3
          PARP(64)=0.25D0
        ELSEIF (ITUNE.EQ.322) THEN
C...Use Lambda_MSbar with muR^2=2pT^2 for Perugia SOFT
          MSTP(64)=2
          PARP(64)=2D0
        ELSEIF (ITUNE.EQ.325) THEN
C...Use Lambda_MC with muR^2=2pT^2 for Perugia LO*
          MSTP(64)=3
          PARP(64)=2D0
        ELSEIF (ITUNE.EQ.329) THEN
C...Use Lambda_MSbar with P64=1.3 for Pro-pT0
          MSTP(64)=2
          PARP(64)=1.3D0
        ENDIF
 
C...ISR : power-suppressed power showers above s_color (since 6.4.19)
        MSTP(67)=2
        PARP(67)=4D0
C...Perugia tunes have stronger suppression, except HARD
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN
          PARP(67)=1D0
          IF (ITUNE.EQ.321) PARP(67)=4D0
          IF (ITUNE.EQ.322) PARP(67)=0.5D0
        ENDIF
 
C...ISR IR cutoff type and FSR off ISR setting:
C...Smooth ISR, low FSR-off-ISR
        MSTP(70)=2
        MSTP(72)=0
        IF (ITUNEB.EQ.301) THEN
C...S1, S1-Pro: sharp ISR, high FSR
          MSTP(70)=0
          MSTP(72)=1
        ELSEIF (ITUNE.EQ.320.OR.ITUNE.EQ.324.OR.ITUNE.EQ.326
     &        .OR.ITUNE.EQ.325) THEN
C...Perugia default is smooth ISR, high FSR-off-ISR
          MSTP(70)=2
          MSTP(72)=1
        ELSEIF (ITUNE.EQ.321) THEN
C...Perugia HARD: sharp ISR, high FSR-off-ISR (but no dip-to-BR rad)
          MSTP(70)=0
          PARP(62)=1.25D0
          MSTP(72)=1
        ELSEIF (ITUNE.EQ.322) THEN
C...Perugia SOFT: scaling sharp ISR, low FSR-off-ISR
          MSTP(70)=1
          PARP(81)=1.5D0
          MSTP(72)=0
        ELSEIF (ITUNE.EQ.323) THEN
C...Perugia 3: sharp ISR, high FSR-off-ISR (with dipole-to-BR radiating)
          MSTP(70)=0
          PARP(62)=1.25D0
          MSTP(72)=2
        ENDIF
 
C...FSR activity: Perugia tunes use a lower PARP(71) as indicated 
C...by Professor tunes (with HARD and SOFT variations)
        PARP(71)=4D0
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN 
          PARP(71)=2D0
          IF (ITUNE.EQ.321) PARP(71)=4D0
          IF (ITUNE.EQ.322) PARP(71)=1D0
        ENDIF

C...FSR: Lambda_FSR scale (only if not using professor)
        IF (ITUNE.LT.310) PARJ(81)=0.23D0
        IF (ITUNE.EQ.321) PARJ(81)=0.30D0
        IF (ITUNE.EQ.322) PARJ(81)=0.20D0
 
C...UE on, new model
        MSTP(81)=21
 
C...UE: hadron-hadron overlap profile (expOfPow for all)
        MSTP(82)=5
C...UE: Overlap smoothness (1.0 = exponential; 2.0 = gaussian)
        PARP(83)=1.6D0
        IF (ITUNEB.EQ.301) PARP(83)=1.4D0
        IF (ITUNEB.EQ.302) PARP(83)=1.2D0
C...NOCR variants have very smooth distributions
        IF (ITUNEB.EQ.304) PARP(83)=1.8D0
        IF (ITUNEB.EQ.305) PARP(83)=2.0D0
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN
C...Perugia variants have slightly smoother profiles by default
C...(to compensate for more tail by added radiation)
C...Perugia-SOFT has more peaked distribution, NOCR less peaked
          PARP(83)=1.7D0
          IF (ITUNE.EQ.322) PARP(83)=1.5D0
          IF (ITUNE.EQ.324) PARP(83)=1.8D0
        ENDIF
C...Professor-pT0 also has very smooth distribution
        IF (ITUNE.EQ.329) PARP(83)=1.8
 
C...UE: pT0 = 1.85 for S0, S0A, 2.0 for Perugia version
        PARP(82)=1.85D0
        IF (ITUNEB.EQ.301) PARP(82)=2.1D0
        IF (ITUNEB.EQ.302) PARP(82)=1.9D0
        IF (ITUNEB.EQ.304) PARP(82)=2.05D0
        IF (ITUNEB.EQ.305) PARP(82)=1.9D0
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN
C...Perugia tunes (def is 2.0 GeV, HARD has higher, SOFT has lower,
C...Perugia-3 has more ISR, so higher pT0, NOCR can be slightly lower,
C...CTEQ6L1 slightly lower, due to less activity, and LO* needs to be
C...slightly higher, due to increased activity.
          PARP(82)=2.0D0
          IF (ITUNE.EQ.321) PARP(82)=2.3D0
          IF (ITUNE.EQ.322) PARP(82)=1.9D0
          IF (ITUNE.EQ.323) PARP(82)=2.2D0
          IF (ITUNE.EQ.324) PARP(82)=1.95D0
          IF (ITUNE.EQ.325) PARP(82)=2.2D0
          IF (ITUNE.EQ.326) PARP(82)=1.95D0
        ENDIF
C...Professor-pT0 maintains low pT0 vaue
        IF (ITUNE.EQ.329) PARP(82)=1.85D0
 
C...UE: IR cutoff reference energy and default energy scaling pace
        PARP(89)=1800D0
        PARP(90)=0.16D0
C...S0A, S0A-Pro have tune A energy scaling
        IF (ITUNEB.EQ.303) PARP(90)=0.25D0
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN
C...Perugia tunes explicitly include MB at 630 to fix energy scaling
          PARP(90)=0.26
          IF (ITUNE.EQ.321) PARP(90)=0.30D0
          IF (ITUNE.EQ.322) PARP(90)=0.24D0
          IF (ITUNE.EQ.323) PARP(90)=0.32D0
          IF (ITUNE.EQ.324) PARP(90)=0.24D0
C...LO* and CTEQ6L1 tunes have slower energy scaling
          IF (ITUNE.EQ.325) PARP(90)=0.23D0
          IF (ITUNE.EQ.326) PARP(90)=0.22D0
        ENDIF
C...Professor-pT0 has intermediate scaling
        IF (ITUNE.EQ.329) PARP(90)=0.22D0
 
C...BR: MPI initiator color connections rap-ordered by default
C...NOCR variants are Lambda-ordered, Perugia SOFT is random-ordered
        MSTP(89)=1
        IF (ITUNEB.EQ.304.OR.ITUNE.EQ.324) MSTP(89)=2
        IF (ITUNE.EQ.322) MSTP(89)=0
 
C...BR: BR-g-BR suppression factor (higher values -> more beam blowup)
        PARP(80)=0.01D0
        IF (ITUNE.GE.320.AND.ITUNE.LE.326) THEN
C...Perugia tunes have more beam blowup by default
          PARP(80)=0.05D0
          IF (ITUNE.EQ.321) PARP(80)=0.01
          IF (ITUNE.EQ.323) PARP(80)=0.03
          IF (ITUNE.EQ.324) PARP(80)=0.01
        ENDIF
 
C...BR: diquarks (def = valence qq and moderate diquark x enhancement)
        MSTP(88)=0
        PARP(79)=2D0
        IF (ITUNEB.EQ.304) PARP(79)=3D0
        IF (ITUNE.EQ.329) PARP(79)=1.18
 
C...BR: Primordial kT, parametrization and cutoff, default is 2 GeV
        MSTP(91)=1
        PARP(91)=2D0
        PARP(93)=10D0
C...Perugia-HARD only uses 1.0 GeV
        IF (ITUNE.EQ.321) PARP(91)=1.0D0
C...Perugia-3 only uses 1.5 GeV
        IF (ITUNE.EQ.323) PARP(91)=1.5D0
C...Professor-pT0 uses 7-GeV cutoff
        IF (ITUNE.EQ.329) PARP(93)=7.0
 
C...FSI: Colour Reconnections - Seattle algorithm is default (S0)
        MSTP(95)=6
C...S1, S1-Pro: use S1
        IF (ITUNEB.EQ.301) MSTP(95)=2
C...S2, S2-Pro: use S2
        IF (ITUNEB.EQ.302) MSTP(95)=4
C...NOCR, NOCR-Pro, Perugia-NOCR: use no CR
        IF (ITUNE.EQ.304.OR.ITUNE.EQ.314.OR.ITUNE.EQ.324) MSTP(95)=0
C..."Old" and "Old"-Pro: use old CR
        IF (ITUNEB.EQ.305) MSTP(95)=1
 
C...FSI: CR strength and high-pT dampening, default is S0
        IF (ITUNE.LT.320.OR.ITUNE.EQ.329) THEN
          PARP(78)=0.2D0
          PARP(77)=0D0
          IF (ITUNEB.EQ.301) PARP(78)=0.35D0
          IF (ITUNEB.EQ.302) PARP(78)=0.15D0
          IF (ITUNEB.EQ.304) PARP(78)=0.0D0
          IF (ITUNEB.EQ.305) PARP(78)=1.0D0
          IF (ITUNE.EQ.329) PARP(78)=0.17D0
        ELSE
C...Perugia tunes also use high-pT dampening : default is Perugia 0,*,6
          PARP(78)=0.33
          PARP(77)=0.9D0
          IF (ITUNE.EQ.321) THEN
C...HARD has HIGH amount of CR
            PARP(78)=0.37D0
            PARP(77)=0.4D0
          ELSEIF (ITUNE.EQ.322) THEN
C...SOFT has LOW amount of CR
            PARP(78)=0.15D0
            PARP(77)=0.5D0
          ELSEIF (ITUNE.EQ.323) THEN
C...Scaling variant appears to need slightly more than default
            PARP(78)=0.35D0
            PARP(77)=0.6D0
          ELSEIF (ITUNE.EQ.324) THEN
C...NOCR has no CR
            PARP(78)=0D0
            PARP(77)=0D0
          ENDIF
        ENDIF
 
C...HAD: fragmentation pT (only if not using professor) - HARD and SOFT
        IF (ITUNE.EQ.321) PARJ(21)=0.34D0
        IF (ITUNE.EQ.322) PARJ(21)=0.28D0
 
C...Switch off trial joinings
        MSTP(96)=0
 
C...S0 (300), S0A (303)
        IF (ITUNEB.EQ.300.OR.ITUNEB.EQ.303) THEN
          IF (M13.GE.1) THEN
            CH60='see P. Skands & D. Wicke, hep-ph/0703081'
            WRITE(M11,5030) CH60
            CH60='M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            IF (ITUNE.GE.310) THEN
              CH60='LEP parameters tuned by Professor'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C...S1 (301)
        ELSEIF(ITUNEB.EQ.301) THEN
          IF (M13.GE.1) THEN
            CH60='see M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            IF (ITUNE.GE.310) THEN
              CH60='LEP parameters tuned with Professor'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C...S2 (302)
        ELSEIF(ITUNEB.EQ.302) THEN
          IF (M13.GE.1) THEN
            CH60='see M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            IF (ITUNE.GE.310) THEN
              CH60='LEP parameters tuned by Professor'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C...NOCR (304)
        ELSEIF(ITUNEB.EQ.304) THEN
          IF (M13.GE.1) THEN
            CH60='"best try" without colour reconnections'
            WRITE(M11,5030) CH60
            CH60='see P. Skands & D. Wicke, hep-ph/0703081'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            IF (ITUNE.GE.310) THEN
              CH60='LEP parameters tuned by Professor'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C..."Lo FSR" retune (305)
        ELSEIF(ITUNEB.EQ.305) THEN
          IF (M13.GE.1) THEN
            CH60='"Lo FSR retune" with primitive colour reconnections'
            WRITE(M11,5030) CH60
            CH60='see T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            IF (ITUNE.GE.310) THEN
              CH60='LEP parameters tuned by Professor'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C...Perugia Tunes (320-326)
        ELSEIF(ITUNE.GE.320.AND.ITUNE.LE.326) THEN
          IF (M13.GE.1) THEN
            CH60='P. Skands, Perugia MPI workshop October 2008'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            CH60='CR by M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='LEP parameters tuned by Professor'
            WRITE(M11,5030) CH60
            IF (ITUNE.EQ.325) THEN
              CH70='NB! This tune requires MRST LO* pdfs to be '//
     &            'externally linked'
              WRITE(M11,5035) CH70
            ELSEIF (ITUNE.EQ.326) THEN
              CH70='NB! This tune requires CTEQ6L1 pdfs to be '//
     &            'externally linked'
              WRITE(M11,5035) CH70
            ELSEIF (ITUNE.EQ.321) THEN
              CH60='NB! This tune has MORE ISR & FSR / LESS UE & BR'
              WRITE(M11,5030) CH60
            ELSEIF (ITUNE.EQ.322) THEN
              CH60='NB! This tune has LESS ISR & FSR / MORE UE & BR'
              WRITE(M11,5030) CH60
            ENDIF
          ENDIF
 
C...Professor-pT0 (329)
        ELSEIF(ITUNE.EQ.329) THEN
          IF (M13.GE.1) THEN
            CH60='See T. Sjostrand & P. Skands, hep-ph/0408302'
            WRITE(M11,5030) CH60
            CH60='and M. Sandhoff & P. Skands, in hep-ph/0604120'
            WRITE(M11,5030) CH60
            CH60='LEP/Tevatron parameters tuned by Professor'
            WRITE(M11,5030) CH60
          ENDIF
 
        ENDIF
 
C...Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          IF (MSTP(70).EQ.0) THEN
            WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          ELSEIF (MSTP(70).EQ.1) THEN
            WRITE(M11,5050) 81, PARP(81), CHPARP(62)
            CH60='(Note: PARP(81) replaces PARP(62).)'
            WRITE(M11,5030) CH60
          ENDIF
          WRITE(M11,5040) 64, MSTP(64), CHMSTP(64)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5040) 67, MSTP(67), CHMSTP(67)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5040) 70, MSTP(70), CHMSTP(70)
          WRITE(M11,5040) 72, MSTP(72), CHMSTP(72)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5060) 82, PARJ(82), CHPARJ(82)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          IF (MSTP(70).EQ.2) THEN
            CH60='(Note: PARP(82) replaces PARP(62).)'
            WRITE(M11,5030) CH60
          ENDIF
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5040) 88, MSTP(88), CHMSTP(88)
          WRITE(M11,5040) 89, MSTP(89), CHMSTP(89)
          WRITE(M11,5050) 79, PARP(79), CHPARP(79)
          WRITE(M11,5050) 80, PARP(80), CHPARP(80)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          IF (MSTP(95).GE.1) THEN
            WRITE(M11,5050) 78, PARP(78), CHPARP(78)
            IF (MSTP(95).GE.2) WRITE(M11,5050) 77, PARP(77), CHPARP(77)
          ENDIF
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C=======================================================================
C...ATLAS-CSC 11-parameter tune (By A. Moraes)
      ELSEIF (ITUNE.EQ.306) THEN
        IF (M13.GE.1) WRITE(M11,5010) ITUNE, CHNAME
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.405))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')
        ENDIF
 
C...PDFs
        MSTP(52)=2
        MSTP(54)=2
        MSTP(51)=10042
        MSTP(53)=10042
C...ISR
C        PARP(64)=1D0
C...UE on, new model.
        MSTP(81)=21
C...Energy scaling
        PARP(89)=1800D0
        PARP(90)=0.22D0
C...Switch off trial joinings
        MSTP(96)=0
C...Primordial kT cutoff
 
        IF (M13.GE.1) THEN
          CH60='see presentations by A. Moraes (ATLAS),'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
          WRITE(M11,5030) CH60
          WRITE(M11,5030) ' '
          CH70='NB! This tune requires CTEQ6.1 pdfs to be '//
     &        'externally linked'
          WRITE(M11,5035) CH70
        ENDIF
C...Smooth ISR, low FSR
        MSTP(70)=2
        MSTP(72)=0
C...pT0
        PARP(82)=1.9D0
C...Transverse density profile.
        MSTP(82)=4
        PARP(83)=0.3D0
        PARP(84)=0.5D0
C...ISR & FSR in interactions after the first (default)
        MSTP(84)=1
        MSTP(85)=1
C...No double-counting (default)
        MSTP(86)=2
C...Companion quark parent gluon (1-x) power
        MSTP(87)=4
C...Primordial kT compensation along chaings (default = 0 : uniform)
        MSTP(90)=1
C...Colour Reconnections
        MSTP(95)=1
        PARP(78)=0.2D0
C...Lambda_FSR scale.
        PARJ(81)=0.23D0
C...Rap order, Valence qq, qq x enhc, BR-g-BR supp
        MSTP(89)=1
        MSTP(88)=0
C   PARP(79)=2D0
        PARP(80)=0.01D0
C...Peterson charm frag, and c and b hadr parameters
        MSTJ(11)=3
        PARJ(54)=-0.07
        PARJ(55)=-0.006
C...  Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5040) 70, MSTP(70), CHMSTP(70)
          WRITE(M11,5040) 72, MSTP(72), CHMSTP(72)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          CH60='(Note: PARJ(81) changed from 0.14! See update notes)'
          WRITE(M11,5030) CH60
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5040) 88, MSTP(88), CHMSTP(88)
          WRITE(M11,5040) 89, MSTP(89), CHMSTP(89)
          WRITE(M11,5040) 90, MSTP(90), CHMSTP(90)
          WRITE(M11,5050) 79, PARP(79), CHPARP(79)
          WRITE(M11,5050) 80, PARP(80), CHPARP(80)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C=======================================================================
C...Tunes A, AW, BW, DW, DWT, QW, D6, D6T (by R.D. Field, CDF)
C...(100-105,108-109), ATLAS-DC2 Tune (by A. Moraes, ATLAS) (106)
C...A-Pro, DW-Pro, etc (100-119), and Pro-Q20 (129)
      ELSEIF ((ITUNE.GE.100.AND.ITUNE.LE.106).OR.ITUNE.EQ.108.OR.
     &      ITUNE.EQ.109.OR.(ITUNE.GE.110.AND.ITUNE.LE.116).OR.
     &      ITUNE.EQ.118.OR.ITUNE.EQ.119.OR.ITUNE.EQ.129) THEN
        IF (M13.GE.1.AND.ITUNE.NE.106.AND.ITUNE.NE.129) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see R.D. Field, in hep-ph/0610012'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          IF (ITUNE.GE.110.AND.ITUNE.LE.119) THEN
            CH60='LEP parameters tuned by Professor'
            WRITE(M11,5030) CH60
          ENDIF
        ELSEIF (M13.GE.1.AND.ITUNE.EQ.129) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='See T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          CH60='LEP/Tevatron parameters tuned by Professor'
          WRITE(M11,5030) CH60
        ENDIF
 
C...Make sure we start from old default fragmentation parameters
        PARJ(81) = 0.29
        PARJ(82) = 1.0
 
C...Use Professor's LEP pars if ITUNE >= 110
C...(i.e., for A-Pro, DW-Pro etc)
        IF (ITUNE.GE.110) THEN
C...# Tuned flavour parameters:
          PARJ(1)  = 0.073
          PARJ(2)  = 0.2
          PARJ(3)  = 0.94
          PARJ(4)  = 0.032
          PARJ(11) = 0.31
          PARJ(12) = 0.4
          PARJ(13) = 0.54
          PARJ(25) = 0.63
          PARJ(26) = 0.12
C...# Switch on Bowler:
          MSTJ(11) = 5
C...# Fragmentation
          PARJ(21) = 0.325
          PARJ(41) = 0.5
          PARJ(42) = 0.6
          PARJ(47) = 0.67
          PARJ(81) = 0.29
          PARJ(82) = 1.65
        ENDIF
 
C...Remove middle digit now for Professor variants, since identical pars
        ITUNEB=ITUNE
        IF (ITUNE.GE.110.AND.ITUNE.LE.119) THEN
          ITUNEB=(ITUNE/100)*100+MOD(ITUNE,10)
        ENDIF
 
C...Multiple interactions on, old framework
        MSTP(81)=1
C...Fast IR cutoff energy scaling by default
        PARP(89)=1800D0
        PARP(90)=0.25D0
C...Default CTEQ5L (internal), except for QW: CTEQ61 (external)
        MSTP(51)=7
        MSTP(52)=1
        IF (ITUNEB.EQ.105) THEN
          MSTP(51)=10150
          MSTP(52)=2
        ELSEIF(ITUNEB.EQ.108.OR.ITUNEB.EQ.109) THEN
          MSTP(52)=2
          MSTP(54)=2
          MSTP(51)=10042
          MSTP(53)=10042
        ENDIF
C...Double Gaussian matter distribution.
        MSTP(82)=4
        PARP(83)=0.5D0
        PARP(84)=0.4D0
C...FSR activity.
        PARP(71)=4D0
C...Fragmentation functions and c and b parameters
C...(only if not using Professor)
        IF (ITUNE.LE.109) THEN
          MSTJ(11)=4
          PARJ(54)=-0.05
          PARJ(55)=-0.005
        ENDIF
 
C...Tune A and AW
        IF(ITUNEB.EQ.100.OR.ITUNEB.EQ.101) THEN
C...pT0.
          PARP(82)=2.0D0
c...String drawing almost completely minimizes string length.
          PARP(85)=0.9D0
          PARP(86)=0.95D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1D0
          PARP(64)=1D0
          PARP(67)=4D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=1D0
          PARP(93)=5D0
C...AW : higher ISR IR cutoff, but also larger alphaS, more intrinsic kT
          IF (ITUNEB.EQ.101) THEN
            PARP(62)=1.25D0
            PARP(64)=0.2D0
            PARP(91)=2.1D0
            PARP(92)=15.0D0
          ENDIF
 
C...Tune BW (larger alphaS, more intrinsic kT. Smaller ISR phase space)
        ELSEIF (ITUNEB.EQ.102) THEN
C...pT0.
          PARP(82)=1.9D0
c...String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=1D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0
 
C...Tune DW
        ELSEIF (ITUNEB.EQ.103) THEN
C...pT0.
          PARP(82)=1.9D0
c...String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0
 
C...Tune DWT
        ELSEIF (ITUNEB.EQ.104) THEN
C...pT0.
          PARP(82)=1.9409D0
C...Run II ref scale and slow scaling
          PARP(89)=1960D0
          PARP(90)=0.16D0
c...String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0
 
C...Tune QW
        ELSEIF(ITUNEB.EQ.105) THEN
          IF (M13.GE.1) THEN
            WRITE(M11,5030) ' '
            CH70='NB! This tune requires CTEQ6.1 pdfs to be '//
     &           'externally linked'
            WRITE(M11,5035) CH70
          ENDIF
C...pT0.
          PARP(82)=1.1D0
c...String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0
 
C...Tune D6 and D6T
        ELSEIF(ITUNEB.EQ.108.OR.ITUNEB.EQ.109) THEN
          IF (M13.GE.1) THEN
            WRITE(M11,5030) ' '
            CH70='NB! This tune requires CTEQ6L pdfs to be '//
     &           'externally linked'
            WRITE(M11,5035) CH70
          ENDIF
C...The "Rick" proton, double gauss with 0.5/0.4
          MSTP(82)=4
          PARP(83)=0.5D0
          PARP(84)=0.4D0
c...String drawing completely minimizes string length.
          PARP(85)=1.0D0
          PARP(86)=1.0D0
          IF (ITUNEB.EQ.108) THEN
C...D6: pT0, Run I ref scale, and fast energy scaling
            PARP(82)=1.8D0
            PARP(89)=1800D0
            PARP(90)=0.25D0
          ELSE
C...D6T: pT0, Run II ref scale, and slow energy scaling
            PARP(82)=1.8387D0
            PARP(89)=1960D0
            PARP(90)=0.16D0
          ENDIF
C...ISR cutoff, muR scale factor, and phase space size
          PARP(62)=1.25D0
          PARP(64)=0.2D0
          PARP(67)=2.5D0
C...Intrinsic kT, size, and max
          MSTP(91)=1
          PARP(91)=2.1D0
          PARP(93)=15D0
 
C...Old ATLAS-DC2 5-parameter tune
        ELSEIF(ITUNEB.EQ.106) THEN
          IF (M13.GE.1) THEN
            WRITE(M11,5010) ITUNE, CHNAME
            CH60='see A. Moraes et al., SN-ATLAS-2006-057,'
            WRITE(M11,5030) CH60
            CH60='    R. Field in hep-ph/0610012,'
            WRITE(M11,5030) CH60
            CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
            WRITE(M11,5030) CH60
          ENDIF
C...  pT0.
          PARP(82)=1.8D0
C...  Different ref and rescaling pacee
          PARP(89)=1000D0
          PARP(90)=0.16D0
C...  Parameters of mass distribution
          PARP(83)=0.5D0
          PARP(84)=0.5D0
C...  Old default string drawing
          PARP(85)=0.33D0
          PARP(86)=0.66D0
C...  ISR, phase space equivalent to Tune B
          PARP(62)=1D0
          PARP(64)=1D0
          PARP(67)=1D0
C...  FSR
          PARP(71)=4D0
C...  Intrinsic kT
          MSTP(91)=1
          PARP(91)=1D0
          PARP(93)=5D0
 
C...Professor's Pro-Q20 Tune
        ELSEIF(ITUNE.EQ.129) THEN
          IF (M13.GE.1) THEN
            CH60='see H. Hoeth, Perugia MPI workshop, Oct 2008'
            WRITE(M11,5030) CH60
          ENDIF
          PARP(62)=2.9
          PARP(64)=0.14
          PARP(67)=2.65
          PARP(82)=1.9
          PARP(83)=0.83
          PARP(84)=0.6
          PARP(85)=0.86
          PARP(86)=0.93
          PARP(89)=1800D0
          PARP(90)=0.22
          MSTP(91)=1
          PARP(91)=2.1
          PARP(93)=5.0
 
        ENDIF
 
C...  Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5060) 82, PARJ(82), CHPARJ(82)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5050) 85, PARP(85), CHPARP(85)
          WRITE(M11,5050) 86, PARP(86), CHPARP(86)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C=======================================================================
C... ACR, tune A with new CR (107)
      ELSEIF(ITUNE.EQ.107.OR.ITUNE.EQ.117) THEN
        IF (M13.GE.1) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='Tune A modified with new colour reconnections'
          WRITE(M11,5030) CH60
          CH60='PARP(85)=0D0 and amount of CR is regulated by PARP(78)'
          WRITE(M11,5030) CH60
          CH60='see P. Skands & D. Wicke, hep-ph/0703081,'
          WRITE(M11,5030) CH60
          CH60='    R. Field, in hep-ph/0610012 (Tune A),'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          IF (ITUNE.EQ.117) THEN
            CH60='LEP parameters tuned by Professor'
            WRITE(M11,5030) CH60
          ENDIF
        ENDIF
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.406))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune. Using defaults.')
          GOTO 100
        ENDIF
 
C...Make sure we start from old default fragmentation parameters
        PARJ(81) = 0.29
        PARJ(82) = 1.0
 
C...Use Professor's LEP pars if ITUNE >= 110
C...(i.e., for A-Pro, DW-Pro etc)
        IF (ITUNE.GE.110) THEN
C...# Tuned flavour parameters:
          PARJ(1)  = 0.073
          PARJ(2)  = 0.2
          PARJ(3)  = 0.94
          PARJ(4)  = 0.032
          PARJ(11) = 0.31
          PARJ(12) = 0.4
          PARJ(13) = 0.54
          PARJ(25) = 0.63
          PARJ(26) = 0.12
C...# Switch on Bowler:
          MSTJ(11) = 5
C...# Fragmentation
          PARJ(21) = 0.325
          PARJ(41) = 0.5
          PARJ(42) = 0.6
          PARJ(47) = 0.67
          PARJ(81) = 0.29
          PARJ(82) = 1.65
        ENDIF
 
        MSTP(81)=1
        PARP(89)=1800D0
        PARP(90)=0.25D0
        MSTP(82)=4
        PARP(83)=0.5D0
        PARP(84)=0.4D0
        MSTP(51)=7
        MSTP(52)=1
        PARP(71)=4D0
        PARP(82)=2.0D0
        PARP(85)=0.0D0
        PARP(86)=0.66D0
        PARP(62)=1D0
        PARP(64)=1D0
        PARP(67)=4D0
        MSTP(91)=1
        PARP(91)=1D0
        PARP(93)=5D0
        MSTP(95)=6
C...P78 changed from 0.12 to 0.09 in 6.4.19 to improve <pT>(Nch)
        PARP(78)=0.09D0
C...Frag functions (only if not using Professor)
        IF (ITUNE.LE.109) THEN
          MSTJ(11)=4
          PARJ(54)=-0.05
          PARJ(55)=-0.005
        ENDIF
 
C...Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5060) 82, PARJ(82), CHPARJ(82)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5050) 85, PARP(85), CHPARP(85)
          WRITE(M11,5050) 86, PARP(86), CHPARP(86)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C=======================================================================
C...Intermediate model. Rap tune
C...(retuned to post-6.406 IR factorization)
      ELSEIF(ITUNE.EQ.200) THEN
        IF (M13.GE.1) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see T. Sjostrand & P. Skands, JHEP03(2004)053'
          WRITE(M11,5030) CH60
        ENDIF
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.405))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')
        ENDIF
C...PDF
        MSTP(51)=7
        MSTP(52)=1
C...ISR
        PARP(62)=1D0
        PARP(64)=1D0
        PARP(67)=4D0
C...FSR
        PARP(71)=4D0
        PARJ(81)=0.29D0
C...UE
        MSTP(81)=11
        PARP(82)=2.25D0
        PARP(89)=1800D0
        PARP(90)=0.25D0
C...  ExpOfPow(1.8) overlap profile
        MSTP(82)=5
        PARP(83)=1.8D0
C...  Valence qq
        MSTP(88)=0
C...  Rap Tune
        MSTP(89)=1
C...  Default diquark, BR-g-BR supp
        PARP(79)=2D0
        PARP(80)=0.01D0
C...  Final state reconnect.
        MSTP(95)=1
        PARP(78)=0.55D0
C...Fragmentation functions and c and b parameters
        MSTJ(11)=4
        PARJ(54)=-0.05
        PARJ(55)=-0.005
C...  Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5040) 88, MSTP(88), CHMSTP(88)
          WRITE(M11,5040) 89, MSTP(89), CHMSTP(89)
          WRITE(M11,5050) 79, PARP(79), CHPARP(79)
          WRITE(M11,5050) 80, PARP(80), CHPARP(80)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5040) 95, MSTP(95), CHMSTP(95)
          WRITE(M11,5050) 78, PARP(78), CHPARP(78)
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C...APT(201), APT-Pro (211), Perugia-APT (221), Perugia-APT6 (226).
C...Old model for ISR and UE, new pT-ordered model for FSR
      ELSEIF(ITUNE.EQ.201.OR.ITUNE.EQ.211.OR.ITUNE.EQ.221.OR
     &       .ITUNE.EQ.226) THEN
        IF (M13.GE.1) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see P. Skands & D. Wicke, hep-ph/0703081 (Tune APT),'
          WRITE(M11,5030) CH60
          CH60='    R.D. Field, in hep-ph/0610012 (Tune A)'
          WRITE(M11,5030) CH60
          CH60='    T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & P. Skands, hep-ph/0408302'
          WRITE(M11,5030) CH60
          IF (ITUNE.EQ.211.OR.ITUNE.GE.221) THEN
            CH60='LEP parameters tuned by Professor'
            WRITE(M11,5030) CH60
          ENDIF
        ENDIF
        IF (MSTP(181).LE.5.OR.(MSTP(181).EQ.6.AND.MSTP(182).LE.411))THEN
          CALL PYERRM(9,'(PYTUNE:) linked PYTHIA version incompatible'//
     &        ' with tune.')
        ENDIF
C...First set as if Pythia tune A
C...Multiple interactions on, old framework
        MSTP(81)=1
C...Fast IR cutoff energy scaling by default
        PARP(89)=1800D0
        PARP(90)=0.25D0
C...Default CTEQ5L (internal)
        MSTP(51)=7
        MSTP(52)=1
C...Double Gaussian matter distribution.
        MSTP(82)=4
        PARP(83)=0.5D0
        PARP(84)=0.4D0
C...FSR activity.
        PARP(71)=4D0
c...String drawing almost completely minimizes string length.
        PARP(85)=0.9D0
        PARP(86)=0.95D0
C...ISR cutoff, muR scale factor, and phase space size
        PARP(62)=1D0
        PARP(64)=1D0
        PARP(67)=4D0
C...Intrinsic kT, size, and max
        MSTP(91)=1
        PARP(91)=1D0
        PARP(93)=5D0
C...Use 2 GeV of primordial kT for "Perugia" version
        IF (ITUNE.EQ.221) THEN
          PARP(91)=2D0
          PARP(93)=10D0
        ENDIF
C...Use pT-ordered FSR
        MSTJ(41)=12
C...Lambda_FSR scale for pT-ordering
        PARJ(81)=0.23D0
C...Retune pT0 (changed from 2.1 to 2.05 in 6.4.20)
        PARP(82)=2.05D0
C...Fragmentation functions and c and b parameters
C...(overwritten for 211, i.e., if using Professor pars)
        MSTJ(11)=4
        PARJ(54)=-0.05
        PARJ(55)=-0.005
 
C...Use Professor's LEP pars if ITUNE == 211, 221, 226
        IF (ITUNE.EQ.211.OR.ITUNE.GE.221) THEN
C...# Tuned flavour parameters:
          PARJ(1)  = 0.073
          PARJ(2)  = 0.2
          PARJ(3)  = 0.94
          PARJ(4)  = 0.032
          PARJ(11) = 0.31
          PARJ(12) = 0.4
          PARJ(13) = 0.54
          PARJ(25) = 0.63
          PARJ(26) = 0.12
C...# Always use pT-ordered shower:
          MSTJ(41) = 12
C...# Switch on Bowler:
          MSTJ(11) = 5
C...# Fragmentation
          PARJ(21) = 3.1327e-01
          PARJ(41) = 4.8989e-01
          PARJ(42) = 1.2018e+00
          PARJ(47) = 1.0000e+00
          PARJ(81) = 2.5696e-01
          PARJ(82) = 8.0000e-01
        ENDIF
 
C...221, 226 : Perugia-APT and Perugia-APT6
        IF (ITUNE.EQ.221.OR.ITUNE.EQ.226) THEN
 
          PARP(64)=0.5D0
          PARP(82)=2.05D0
          PARP(90)=0.26D0
          PARP(91)=2.0D0
C...The Perugia variants use Steve's showers off the old MPI
          MSTP(152)=1
C...And use a lower PARP(71) as suggested by Professor tunings
C...(although not certain that applies to Q2-pT2 hybrid)
          PARP(71)=2.5D0
 
C...Perugia-APT6 uses CTEQ6L1 and a slightly lower pT0
          IF (ITUNE.EQ.226) THEN
            CH70='NB! This tune requires CTEQ6L1 pdfs to be '//
     &           'externally linked'
            WRITE(M11,5035) CH70
            MSTP(52)=2
            MSTP(51)=10042
            PARP(82)=1.95D0
          ENDIF
 
        ENDIF
 
C...  Output
        IF (M13.GE.1) THEN
          WRITE(M11,5030) ' '
          WRITE(M11,5040) 51, MSTP(51), CHMSTP(51)
          WRITE(M11,5040) 52, MSTP(52), CHMSTP(52)
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5050) 64, PARP(64), CHPARP(64)
          WRITE(M11,5050) 67, PARP(67), CHPARP(67)
          WRITE(M11,5040) 68, MSTP(68), CHMSTP(68)
          CH60='(Note: MSTP(68) is not explicitly (re-)set by PYTUNE)'
          WRITE(M11,5030) CH60
          WRITE(M11,5070) 41, MSTJ(41), CHMSTJ(41)
          WRITE(M11,5050) 71, PARP(71), CHPARP(71)
          WRITE(M11,5060) 81, PARJ(81), CHPARJ(81)
          WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
          WRITE(M11,5050) 82, PARP(82), CHPARP(82)
          WRITE(M11,5050) 89, PARP(89), CHPARP(89)
          WRITE(M11,5050) 90, PARP(90), CHPARP(90)
          WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          WRITE(M11,5050) 83, PARP(83), CHPARP(83)
          WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          WRITE(M11,5050) 85, PARP(85), CHPARP(85)
          WRITE(M11,5050) 86, PARP(86), CHPARP(86)
          WRITE(M11,5040) 91, MSTP(91), CHMSTP(91)
          WRITE(M11,5050) 91, PARP(91), CHPARP(91)
          WRITE(M11,5050) 93, PARP(93), CHPARP(93)
          WRITE(M11,5070) 11, MSTJ(11), CHMSTJ(11)
          WRITE(M11,5060) 21, PARJ(21), CHPARJ(21)
          WRITE(M11,5060) 41, PARJ(41), CHPARJ(41)
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          IF (MSTJ(11).LE.3) THEN
             WRITE(M11,5060) 54, PARJ(54), CHPARJ(54)
             WRITE(M11,5060) 55, PARJ(55), CHPARJ(55)
          ELSE
             WRITE(M11,5060) 46, PARJ(46), CHPARJ(46)
          ENDIF
          IF (MSTJ(11).EQ.5) WRITE(M11,5060) 47, PARJ(47), CHPARJ(47)
        ENDIF
 
C======================================================================
C...Uppsala models: Generalized Area Law and Soft Colour Interactions
      ELSEIF(CHNAME.EQ.'GAL Tune 0'.OR.CHNAME.EQ.'GAL Tune 1') THEN
        IF (M13.GE.1) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see J. Rathsman, PLB452(1999)364'
          WRITE(M11,5030) CH60
C ?         CH60='A. Edin, G. Ingelman, J. Rathsman, hep-ph/9912539,'
C ?         WRITE(M11,5030)
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          WRITE(M11,5030) ' '
          CH70='NB! The GAL model must be run with modified '//
     &        'Pythia v6.215:'
          WRITE(M11,5035) CH70
          CH70='available from http://www.isv.uu.se/thep/MC/scigal/'
          WRITE(M11,5035) CH70
          WRITE(M11,5030) ' '
        ENDIF
C...GAL Recommended settings from Uppsala web page (as per 22/08 2006)
        MSWI(2) = 3
        PARSCI(2) = 0.10
        MSWI(1) = 2
        PARSCI(1) = 0.44
        MSTJ(16) = 0
        PARJ(42) = 0.45
        PARJ(82) = 2.0
        PARP(62) = 2.0	
        MSTP(81) = 1
        MSTP(82) = 1
        PARP(81) = 1.9
        MSTP(92) = 1
        IF(CHNAME.EQ.'GAL Tune 1') THEN
C...GAL retune (P. Skands) to get better min-bias <Nch> at Tevatron
          MSTP(82)=4
          PARP(83)=0.25D0
          PARP(84)=0.5D0
          PARP(82) = 1.75
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 82, PARP(82), CHPARP(82)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
            WRITE(M11,5050) 83, PARP(83), CHPARP(83)
            WRITE(M11,5050) 84, PARP(84), CHPARP(84)
          ENDIF
        ELSE
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 81, PARP(81), CHPARP(81)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          ENDIF
        ENDIF
C...Output
        IF (M13.GE.1) THEN
          WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          WRITE(M11,5060) 82, PARJ(82), CHPARJ(82)
          WRITE(M11,5040) 92, MSTP(92), CHMSTP(92)
          CH40='FSI SCI/GAL selection'
          WRITE(M11,6040) 1, MSWI(1), CH40
          CH40='FSI SCI/GAL sea quark treatment'
          WRITE(M11,6040) 2, MSWI(2), CH40
          CH40='FSI SCI/GAL sea quark treatment parm'
          WRITE(M11,6050) 1, PARSCI(1), CH40
          CH40='FSI SCI/GAL string reco probability R_0'
          WRITE(M11,6050) 2, PARSCI(2), CH40
          WRITE(M11,5060) 42, PARJ(42), CHPARJ(42)
          WRITE(M11,5070) 16, MSTJ(16), CHMSTJ(16)
        ENDIF
      ELSEIF(CHNAME.EQ.'SCI Tune 0'.OR.CHNAME.EQ.'SCI Tune 1') THEN
        IF (M13.GE.1) THEN
          WRITE(M11,5010) ITUNE, CHNAME
          CH60='see A.Edin et al, PLB366(1996)371, Z.Phys.C75(1997)57,'
          WRITE(M11,5030) CH60
          CH60='and T. Sjostrand & M. v. Zijl, PRD36(1987)2019'
          WRITE(M11,5030) CH60
          WRITE(M11,5030) ' '
          CH70='NB! The SCI model must be run with modified '//
     &        'Pythia v6.215:'
          WRITE(M11,5035) CH70
          CH70='available from http://www.isv.uu.se/thep/MC/scigal/'
          WRITE(M11,5035) CH70
          WRITE(M11,5030) ' '
        ENDIF
C...SCI Recommended settings from Uppsala web page (as per 22/08 2006)
        MSTP(81)=1
        MSTP(82)=1
        PARP(81)=2.2
        MSTP(92)=1
        MSWI(2)=2
        PARSCI(2)=0.50
        MSWI(1)=2
        PARSCI(1)=0.44
        MSTJ(16)=0
        IF (CHNAME.EQ.'SCI Tune 1') THEN
C...SCI retune (P. Skands) to get better min-bias <Nch> at Tevatron
          MSTP(81) = 1
          MSTP(82) = 3
          PARP(82) = 2.4
          PARP(83) = 0.5D0
          PARP(62) = 1.5
          PARP(84)=0.25D0
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 82, PARP(82), CHPARP(82)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
            WRITE(M11,5050) 83, PARP(83), CHPARP(83)
            WRITE(M11,5050) 62, PARP(62), CHPARP(62)
          ENDIF
        ELSE
          IF (M13.GE.1) THEN
            WRITE(M11,5040) 81, MSTP(81), CHMSTP(81)
            WRITE(M11,5050) 81, PARP(81), CHPARP(81)
            WRITE(M11,5040) 82, MSTP(82), CHMSTP(82)
          ENDIF
        ENDIF
C...Output
        IF (M13.GE.1) THEN
          WRITE(M11,5040) 92, MSTP(92), CHMSTP(92)
          CH40='FSI SCI/GAL selection'
          WRITE(M11,6040) 1, MSWI(1), CH40
          CH40='FSI SCI/GAL sea quark treatment'
          WRITE(M11,6040) 2, MSWI(2), CH40
          CH40='FSI SCI/GAL sea quark treatment parm'
          WRITE(M11,6050) 1, PARSCI(1), CH40
          CH40='FSI SCI/GAL string reco probability R_0'
          WRITE(M11,6050) 2, PARSCI(2), CH40
          WRITE(M11,5070) 16, MSTJ(16), CHMSTJ(16)
        ENDIF
 
      ELSE
        IF (MSTU(13).GE.1) WRITE(M11,5020) ITUNE
 
      ENDIF
 
  100 IF (MSTU(13).GE.1) WRITE(M11,6000)
 
 9999 RETURN
 
 5000 FORMAT(1x,78('*')/' *',76x,'*'/' *',3x,'PYTUNE v',A6,' : ',
     &    'Presets for underlying-event (and min-bias)',13x,'*'/' *',
     &    20x,'Last Change : ',A8,' - P. Skands',22x,'*'/' *',76x,'*')
 5010 FORMAT(' *',3x,I4,1x,A16,52x,'*')
 5020 FORMAT(' *',3x,'Tune ',I4, ' not recognized. Using defaults.')
 5030 FORMAT(' *',3x,10x,A60,3x,'*')
 5035 FORMAT(' *',3x,A70,3x,'*')
 5040 FORMAT(' *',5x,'MSTP(',I2,') = ',I12,3x,A42,3x,'*')
 5050 FORMAT(' *',5x,'PARP(',I2,') = ',F12.4,3x,A40,5x,'*')
 5060 FORMAT(' *',5x,'PARJ(',I2,') = ',F12.4,3x,A40,5x,'*')
 5070 FORMAT(' *',5x,'MSTJ(',I2,') = ',I12,3x,A40,5x,'*')
 5140 FORMAT(' *',5x,'MSTP(',I3,')= ',I12,3x,A40,5x,'*')
 5150 FORMAT(' *',5x,'PARP(',I3,')= ',F12.4,3x,A40,5x,'*')
 6000 FORMAT(' *',76x,'*'/1x,32('*'),1x,'END OF PYTUNE',1x,31('*'))
 6040 FORMAT(' *',5x,'MSWI(',I1,')  = ',I12,3x,A40,5x,'*')
 6050 FORMAT(' *',5x,'PARSCI(',I1,')= ',F12.4,3x,A40,5x,'*')
 
      END
