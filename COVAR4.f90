C     COVAR 4
C
C
C     22/01/16 - Rafael Vanhoz Ribeiro / Vanderlei Cardoso / Mauro S.Dias
C
C     CALCULA    COVARIANCIA ENTRE OS K0S E Q0S CALCULADOS;
C
      IMPLICIT REAL*4 (A-H,O-Z)
      DIMENSION ID1(30),ERG1(30),N1(30),FZ1(30),FA1(30),D1(30),
     +C1(30),S1(30),W1(30),GTHI(30),
     +EFI(30),FCDI(30),ID2(30), ERG2(30), N2(30),FZ2(30),FA2(30),
     +D2(30),C2(30),S2(30),W2(30),ID3(30),ERG3(30),N3(30),Fii(30),
     +FZ3(30),FA3(30),D3(30),C3(30),S3(30),
     +W3(30),GTHC(30),ID4(30), ERG4(30),N4(30),FZ4(30),D4(30),
     +C4(30),S4(30),W4(30),Q0C(30),ALFA(30),
     +EFC(30),FCDC(30),FA4(30),GEI(30),Q0ii(30),
     +GEC(30),COVAR(30,30,4,4),Q0Ia(30),Q0I(30),
     +ERI(30),Q02(30),CORREL(30,30,4,4)
      DIMENSION RO(4,4,50),SIG(30,4,50),DEL(30,8,50),ERC(30),K0(30),
     +ROB(4,4,50), ROC(4,4,50)
      DIMENSION SN1(30),SFZ1(30),SFA1(30),SD1(30),K02(30),K03(30),
     +SC1(30),SS1(30),SW1(30),SGTHI(30),
     +SEFI(30),SFCDI(30),SQ0C(30),SERC(30),
     +SALFA(30),SGEI(30),SERI(30),SQ0ii(30),SFii(30)
      DIMENSION SN2(30),SFZ2(30),SFA2(30),SD2(30),
     +SC2(30),SS2(30),SW2(30)
      DIMENSION SN3(30),SFZ3(30),SFA3(30),SD3(30),
     +SC3(30),SS3(30),SW3(30),SGTHC(30),
     +SEFC(30),SFCDC(30),SGEC(30)
      DIMENSION SN4(30),SFZ4(30),SFA4(30),SD4(30),
     +SC4(30),SS4(30),SW4(30);

      REAL*4 N1,N2,N3,N4,K0,K02,K03
      CHARACTER*70 INP1,INP2,INP3,INP4,INP5,OUT,AAA
      CHARACTER*4 X(3)
      DATA INP1/'C:\Fortran\Input\COVAR14.DAT'/
      DATA INP2/'C:\Fortran\Input\COVAR24.DAT'/
      DATA INP3/'C:\Fortran\Input\COVAR34.DAT'/
      DATA INP4/'C:\Fortran\Input\COVAR24B.DAT'/
      DATA INP5/'C:\Fortran\Input\COVAR24C.DAT'/
      DATA OUT/'C:\Fortran\COVAR4.OUT'/


C
C     ENTRADA DE DADOS
      WRITE(*,*) 'LEITURA DOS PARAMETROS INICIAIS'
      OPEN (1,FILE=INP1)
      OPEN (2,FILE=INP2)
      OPEN (3,FILE=INP3)
      OPEN (4,FILE=INP4)
      OPEN (5,FILE=INP5)
      OPEN (6,FILE=OUT)

      READ (1,*) M1,M2,LL,NN

C     WRITE(*,*) 'M1 = ', M1,'  M2 = ', M2
C     WRITE(*,*) 'L = ', L,'  LL = ', LL


C

      DO 300 K=1,M2
C     WRITE(*,*) 'K = ', K
        READ(2,1000) AAA
        READ(4,1000) AAA
        READ(5,1000) AAA
C     WRITE(*,*) AAA
1000  FORMAT(A70)
      DO 400 I=1,LL
      READ(2,*) (RO(I,J,K),J=1,LL)
      READ(4,*) (ROB(I,J,K),J=1,LL)
      READ(5,*) (ROC(I,J,K),J=1,LL)
c      WRITE(6,*)'I',I,'  , K',K,'  RO(I,J,K)',(RO(I,J,K),J=1,LL)
c      WRITE(*,*)'I',I,'  , K',K,'  RO(I,J,K)',(RO(I,J,K),J=1,LL)


400   CONTINUE
 300   CONTINUE


      DO 3000 N = 1, NN

      READ(1,*)ID1(N),ERG1(N),N1(N),FZ1(N),FA1(N),D1(N),
     +C1(N),S1(N),W1(N),GTHI(N),
     +EFI(N),FCDI(N),Q0C(N),ERC(N),ALFA(N),GEI(N),ERI(N),FII(N),Q0II(N)
      READ(1,*)ID2(N),ERG2(N), N2(N),FZ2(N),FA2(N),D2(N),
     +C2(N),S2(N),W2(N),GTHI(N),
     +EFI(N),FCDI(N),Q0C(N),ERC(N),ALFA(N),GEI(N),ERI(N),FII(N),Q0II(N)
      READ(1,*)ID3(N), ERG3(N), N3(N),FZ3(N),FA3(N),D3(N),
     +C3(N),S3(N),W3(N),GTHC(N),
     +EFC(N),FCDC(N),Q0C(N),ERC(N),ALFA(N),GEC(N),ERI(N),FII(N),Q0II(N)
      READ(1,*) ID4(N),ERG4(N),N4(N),FZ4(N),FA4(N),D4(N),
     +C4(N),S4(N),W4(N),GTHC(N),
     +EFC(N),FCDC(N),Q0C(N),ERC(N),ALFA(N),GEC(N),ERI(N),FII(N),Q0II(N)

c      WRITE(6,*) N1(N),FZ1(N),FA1(N),D1(N),C1(N),S1(N),W1(N),GTHI(N),
c     +EFI(N),FCDI(N),Q0C(N),ERC(N),ALFA(N),GEI(N),ERI(N)
c      WRITE(6,*) N2(N),FZ2(N),FA2(N),D2(N),C2(N),S2(N),W2(N),GTHI(N),
c     +EFI(N),FCDI(N),Q0C(N),ERC(N),ALFA(N),GEI(N),ERI(N)
c      WRITE(6,*) N3(N),FZ3(N),FA3(N),D3(N),C3(N),S3(N),W3(N),GTHC(N),
c     +EFC(N),FCDC(N),Q0C(N),ERC(N),ALFA(N),GEC(N),ERI(N)
c      WRITE(6,*) N4(N),FZ4(N),FA4(N),D4(N),C4(N),S4(N),W4(N),GTHC(N),
c     +EFC(N),FCDC(N),Q0C(N),ERC(N),ALFA(N),GEC(N),ERI(N)


      READ(3,*) SN1(N),SFZ1(N),SFA1(N),SD1(N),
     +SC1(N),SS1(N),SW1(N),SGTHI(N),
     +SEFI(N),SFCDI(N),SQ0C(N),SERC(N),
     +SALFA(N),SGEI(N),SERI(N),SFII(N),SQ0II(N)
      READ(3,*) SN2(N),SFZ2(N),SFA2(N),SD2(N),
     +SC2(N),SS2(N),SW2(N),SGTHI(N),
     +SEFI(N),SFCDI(N),SQ0C(N),SERC(N),
     +SALFA(N),SGEI(N),SERI(N),SFII(N),SQ0II(N)
      READ(3,*) SN3(N),SFZ3(N),SFA3(N),SD3(N),
     +SC3(N),SS3(N),SW3(N),SGTHC(N),
     +SEFC(N),SFCDC(N),SQ0C(N),SERC(N),
     +SALFA(N),SGEC(N),SERI(N),SFII(N),SQ0II(N)
      READ(3,*) SN4(N),SFZ4(N),SFA4(N),SD4(N),
     +SC4(N),SS4(N),SW4(N),SGTHC(N),
     +SEFC(N),SFCDC(N),SQ0C(N),SERC(N),
     +SALFA(N),SGEC(N),SERI(N),SFII(N),SQ0II(N)

c      WRITE(*,*) sfii(N),sq0ii(N)

C      WRITE(6,*) SN1(N),SFZ1(N),SFA1(N),SD1(N),
C     +SC1(N),SS1(N),SW1(N),SGTHI(N),
C     +SEFI(N),SFCDI(N),SQ0C(N),SERC(N),
C     +SALFA(N),SGEI(N),SERI(N)
C      WRITE(6,*) SN2(N),SFZ2(N),SFA2(N),SD2(N),
C     +SC2(N),SS2(N),SW2(N),SGTHI(N),
C   +SEFI(N),SFCDI(N),SQ0C(N),SERC(N),
C     +SALFA(N),SGEI(N),SERI(N)
C      WRITE(6,*) SN3(N),SFZ3(N),SFA3(N),SD3(N),
C     +SC3(N),SS3(N),SW3(N),SGTHC(N),
C   +SEFC(N),SFCDC(N),SQ0C(N),SERC(N),
C     +SALFA(N),SGEC(N),SERI(N)
C      WRITE(6,*) SN4(N),SFZ4(N),SFA4(N),SD4(N),
C     +SC4(N),SS4(N),SW4(N),SGTHC(N),
C   +SEFC(N),SFCDC(N),SQ0C(N),SERC(N),
C     +SALFA(N),SGEC(N),SERI(N)

      SIG(N,1,1) = SN1(N)
      SIG(N,1,2) = SFZ1(N)
      SIG(N,1,3) = SFA1(N)
      SIG(N,1,4) = SD1(N)
      SIG(N,1,5) = SC1(N)
      SIG(N,1,6) = SS1(N)
      SIG(N,1,7) = SW1(N)
      SIG(N,1,8) = SN2(N)
      SIG(N,1,9) = SFZ2(N)
      SIG(N,1,10) = SFA2(N)
      SIG(N,1,11) = SD2(N)
      SIG(N,1,12) = SC2(N)
      SIG(N,1,13) = SS2(N)
      SIG(N,1,14) = SW2(N)
      SIG(N,1,15) = SFCDI(N)
      SIG(N,1,16) = SN3(N)
      SIG(N,1,17) = SFZ3(N)
      SIG(N,1,18) = SFA3(N)
      SIG(N,1,19) = SD3(N)
      SIG(N,1,20) = SC3(N)
      SIG(N,1,21) = SS3(N)
      SIG(N,1,22) = SW3(N)
      SIG(N,1,23) = SN4(N)
      SIG(N,1,24) = SFZ4(N)
      SIG(N,1,25) = SFA4(N)
      SIG(N,1,26) = SD4(N)
      SIG(N,1,27) = SC4(N)
      SIG(N,1,28) = SS4(N)
      SIG(N,1,29) = SW4(N)
      SIG(N,1,30) = SFCDC(N)
      SIG(N,1,31) = SGTHC(N)
      SIG(N,1,32) = SGTHI(N)
      SIG(N,1,33) = SEFC(N)
      SIG(N,1,34) = SEFI(N)
C      WRITE(*,*) sefi(N)

      SIG(N,2,1) = SN1(N)
      SIG(N,2,2) = SFZ1(N)
      SIG(N,2,3) = SFA1(N)
      SIG(N,2,4) = SD1(N)
      SIG(N,2,5) = SC1(N)
      SIG(N,2,6) = SS1(N)
      SIG(N,2,7) = SW1(N)
      SIG(N,2,8) = SN2(N)
      SIG(N,2,9) = SFZ2(N)
      SIG(N,2,10) = SFA2(N)
      SIG(N,2,11) = SD2(N)
      SIG(N,2,12) = SC2(N)
      SIG(N,2,13) = SS2(N)
      SIG(N,2,14) = SW2(N)
      SIG(N,2,15) = SFCDI(N)
      SIG(N,2,16) = SN3(N)
      SIG(N,2,17) = SFZ3(N)
      SIG(N,2,18) = SFA3(N)
      SIG(N,2,19) = SD3(N)
      SIG(N,2,20) = SC3(N)
      SIG(N,2,21) = SS3(N)
      SIG(N,2,22) = SW3(N)
      SIG(N,2,23) = SN4(N)
      SIG(N,2,24) = SFZ4(N)
      SIG(N,2,25) = SFA4(N)
      SIG(N,2,26) = SD4(N)
      SIG(N,2,27) = SC4(N)
      SIG(N,2,28) = SS4(N)
      SIG(N,2,29) = SW4(N)
      SIG(N,2,30) = SFCDC(N)
      SIG(N,2,31) = SGTHC(N)
      SIG(N,2,32) = SGTHI(N)
      SIG(N,2,33) = SQ0C(N)
      SIG(N,2,34) = SERC(N)
      SIG(N,2,35) = SALFA(N)
      SIG(N,2,36) = SGEI(N)
      SIG(N,2,37) = SGEC(N)
      SIG(N,2,38) = SERI(N)


      SIG(N,3,1) = SN1(N)
      SIG(N,3,2) = SFZ1(N)
      SIG(N,3,3) = SFA1(N)
      SIG(N,3,4) = SD1(N)
      SIG(N,3,5) = SC1(N)
      SIG(N,3,6) = SS1(N)
      SIG(N,3,7) = SW1(N)
      SIG(N,3,8) = SN3(N)
      SIG(N,3,9) = SFZ3(N)
      SIG(N,3,10) = SFA3(N)
      SIG(N,3,11) = SD3(N)
      SIG(N,3,12) = SC3(N)
      SIG(N,3,13) = SS3(N)
      SIG(N,3,14) = SW3(N)
      SIG(N,3,15) = SQ0c(N)
      SIG(N,3,16) = SGEC(N)
      SIG(N,3,17) = SGEI(N)
      SIG(N,3,18) = SGTHC(N)
      SIG(N,3,19) = SGTHI(N)
      SIG(N,3,20) = SEFC(N)
      SIG(N,3,21) = SEFI(N)
      SIG(N,3,22) = SFII(N)
      SIG(N,3,23) = SQ0II(N)


c      write(*,*) SIG(N,3,22)
C     CALCULO DAS DERIVADAS
C
C     DERIVADAS EM RELACAO AOS PARAMETROS DO K0

C     PRIMEIRO CONJUNTO
C       N1
      DEL(N,1,1)= (Efc(N)*fa1(N)*fz1(N)*
     +Gthc(N))/(Efi(N)*Gthi(N)*w1(N)
     +*C1(N)*D1(N)*S1(N)*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*N4(N))/(Fcdc(N)*
     +w4(N)*C4(N)*D4(N)*S4(N))))
C       FZ1
      DEL(N,1,2)= (Efc(N)*fa1(N)*Gthc(N)*
     +N1(N))/(Efi(N)*Gthi(N)*w1(N)
     +*C1(N)*D1(N)*S1(N)*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*N4(N))/(Fcdc(N)*
     +w4(N)*C4(N)*D4(N)*S4(N))))
C       FA1
      DEL(N,1,3)=(Efc(N)*fz1(N)*Gthc(N)*
     +N1(N))/(Efi(N)*Gthi(N)*w1(N)*C1(N)
     +*D1(N)*S1(N)*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*N4(N))/(Fcdc(N)*
     +w4(N)*C4(N)*D4(N)*S4(N))))
C      D1
      DEL(N,1,4)= -(Efc(N)*fa1(N)*fz1(N)*
     +Gthc(N)*N1(N))/(Efi(N)*Gthi(N)
     +*w1(N)*C1(N)*D1(N)**2*S1(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C       C1
      DEL(N,1,5)= -(Efc(N)*fa1(N)*fz1(N)*
     +Gthc(N)*N1(N))/(Efi(N)*Gthi(N)
     +*w1(N)*C1(N)**2*D1(N)*S1(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C       S1
      DEL(N,1,6)=-(Efc(N)*fa1(N)*fz1(N)*
     +Gthc(N)*N1(N))/(Efi(N)*Gthi(N)
     +*w1(N)*C1(N)*D1(N)*S1(N)**2*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C       W1
      DEL(N,1,7)=-(Efc(N)*fa1(N)*fz1(N)*
     +Gthc(N)*N1(N))/(Efi(N)*Gthi(N)
     +*w1(N)**2*C1(N)*D1(N)*S1(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C     SEGUNDO CONJUNTO
C       N2
      DEL(N,1,8)=-(Efc(N)*fa2(N)*fz2(N)*
     +Gthc(N))/(Efi(N)*Fcdi(N)*Gthi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N)*((fa3(N)*
     +fz3(N)*N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N))))
C       FZ2
      DEL(N,1,9)=-(Efc(N)*fa2(N)*Gthc(N)*
     +N2(N))/(Efi(N)*Fcdi(N)*Gthi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N))))
C       FA2
      DEL(N,1,10)=-(Efc(N)*fz2(N)*Gthc(N)*
     +N2(N))/(Efi(N)*Fcdi(N)*Gthi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)*D3(N)
     +*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N))))
C       D2
      DEL(N,1,11)=(Efc(N)*fa2(N)*fz2(N)*
     +Gthc(N)*N2(N))/(Efi(N)*Fcdi(N)
     +*Gthi(N)*w2(N)*C2(N)*D2(N)**2*
     +S2(N)*((fa3(N)*fz3(N)*N3(N))
     +/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)*
     +fz4(N)*N4(N))/(Fcdc(N)*w4(N)
     +*C4(N)*D4(N)*S4(N))))
C       C2
      DEL(N,1,12)=(Efc(N)*fa2(N)*fz2(N)*
     +Gthc(N)*N2(N))/(Efi(N)*Fcdi(N)
     +*Gthi(N)*w2(N)*C2(N)**2*D2(N)*
     +S2(N)*((fa3(N)*fz3(N)*N3(N))
     +/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)*
     +fz4(N)*N4(N))/(Fcdc(N)
     +*w4(N)*C4(N)*D4(N)*S4(N))))
C       S2
      DEL(N,1,13)=(Efc(N)*fa2(N)*
     +fz2(N)*Gthc(N)*N2(N))/(Efi(N)*Fcdi(N)
     +*Gthi(N)*w2(N)*C2(N)*D2(N)*
     +S2(N)**2*((fa3(N)*fz3(N)*N3(N))
     +/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)*
     +fz4(N)*N4(N))/(Fcdc(N)*w4(N)
     +*C4(N)*D4(N)*S4(N))))
C       W2
      DEL(N,1,14)=(Efc(N)*fa2(N)*fz2(N)*
     +Gthc(N)*N2(N))/(Efi(N)*Fcdi(N)
     +*Gthi(N)*w2(N)**2*C2(N)*D2(N)*
     +S2(N)*((fa3(N)*fz3(N)*N3(N))
     +/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)*
     +fz4(N)*N4(N))/(Fcdc(N)*w4(N)
     +*C4(N)*D4(N)*S4(N))))
C       FCDI
      DEL(N,1,15)=(Efc(N)*fa2(N)*fz2(N)*
     +Gthc(N)*N2(N))/(Efi(N)*Fcdi(N)**2
     +*Gthi(N)*w2(N)*C2(N)*D2(N)*S2(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)
     +*C3(N)*D3(N)*S3(N))-(fa4(N)*
     +fz4(N)*N4(N))/(Fcdc(N)*w4(N)*C4(N)
     +*D4(N)*S4(N))))
C     TERCEIRO CONJUNTO
C       N3
      DEL(N,1,16)=-(Efc(N)*fa3(N)*fz3(N)*
     +Gthc(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*Gthi(N)*
     +w3(N)*C3(N)*D3(N)*S3(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N)))**2)
C       FZ3
      DEL(N,1,17)=-(Efc(N)*fa3(N)*Gthc(N)*
     +N3(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*Gthi(N)*
     +w3(N)*C3(N)*D3(N)*S3(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)*
     +D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N)))**2)
C       FA3
      DEL(N,1,18)=-(Efc(N)*fz3(N)*Gthc(N)*
     +N3(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Gthi(N)*w3(N)*C3(N)*D3(N)*S3(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*S4(N)))**2)
C       D3
      DEL(N,1,19)=(Efc(N)*fa3(N)*fz3(N)*
     +Gthc(N)*N3(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Gthi(N)*w3(N)*C3(N)*D3(N)**2
     +*S3(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2)
C       C3
      DEL(N,1,20)=(Efc(N)*fa3(N)*fz3(N)*
     +Gthc(N)*N3(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Gthi(N)*w3(N)*C3(N)**2*D3(N)
     +*S3(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2)
C       S3
      DEL(N,1,21)=(Efc(N)*fa3(N)*fz3(N)*
     +Gthc(N)*N3(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Gthi(N)*w3(N)*C3(N)*D3(N)
     +*S3(N)**2*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*
     +w4(N)*C4(N)*D4(N)*S4(N)))**2)
C       W3
      DEL(N,1,22)=(Efc(N)*fa3(N)*fz3(N)*
     +Gthc(N)*N3(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Gthi(N)*w3(N)**2*C3(N)*D3(N)
     +*S3(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2)
C      QUARTO CONJUNTO
C       N4
      DEL(N,1,23)=(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*Fcdc(N)*
     +Gthi(N)*w4(N)*C4(N)*D4(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)*
     +D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)*
     +S4(N)))**2*S4(N))
C       FZ4
      DEL(N,1,24)=(Efc(N)*fa4(N)*Gthc(N)*
     +N4(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)*C4(N)*D4(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*
     +D4(N)*S4(N)))**2*S4(N))
C       FA4
      DEL(N,1,25)=(Efc(N)*fz4(N)*Gthc(N)*
     +N4(N)*((fa1(N)*fz1(N)*N1(N))
     +/(w1(N)*C1(N)*D1(N)*S1(N))-(fa2(N)*
     +fz2(N)*N2(N))/(Fcdi(N)*w2(N)
     +*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)*C4(N)*D4(N)
     +*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)*fz4(N)
     +*N4(N))/(Fcdc(N)*w4(N)*C4(N)*
     +D4(N)*S4(N)))**2*S4(N))
C       D4
      DEL(N,1,26)=-(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*N4(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)*C4(N)
     +*D4(N)**2*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2*S4(N))
C       C4
      DEL(N,1,27)=-(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*N4(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)*C4(N)**2
     +*D4(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2*S4(N))
C       S4
      DEL(N,1,28)=-(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*N4(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)*C4(N)
     +*D4(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*C4(N)*
     +D4(N)*S4(N)))**2*S4(N)**2)
C       W4
      DEL(N,1,29)=-(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*N4(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)*Gthi(N)*w4(N)**2*C4(N)
     +*D4(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2*S4(N))
C       FCDC
      DEL(N,1,30)=-(Efc(N)*fa4(N)*fz4(N)*
     +Gthc(N)*N4(N)*((fa1(N)*fz1(N)
     +*N1(N))/(w1(N)*C1(N)*D1(N)*
     +S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)
     +*w2(N)*C2(N)*D2(N)*S2(N))))/(Efi(N)*
     +Fcdc(N)**2*Gthi(N)*w4(N)*C4(N)
     +*D4(N)*((fa3(N)*fz3(N)*N3(N))/(w3(N)*
     +C3(N)*D3(N)*S3(N))-(fa4(N)
     +*fz4(N)*N4(N))/(Fcdc(N)*w4(N)*
     +C4(N)*D4(N)*S4(N)))**2*S4(N))
C      QUINTO    CONJUNTO
C       GTHC
      DEL(N,1,31)=(Efc(N)*((fa1(N)*fz1(N)*
     +N1(N))/(w1(N)*C1(N)*D1(N)
     +*S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)*
     +w2(N)*C2(N)*D2(N)
     +*S2(N))))/(Efi(N)*Gthi(N)*((fa3(N)*
     +fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C       GTHI
      DEL(N,1,32)=-(Efc(N)*Gthc(N)*((fa1(N)*
     +fz1(N)*N1(N))/(w1(N)*C1(N)
     +*D1(N)*S1(N))-(fa2(N)*fz2(N)*
     +N2(N))/(Fcdi(N)*w2(N)*C2(N)*D2(N)
     +*S2(N))))/(Efi(N)*Gthi(N)**2*((fa3(N)*
     +fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C       EFC
      DEL(N,1,33)=(Gthc(N)*((fa1(N)*fz1(N)*
     +N1(N))/(w1(N)*C1(N)*D1(N)
     +*S1(N))-(fa2(N)*fz2(N)*N2(N))/(Fcdi(N)*
     +w2(N)*C2(N)*D2(N)*S2(N))))
     +/(Efi(N)*Gthi(N)*((fa3(N)*fz3(N)*
     +N3(N))/(w3(N)*C3(N)*D3(N)*S3(N))
     +-(fa4(N)*fz4(N)*N4(N))/(Fcdc(N)*
     +w4(N)*C4(N)*D4(N)*S4(N))))
C       EFI
      DEL(N,1,34)=-(Efc(N)*Gthc(N)*((fa1(N)*
     +fz1(N)*N1(N))/(w1(N)*C1(N)
     +*D1(N)*S1(N))-(fa2(N)*fz2(N)*
     +N2(N))/(Fcdi(N)*w2(N)*C2(N)*D2(N)
     +*S2(N))))/(Efi(N)**2*Gthi(N)*
     +((fa3(N)*fz3(N)*N3(N))/(w3(N)*C3(N)
     +*D3(N)*S3(N))-(fa4(N)*fz4(N)*
     +N4(N))/(Fcdc(N)*w4(N)*C4(N)*D4(N)
     +*S4(N))))
C
C       FIM DO CALCULO DAS DERIVADAS DO K0
C
C
C
C      DERIVADAS EM RELACAO AOS PARAMETROS DO Q0
C      PRIMEIRO CONJUNTO
C
C
C      N1
      DEL(N,2,1)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      fz1
      DEL(N,2,2)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*GEC(N)*GTHI(N)*N1(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +GEI(N)*GTHC(N)*N2(N)*S1(N)*W1(N)
     +*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      fa1
      DEL(N,2,3)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)*N1(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      D1
      DEL(N,2,4)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*
     +D4(N)*FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)**2*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      C1
      DEL(N,2,5)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))
     +/(C1(N)**2*D1(N)*FA2(N)*FZ2(N)*
     +GEI(N)*GTHC(N)*N2(N)*S1(N)*W1(N)
     +*((C2(N)*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C     S1
      DEL(N,2,6)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)**2*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      W1
      DEL(N,2,7)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*
     +D4(N)*FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)**2*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      SEGUNDO CONJUNTO
C      N2
      DEL(N,2,8)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*GTHC(N)*
     +N2(N)**2*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*N1(N)*
     +S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      FZ2
      DEL(N,2,9)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)**2*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      FA2
      DEL(N,2,10)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)**2*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*N1(N)*
     +S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      D2
      DEL(N,2,11)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)*N1(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      C2
      DEL(N,2,12)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)*N1(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      S2
      DEL(N,2,13)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*W2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      W2
      DEL(N,2,14)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*GEC(N)*GTHI(N)
     +*N1(N)*S2(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C       FCDI
      DEL(N,2,15)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C2(N)*D2(N)*
     +FA1(N)*FZ1(N)*GEC(N)*GTHI(N)*N1(N)
     +*S2(N)*W2(N)*((C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*N3(N)*S4(N)
     +*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))/(C1(N)
     +*D1(N)*FA2(N)*FZ2(N)*GEI(N)*
     +GTHC(N)*N2(N)*S1(N)*W1(N)*((C2(N)
     +*D2(N)*FA1(N)*FCDI(N)*FZ1(N)*
     +N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)
     +*FA2(N)*FZ2(N)*N2(N)*S1(N)*W1(N))-1)**2)
C      TERCEIRO CONJUNTO
C    N3
      DEL(N,2,16)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      FZ3
      DEL(N,2,17)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*GEC(N)*GTHI(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      FA3
      DEL(N,2,18)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      D3
      DEL(N,2,19)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)**
     +2*FA4(N)*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      C3
      DEL(N,2,20)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)**2*D3(N)*
     +FA4(N)*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      S3
      DEL(N,2,21)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)**2*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      W3
      DEL(N,2,22)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N)**2)
C      QUARTO CONJUNTO
C       N4
      DEL(N,2,23)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)**2*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      FZ4
      DEL(N,2,24)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)**2*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C      FA4
      DEL(N,2,25)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)**2*FZ4(N)*GEI(N)*GTHC(N)
     +*N4(N)*S3(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1)*W3(N))
C       D4
      DEL(N,2,26)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*FA3(N)*FCDC(N)*
     +FZ3(N)*GEC(N)*GTHI(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      C4
      DEL(N,2,27)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*D4(N)*FA3(N)*FCDC(N)*
     +FZ3(N)*GEC(N)*GTHI(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      S4
      DEL(N,2,28)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      W4
      DEL(N,2,29)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*GEC(N)*GTHI(N)
     +*N3(N)*S4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      FCDC
      DEL(N,2,30)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*C4(N)*D4(N)*FA3(N)*
     +FZ3(N)*GEC(N)*GTHI(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*GEI(N)*GTHC(N)*N4(N)
     +*S3(N)*((C2(N)*D2(N)*FA1(N)*FCDI(N)*
     +FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*N2(N)*
     +S1(N)*W1(N))-1)*W3(N))
C      QUINTO    CONJUNTO
C      GTHC
      DEL(N,2,31)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*GEC(N)*GTHI(N)*
     +((C4(N)*D4(N)*FA3(N)*FCDC(N)
     +*FZ3(N)*N3(N)*S4(N)*W4(N))/(C3(N)*
     +D3(N)*FA4(N)*FZ4(N)*N4(N)*S3(N)
     +*W3(N))-1))/(GEI(N)*GTHC(N)**2*((C2(N)*
     +D2(N)*FA1(N)*FCDI(N)*FZ1(N)
     +*N1(N)*S2(N)*W2(N))/(C1(N)*D1(N)*
     +FA2(N)*FZ2(N)*N2(N)*S1(N)
     +*W1(N))-1))
C      GTHI
      DEL(N,2,32)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*GEC(N)*((C4(N)*D4(N)*
     +FA3(N)*FCDC(N)*FZ3(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))
     +/(GEI(N)*GTHC(N)*((C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1))
C      Q0C
      DEL(N,2,33)=(GEC(N)*GTHI(N)*((C4(N)*
     +D4(N)*FA3(N)*FCDC(N)*FZ3(N)
     +*N3(N)*S4(N)*W4(N))/(C3(N)*D3(N)*
     +FA4(N)*FZ4(N)*N4(N)*S3(N)
     +*W3(N))-1))/(Erc(N)**alfa(N)*GEI(N)*
     +GTHC(N)*((C2(N)*D2(N)*FA1(N)
     +*FCDI(N)*FZ1(N)*N1(N)*S2(N)*
     +W2(N))/(C1(N)*D1(N)*FA2(N)*FZ2(N)
     +*N2(N)*S1(N)*W1(N))-1))

C      ERC
      DEL(N,2,34)=-(alfa(N)*Erc(N)**
     +(-alfa(N)-1)*(Q0c(N)-0.429)*GEC(N)
     +*GTHI(N)*((C4(N)*D4(N)*FA3(N)*
     +FCDC(N)*FZ3(N)*N3(N)*S4(N)*W4(N))
     +/(C3(N)*D3(N)*FA4(N)*FZ4(N)*
     +N4(N)*S3(N)*W3(N))-1))/(GEI(N)
     +*GTHC(N)*((C2(N)*D2(N)*FA1(N)*
     +FCDI(N)*FZ1(N)*N1(N)*S2(N)*W2(N))
     +/(C1(N)*D1(N)*FA2(N)*FZ2(N)*
     +N2(N)*S1(N)*W1(N))-1))
C      ALFA
      DEL(N,2,35)=((-(lOG(Erc(N))*
     +(Q0c(N)-0.429))/Erc(N)**alfa(N)
     ++0.25647207332416/((2*alfa(N)+1)*0.55**alfa(N))-0.858
     +/((2*alfa(N)+1)**2*0.55**alfa(N)))*
     +GEC(N)*GTHI(N)*((C4(N)*D4(N)
     +*FA3(N)*FCDC(N)*FZ3(N)*N3(N)*
     +S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)
     +*FZ4(N)*N4(N)*S3(N)*
     +W3(N))-1))/(GEI(N)*GTHC(N)*((C2(N)*D2(N)
     +*FA1(N)*FCDI(N)*FZ1(N)*N1(N)*
     +S2(N)*W2(N))/(C1(N)*D1(N)*FA2(N)
     +*FZ2(N)*N2(N)*S1(N)*
     +W1(N))-1))+(0.429*LOG(Eri(N)))/((2*alfa(N)+1)
     +*0.55**alfa(N)*Eri(N)**
     +alfa(N))-0.25647207332416/((2*alfa(N)+1)
     +*0.55**alfa(N)*Eri(N)**
     +alfa(N))+0.858/((2*alfa(N)+1)**2
     +*0.55**alfa(N)*Eri(N)**alfa(N))
C      GEI
      DEL(N,2,36)=-(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*GEC(N)*GTHI(N)*
     +((C4(N)*D4(N)*FA3(N)*FCDC(N)
     +*FZ3(N)*N3(N)*S4(N)*W4(N))/(C3(N)*
     +D3(N)*FA4(N)*FZ4(N)*N4(N)*S3(N)
     +*W3(N))-1))/(GEI(N)**2*GTHC(N)*
     +((C2(N)*D2(N)*FA1(N)*FCDI(N)*FZ1(N)
     +*N1(N)*S2(N)*W2(N))/(C1(N)*
     +D1(N)*FA2(N)*FZ2(N)*N2(N)*S1(N)
     +*W1(N))-1))
C      GEC
      DEL(N,2,37)=(((Q0c(N)-0.429)/Erc(N)**
     +alfa(N)+0.429/((2*alfa(N)+1)
     +*0.55**alfa(N)))*GTHI(N)*((C4(N)*
     +D4(N)*FA3(N)*FCDC(N)*FZ3(N)*N3(N)
     +*S4(N)*W4(N))/(C3(N)*D3(N)*FA4(N)*
     +FZ4(N)*N4(N)*S3(N)*W3(N))-1))
     +/(GEI(N)*GTHC(N)*((C2(N)*D2(N)*
     +FA1(N)*FCDI(N)*FZ1(N)*N1(N)*S2(N)
     +*W2(N))/(C1(N)*D1(N)*FA2(N)*
     +FZ2(N)*N2(N)*S1(N)*W1(N))-1))
C      ERI
      DEL(N,2,38)=(0.429*alfa(N)*
     +Eri(N)**(-alfa(N)-1))/((2*alfa(N)+1)
     +*0.55**alfa(N))


C       FIM DO CALCULO DAS DERIVADAS DO K0
C
C
C
C      DERIVADAS EM RELACAO AOS PARAMETROS DO K03
C      PRIMEIRO CONJUNTO
C
C       N1
      DEL(N,3,1)=(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*(gec(N)*Q0C(N)+fii(N)
     +*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz
     +3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       FZ1
      DEL(N,3,2)=(c3(N)*d3(N)*efc(N)*fa1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)
     +*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       FA1
      DEL(N,3,3)=(c3(N)*d3(N)*efc(N)*fz1(N)*N1(N)*(gec(N)*Q0C(N)
     ++fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)
     +*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       D1
      DEL(N,3,4)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*Q0C(N)
     ++fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)**2*efi(N)
     +*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       C1
      DEL(N,3,5)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)**2*d1(N)*efi(N)
     +*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       S1
      DEL(N,3,6)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa
     +3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)**2*w1(N))
C       W1
      DEL(N,3,7)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa
     +3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N)**2)

C     SEGUNDO CONJUNTO
C       N3
      DEL(N,3,8)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa
     +3(N)*fz3(N)*n3(N)**2*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       FZ3
      DEL(N,3,9)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa
     +3(N)*fz3(N)**2*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       FA3
      DEL(N,3,10)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa
     +3(N)**2*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       D3
      DEL(N,3,11)=(c3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*Q0C(N)+
     +fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz
     +3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       C3
      DEL(N,3,12)=(d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*Q0C(N)+
     +fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz
     +3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       S3
      DEL(N,3,13)=(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz
     +3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       W3
      DEL(N,3,14)=(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz
     +3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       QOC
      DEL(N,3,15)=(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*gec(N)*N1(N)*s3(N)*
     +w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+f
     +ii(N)*gthi(N))*s1(N)*w1(N))
C     TERCEIRO CONJUNTO
C       GEC
      DEL(N,3,16)=(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*Q0C(N)*s3(N)*
     +w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0
     +ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       GEI
      DEL(N,3,17)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*q0ii(N)*s3(N)*w3(N))/(c1(N)*d1(N)*e
     +fi(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))**2*
     +s1(N)*w1(N))
C       GTHC
      DEL(N,3,18)=(c3(N)*d3(N)*efc(N)*fa1(N)*fii(N)*fz1(N)*N1(N)*s3(N)*
     +w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+f
     +ii(N)*gthi(N))*s1(N)*w1(N))
C       GTHI
      DEL(N,3,19)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fii(N)*fz1(N)*N1(N)*
     +(gec(N)*Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*ef
     +i(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))**2*s1(N)*
     +w1(N))
C       EFC
      DEL(N,3,20)=(c3(N)*d3(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*Q0C(N)+
     +fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)
     +*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       EFI
      DEL(N,3,21)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*N1(N)*(gec(N)*
     +Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)**2
     +*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))
C       FII
      DEL(N,3,22)= (c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*gthc(N)*N1(N)*
     +s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*
     +q0ii(N)+fii(N)*gthi(N))*s1(N)*w1(N))-(c3(N)*d3(N)*efc(N)*fa1(N)*
     +fz1(N)*gthi(N)*N1(N)*(gec(N)*Q0C(N)+fii(N)*gt
     +hc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*efi(N)*fa3(N)*fz3(N)*n3(N)*
     +(gei(N)*q0ii(N)+fii(N)*gthi(N))**2*s1(N)*w1(N))
c       Q0II
      DEL(N,3,23)=-(c3(N)*d3(N)*efc(N)*fa1(N)*fz1(N)*gei(N)*N1(N)*
     +(gec(N)*Q0C(N)+fii(N)*gthc(N))*s3(N)*w3(N))/(c1(N)*d1(N)*ef
     +i(N)*fa3(N)*fz3(N)*n3(N)*(gei(N)*q0ii(N)+fii(N)*gthi(N))**2*s1(N)*
     +w1(N))

C
C      FIM DOS C�LCULOS DAS DERIVADAS
C

3000  CONTINUE


      DO 3010 N=1,NN


      ASP1 = ((N1(N)*fz1(N)*fa1(N))/(D1(N)*C1(N)*S1(N)*w1(N)))
      ASP2 = N2(N)*fz2(N)*fa2(N)/(D2(N)*C2(N)*S2(N)*w2(N))

      ASP3 = ((N3(N)*fz3(N)*fa3(N))/(D3(N)*C3(N)*S3(N)*w3(N)))
      ASP4 = N4(N)*fz4(N)*fa4(N)/(D4(N)*C4(N)*S4(N)*w4(N))

      RCDI = ASP1/ASP2
      RCDC = ASP3/ASP4

      K0(N)= (ASP1-ASP2/FCDI(N))/(ASP3-ASP4/FCDC(N))
     +*(Gthc(N)*Efc(N))/(Gthi(N)*Efi(N))

      Q0alfac =  ((Q0c(N) - 0.429)/(ERC(N)**alfa(N))
     ++0.429/((2*Alfa(N)+1)*(0.55**Alfa(N))))

      Q0Ia(N) = (( Fcdc(N)*RCDC - 1 )/( Fcdi(N)*RCDI - 1 )
     +* (  Gthi(N)*Gec(N) /( Gthc(N)*Gei(N) ) )
     +*Q0alfac)


      Q0I(N) = ( Q0Ia(N) -0.429/( (2*
     +Alfa(N)+1)*(0.55**Alfa(N) ) ) )/
     +ERI(N)**Alfa(N) + 0.429

      Q02(N)=((FCDC(N)*N3(N)*FZ3(N)*FA3(N)*
     +D4(N)*C4(N)*S4(N)*W4(N))
     +/(D3(N)*C3(N)*S3(N)*W3(N)*N4(N)*
     +FZ4(N)*FA4(N))-1)/((FCDI(N)*N1(N)
     +*FZ1(N)*FA1(N)*D2(N)*C2(N)*S2(N)*
     +W2(N))/(D1(N)*C1(N)*S1(N)*W1(N)
     +*N2(N)*FZ2(N)*FA2(N))-1)*
     +(GTHI(N)*GEC(N)/(GTHC(N)*GEI(N)))
     +*(((Q0c(N)-0.429)/Erc(N)**alfa(N))+0.429/((2*alfa(N)+1)
     +*(0.55**alfa(N))))-((0.429/((2*alfa(N)+1)*
     +(0.55**alfa(N))))
     +/Eri(N)**alfa(N))+0.429

      FC =  Q0alfac*Gec(N)*(Fcdc(N)*RCDC - 1.)/Gthc(N)
      FI =  Q0Ia(N)*Gei(N)*(Fcdi(N)*RCDI - 1.)/Gthi(N)

      K02(N) = (  ASP1*(Gthc(N)*FI + Gec(N)*Q0alfac)*Efc(N) )/
     +         (  ASP3*(GthI(N)*FI + GeI(N)*Q0Ia(N))*Efi(N) )

      K03(N)=(ASP1*(Gthc(N)*Fii(N) + Gec(N)*Q0alfac)*Efc(N) )/
     +    (ASP3*(GthI(N)*FII(N) + GeI(N)*Q0II(N))*Efi(N) )

      WRITE(6,*)'N',N
      WRITE(6,*)'ASP1     ',ASP1,     '  ASP2    ',ASP2
      WRITE(6,*)'ASP3     ',ASP3,     '  ASP4    ',ASP4
      WRITE(6,*)'FCDI(N)  ',FCDI(N)  ,'  FCDC(N) ',FCDC(N)
      WRITE(6,*)'Gthc(N)  ',Gthc(N),  '  Gthi(N) ',Gthi(N)
      WRITE(6,*)'Efc(N)   ',Efc(N),   '  Efi(N)  ',Efi(N)
      WRITE(6,*)'K0(N)    ',K0(N),    '  Q0I(N)  ',Q0I(N)
      WRITE(6,*)'K02(N)   ',K02(N)
      WRITE(6,*)'K03(N)   ',K03(N),   '  FII(N)  ',FII(N)
      WRITE(6,*)'Gec(N)   ',Gec(N),   '  Q0II(N) ',Q0II(N)
      WRITE(6,*)'Q0alfac  ',Q0alfac,  '  Gei(N)  ',Gei(N)
      WRITE(6,*)'Q0Ia(N)  ',Q0Ia(N),  '  Q02(N)  ',Q02(N)
      WRITE(6,*)'RCDI     ',RCDI,     '  RCDC    ',RCDC
      WRITE(6,*)'FC       ',FC,       '  FI      ',FI

      WRITE(6,*)

2000  CONTINUE


3010  CONTINUE


C
C     CALCULO DA MATRIZ DE COVARIANCIA


C
C   WRITE(6,*)
C   WRITE(6,*) 'COVAR FINAL'
C   WRITE(6,*)

      DO 3020 NA=1,NN
      DO 3015 NB=1,NN


      DO 1110 I=1,3
      DO 1510 J=1,3

      COVAR(NA,NB,I,J) = 0
      DO 1610 K=1,M2

      IF (ID1(NA) .EQ. ID1(NB) .AND. ERG1(NA) .EQ. ERG1(NB)) THEN
      PARC =  DEL(NA,I,K)*DEL(NB,J,K)*RO(I,J,K)*SIG(NA,I,K)*SIG(NB,J,K)
      ELSE IF (ID1(NA) .EQ. ID1(NB) .AND. ERG1(NA) .NE. ERG1(NB)) THEN
      PARC =  DEL(NA,I,K)*DEL(NB,J,K)*ROC(I,J,K)*SIG(NA,I,K)*SIG(NB,J,K)
      ELSE
      PARC =  DEL(NA,I,K)*DEL(NB,J,K)*ROB(I,J,K)*SIG(NA,I,K)*SIG(NB,J,K)
      ENDIF
      COVAR(NA,NB,I,J) = COVAR(NA,NB,I,J) + PARC
c      write(*, *) COVAR(NA,NB,I,J)
c   IF(NA .EQ. 3 .AND. NB .EQ. 3) THEN
C       WRITE(*,*) 'NA,NB,I,J,K',NA,NB,I,J,K,'  PARC',PARC,
C     '               '  COVAR(NA,NB,I,J)',COVAR(NA,NB,I,J)
C       PAUSE
c   ENDIF

1610  CONTINUE

C      WRITE(6,4000) NA,NB,I,J,COVAR(NA,NB,I,J)
c      WRITE(*,4000) 'NA',NA,'  NB',NB,'  I',I,'  J',J,'
c     +COVAR(NA,NB,I,J) ',COVAR(NA,NB,I,J)

C   PAUSE

1510  CONTINUE

1110  CONTINUE

3015  CONTINUE

3020  CONTINUE

C
C     MATRIZ DE CORRELACAO
C

C   WRITE(6,*)
C      WRITE(6,*) 'CORRELACAO FINAL'
C   WRITE(6,*)

      DO 3030 NA=1,NN
      DO 3025 NB=1,NN

      DO 1120 I=1,3
      DO 1520 J=1,3

      CORREL(NA,NB,I,J) = 1000*COVAR(NA,NB,I,J)/SQRT(COVAR(NA,NA,I,I)*
     +COVAR(NB,NB,J,J))

C      WRITE(6,4010) NA,NB,I,J,CORREL(NA,NB,I,J)
C      WRITE(*,*) 'NA',NA,'  NB',NB,'  I',I,'  J',J,'
C   'CORREL(NA,NB,I,J)',CORREL(NA,NB,I,J)

1520  CONTINUE
1120  CONTINUE

3025  CONTINUE
3030  CONTINUE

4000  FORMAT('NA =  ',I6,'  NB =  ',I6,'  I =  ',I6,'  J =  ',I6,
     +  '  COVAR(NA,NB,I,J) =  ',1P,E14.4,0P )
4010  FORMAT('NA =  ',I6,'  NB =  ',I6,'  I =  ',I6,'  J =  ',I6,
     +  '  CORREL(NA,NB,I,J) =  ',F8.0 )

C   WRITE(6,*)
C      WRITE(6,*) 'CORRELACAO FINAL ORDENADA'
C   WRITE(6,*)

      DO 3031 NA=1,NN

      DO 1121 I=1,3

      DO 3026 NB=1,NN

      DO 1521 J=1,3

      CORREL(NA,NB,I,J) = 1000*COVAR(NA,NB,I,J)/SQRT(COVAR(NA,NA,I,I)*
     +COVAR(NB,NB,J,J))

C      WRITE(6,4010) NA,NB,I,J,CORREL(NA,NB,I,J)
C      WRITE(*,*) 'NA',NA,'  NB',NB,'  I',I,'  J',J,'
C   +CORREL(NA,NB,I,J)',CORREL(NA,NB,I,J)

1521  CONTINUE

3026  CONTINUE

1121  CONTINUE

3031  CONTINUE

4001  FORMAT('NA =  ',I6,'  NB =  ',I6,'  I =  ',I6,'  J =  ',I6,
     +  '  COVAR(NA,NB,I,J) =  ',1P,E14.4,0P )
4011  FORMAT('NA =  ',I6,'  NB =  ',I6,'  I =  ',I6,'  J =  ',I6,
     '  '  CORREL(NA,NB,I,J) =  ',F8.0 )

      WRITE(6,*)
      WRITE(6,*) 'PARAMETROS K0, Q0 E K03'
      WRITE(6,*)

      M = 2*NN

      X(1) = ' K0 ='
      X(2) = ' Q0 ='
      X(3) = 'K03 ='


      DO 5000 NA = 1,NN
      DO 5010 I = 1,3

      IF (I .EQ. 1) THEN
        PAR = K0(NA)
      ELSEIF (I .EQ. 2) THEN
        PAR = Q0I(NA)
      ELSE
        PAR = K03(NA)
      ENDIF

      DO 5030 J = 1,3

      IF(I .EQ. J) THEN
      SIGP = SQRT( COVAR(NA,NA,I,J) )
      WRITE(*,4020) NA,X(I),PAR,SIGP
      WRITE(6,4020) NA,X(I),PAR,SIGP
      ENDIF

5030  CONTINUE
5010  CONTINUE
5000  CONTINUE

      WRITE(6,*)
      WRITE(6,*) 'MATRIZ DE COVARIANCIA ENTRE OS PARAMETROS Q0 E K0'
      WRITE(6,*)

      WRITE(6,4025)((((COVAR(NA,NB,I,J),J=1,2),
     +NB=1,NN),NA,I=1,2),NA =1,NN)


      WRITE(6,*)
      WRITE(6,*) 'MATRIZ DE COVARIANCIA ENTRE OS PARAMETROS Q0 E K03'
      WRITE(6,*)

      WRITE(6,4025)((((COVAR(NA,NB,I,J),J=2,3),
     +NB=1,NN),NA,I=2,3),NA =1,NN)


      WRITE(6,*)
      WRITE(6,*) 'MATRIZ DE CORRELACAO ENTRE OS PARAMETROS Q0 E K0'
      WRITE(6,*)

      WRITE(6,4030)((((CORREL(NA,NB,I,J),J=1,2),
     +NB=1,NN),NA,I=1,2),NA =1,NN)

      WRITE(6,*)
      WRITE(6,*) 'MATRIZ DE CORRELACAO ENTRE OS PARAMETROS Q0 E K03'
      WRITE(6,*)

      WRITE(6,4030)((((CORREL(NA,NB,I,J),J=2,3),
     +NB=1,NN),NA,I=2,3),NA =1,NN)

4020  FORMAT('NA = ',I3,4X,A5,2X,1P,E12.4,2X,E12.4,0P)
4025  FORMAT(12(1P,E9.1,0P),3X,'NA =',I3 )
4030  FORMAT(12(F8.0) ,3X,'NA =',I3)


C
C
C
      CLOSE (1)
      CLOSE (2)
      CLOSE (3)
      CLOSE (6)
C      READ (*,*)
      STOP
      END PROGRAM
