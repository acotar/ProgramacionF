PROGRAM Mareas

IMPLICIT NONE



REAL, DIMENSION (7674):: altura
INTEGER :: i
real :: M1, M2, M3, M4, M5, Dif    ! Maximox
real :: T1, T2, T3, T4, T5 ! Tiempox
real :: Dif2, 1Min, 2Min, 3Min, 4Min, 5Min !Minm#
real :: T1n, T2n, T3n, T4n, T5n !Tiempon
real :: Dif3, d1Mx, d2Mx, d3Max, d4Max, d5Max !Maxd#
real :: T1x, T2x, T3x, T4x, T5x !Tiempod#x
real :: Dif4, 1Mi, 2Mi, 3Mi, 4Mi, 5Mi !Mind#
real :: 1nT, 2nT, 3nT, 4nT, 5nT !Tiempod#n
real :: PM1, PM2, PM3, PM4, PM5 !PeriodomM#
REAL :: PN1, PN2, PN3, PN4, PN5 !PeriodomN#
REAL :: PdM1, PdM2, PdM3, PdM4, PdM5 !PeriododM#
REAL :: PdN1, PdN2, PdN3, PdN4, PdN5 !PeriododN#
REAL :: Mensual_maximo !Periodo_mensual_max
REAL :: Mensual_minimo !Periodo_mensual_min
REAL :: Diario_max !Periodo_diario_max
REAL :: Diario_min !Periodo_diario_min





OPEN (1,file="Mareas.csv")

DO i=1,7674
READ (1,*) altura(i)
END DO
CLOSE (1)
!*************************************
M1 = 0
DO i=1,1344
Dif=M1 - altura(i)
IF (Dif < 0) THEN 
M1 = altura (i)

Tiempom1x= i/48.0

END IF
END DO

!************************************

M2 = 0
DO i=1345,2690
Dif =  M2 - altura(i)
IF (Dif < 0) THEN 
M2 = altura(i)

Tiempom2x=i/48.0
END IF
END DO

!************************************

M3 = 0
DO i=2691,4035
Dif = M3 - altura(i)
IF (Dif < 0) THEN 
M3 = altura (i)

Tiempom3x=i/48.0
END IF
END DO 

!************************************

M4 = 0
DO i=4036,5380
Dif = M4 - altura(i)
IF (Dif < 0) THEN 
M4 = altura (i)

Tiempom4x=i/48.0
END IF
END DO

!************************************

M5 = 0
DO i=5381, 6725
Dif = M5 - altura(i)
IF (Dif < 0) THEN 
M5 = altura (i)

Tiempom5x=i/48.0
END IF
END DO

!************************************

Minm1 = 0
DO i= 1, 1344
Dif2= Minm1 - altura(i)
IF (Dif2> 0) THEN 
Minm1 = altura (i)

Tiempom1n=i/48.0
END IF
END DO

!************************************

Minm2 = 0
DO i= 1345, 2690
Dif2= Minm2 - altura(i)
IF (Dif2> 0) THEN 
Minm2 = altura (i)

Tiempom2n=i/48.0
END IF
END DO

!************************************

Minm3 = 0
DO i= 2691, 4035
Dif2= Minm3 - altura(i)
IF (Dif2> 0) THEN 
Minm3 = altura (i)

Tiempom3n=i/48.0
END IF
END DO

!************************************

Minm4 = 0
DO i= 4036, 5380
Dif2= Minm4 - altura(i)
IF (Dif2> 0) THEN 
Minm4 = altura (i)

Tiempom4n=i/48.0
END IF
END DO

!************************************

Minm3 = 0
DO i= 5381, 6725
Dif2= Minm5 - altura(i)
IF (Dif2> 0) THEN 
Minm5 = altura (i)

Tiempom5n=i/48.0
END IF
END DO


!************************************

Maxd1 = 0
DO i= 18, 65
Dif3= Maxd1- altura(i)
IF (Dif3< 0) THEN 
Maxd1 = altura (i)

Tiempod1x= i * 0.5

END IF
END DO

!************************************

Maxd2 = 0
DO i= 66, 113
Dif2=  Maxd2 - altura(i)
IF (Dif3< 0) THEN 
Maxd2 = altura(i)

Tiempod2x=(i* 0.5)

END IF
END DO

!************************************

Maxd3 = 0
DO i= 114, 161
Dif3= Maxd3 - altura(i)
IF (Dif3< 0) THEN 
Maxd3 = altura (i)

Tiempod3x=(i* 0.5)

END IF
END DO 

!************************************

Maxd4 = 0
DO i= 162, 209
Dif3= Maxd4 - altura(i)
IF (Dif3< 0) THEN 
Maxd4 = altura (i)

Tiempod4x=(i* 0.5)

END IF
END DO 

!************************************

Maxd5 = 0
DO i= 210, 257
Dif3= Maxd5 - altura(i)
IF (Dif3< 0) THEN 
Maxd5 = altura (i)

Tiempod5x=(i* 0.5)

END IF
END DO 

!************************************

Mind1 = 0
DO i= 18, 65
Dif4= Mind1 - altura(i)
IF (Dif4> 0) THEN 
Mind1 = altura (i)

Tiempod1n=i * 0.5

END IF
END DO

Mind2 = 0
DO i= 66, 113
Dif4= Mind2 - altura(i)
IF (Dif2> 0) THEN 
Mind2 = altura (i)

Tiempod2n=( i * 0.5) 
END IF
END DO

Mind3 = 0
DO i= 114, 161
Dif4= Mind3 - altura(i)
IF (Dif4> 0) THEN 
Mind3 = altura (i)

Tiempod3n=(i* 0.5) 

END IF
END DO

Mind4 = 0
DO i= 162, 209
Dif4= Mind4 - altura(i)
IF (Dif4> 0) THEN 
Mind4 = altura (i)

Tiempod4n=(i* 0.5) 

END IF
END DO

Mind5 = 0
DO i= 210, 257
Dif4= Mind5 - altura(i)
IF (Dif4> 0) THEN 
Mind5 = altura (i)

Tiempod5n=(i* 0.5) 

END IF
END DO
!--------------------------------------------

PeriodomM1 = Tiempom1x 
PeriodomM2 = Tiempom2x - Tiempom1x
PeriodomM3 = Tiempom3x - Tiempom2x
PeriodomM4 = Tiempom4x - Tiempom3x
PeriodomM5 = Tiempom5x - Tiempom4x

PeriodomN1 = Tiempom1n 
PeriodomN2 = Tiempom2n - Tiempom1n
PeriodomN3 = Tiempom3n - Tiempom2n
PeriodomN4 = Tiempom4n - Tiempom3n
PeriodomN5 = Tiempom5n - Tiempom4n

PeriododM1 = Tiempod1x 
PeriododM2 = Tiempod2x - Tiempod1x
PeriododM3 = Tiempod3x - Tiempod2x
PeriododM4 = Tiempod4x - Tiempod3x
PeriododM5 = Tiempod5x - Tiempod4x

PeriododN1 = Tiempod1n 
PeriododN2 = Tiempod2n - Tiempod1n
PeriododN3 = Tiempod3n - Tiempod2n
PeriododN4 = Tiempod4n - Tiempod3n
PeriododN5 = Tiempod5n - Tiempod4n

!---------------------------------------------

Periodo_mensual_max = (PeriodomM1 + PeriodomM2 + PeriodomM3 + PeriodomM4 + PeriodomM5)/5.0

Periodo_mensual_min = (PeriodomN1 + PeriodomN2 + PeriodomN3 + PeriodomN4 + PeriodomN5)/5.0

Periodo_diario_max = (PeriododM1 +PeriododM2 +PeriododM3 + PeriododM4 + PeriododM5)/5.0

Periodo_diario_min = (PeriododN1 +PeriododN2 +PeriododN3 + PeriododN4 + PeriododN5)/5.0




Print *, 'Las mareas maximas mensuales fueron:'       
Print *, 'Primer mes:', Maxm1,'En el dia:', Tiempom1x
Print *, 'Segundo mes:',Maxm2,'En el dia:', Tiempom2x              
Print *, 'Tercer mes:',Maxm3,'En el dia:', Tiempom3x
Print *, 'Cuarto  mes:',Maxm4,'En el dia:', Tiempom4x             
Print *, 'Quinto mes:',Maxm5,'En el dia:', Tiempom5x

Print *, 'Las mareas minimas mensuales fueron:'
       
Print *, 'Primer mes:',Minm1, 'En el dia:', Tiempom1n
Print *, 'Segundo mes:',Minm2,'En el dia:', Tiempom2n           
Print *, 'Tercer mes:',Minm3,'En el dia:', Tiempom3n
Print *, 'Cuarto  mes:',Minm4,'En el dia:', Tiempom4n              
Print *, 'Quinto  mes:',Minm5,'En el dia:', Tiempom5n

Print *, 'El periodo mensual de la  marea  maxima  es:', Periodo_mensual_max, 'dias'

Print *, 'El periodo mensual de la marea minima es:', Periodo_mensual_min, 'dias'

Print *, 'Las mareas maximas diarias fueron:'       
Print *, 'Primer dia:', Maxd1
Print *, 'Segundo dia:',Maxd2           
Print *, 'Tercer dia:',Maxd3
Print *, 'Cuarto dia:',Maxd4           
Print *, 'Quinto dia:',Maxd5

Print *, 'Las mareas minimas diarias fueron:'     

Print *, 'Primer dia:',Mind1
Print *, 'Segundo dia:',Mind2             
Print *, 'Tercer dia:',Mind3
Print *, 'Cuarto dia:',Mind4              
Print *, 'Quinto dia:',Mind5


Print *, 'El periodo diario de la marea maxima es:', Periodo_diario_max, 'hrs'

Print *, 'El periodo diario de la marea  minima es:', Periodo_diario_min, 'hrs' 



end program Mareas
