! Objetivo del programa:
! Obtener el volumen de una esfera dado el radio por el usuario

PROGRAM Volumen_Esfera

IMPLICIT NONE 

! Declaracion de variables

REAL :: Volumen
REAL :: radio
REAL, PARAMETER :: PI = 3.1416

!-----------------------------------------------------------------

! Obtener el valor del radio

Write (*,*) " Ingrese el radio de la esfera "
READ (*,*) radio

!-------------------------------------------------------------------

! Calcular el volumen

Volumen = (4/3) * PI * radio**3

!------------------------------------------------------------------

! Brindar la informacion

WRITE (*,*) " El volumen de la esfera es de = ", Volumen

!------------------------------------------------------------------


END PROGRAM Volumen_Esfera
