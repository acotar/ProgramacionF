! Math . f90 : demo some Fortran math functions
!-----------------------------------------------

Program Math_test ! Begin main program
REAL *8 :: x = -1.0 , w = 2.0 , z = 0
COMPLEX :: y
! Declare variables x, y, z, w


y = SQRT(x) ! Call the sqrt function
z = ASIN (2.0) ! Call the asin function
w = LOG (z)
print * , x, y,w,z !Print x, y, z,w
End program Math_test ! End main program

