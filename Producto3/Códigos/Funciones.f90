! Math . f90 : demo some Fortran math functions
!-----------------------------------------------

Program Math_test ! Begin main program
Real *8 :: x = 1.0 , y, z ! Declare variables x, y, z

y = sin (x) ! Call the sine function
z = exp (x) + 1.0 ! Call the exponential function
print * , x, y, z ! Print x, y, z
End program Math_test ! End main program

