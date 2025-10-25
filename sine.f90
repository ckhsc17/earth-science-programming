program sine_table
  implicit none
  real :: x, y, xstep

  ! Step size = π / 1000
  xstep = 3.141592654 / 1000.0

  ! Loop from 0.0 to 3.1416 (approx π) with step xstep
  do x = 0.0, 3.1416, xstep
     y = sin(x)
     print *, x, y
  end do

end program sine_table
