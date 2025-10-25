program inverse3x3
  implicit none
  real :: a(3,3), inv(3,3)
  real :: det
  integer :: i, j

  ! 輸入矩陣
  print *, 'Enter a 3x3 matrix (row by row):'
  do i = 1, 3
     read(*,*) (a(i,j), j=1,3)
  end do

  ! 計算行列式
  det = a(1,1)*(a(2,2)*a(3,3) - a(2,3)*a(3,2)) - &
        a(1,2)*(a(2,1)*a(3,3) - a(2,3)*a(3,1)) + &
        a(1,3)*(a(2,1)*a(3,2) - a(2,2)*a(3,1))

  if (abs(det) < 1.0e-6) then
     print *, 'Matrix is singular, no inverse exists.'
     stop
  end if

  ! 計算伴隨矩陣 (cofactor matrix 的轉置)
  inv(1,1) =  (a(2,2)*a(3,3) - a(2,3)*a(3,2)) / det
  inv(1,2) = -(a(1,2)*a(3,3) - a(1,3)*a(3,2)) / det
  inv(1,3) =  (a(1,2)*a(2,3) - a(1,3)*a(2,2)) / det

  inv(2,1) = -(a(2,1)*a(3,3) - a(2,3)*a(3,1)) / det
  inv(2,2) =  (a(1,1)*a(3,3) - a(1,3)*a(3,1)) / det
  inv(2,3) = -(a(1,1)*a(2,3) - a(1,3)*a(2,1)) / det

  inv(3,1) =  (a(2,1)*a(3,2) - a(2,2)*a(3,1)) / det
  inv(3,2) = -(a(1,1)*a(3,2) - a(1,2)*a(3,1)) / det
  inv(3,3) =  (a(1,1)*a(2,2) - a(1,2)*a(2,1)) / det

  ! 輸出反矩陣
  print *, 'Inverse matrix:'
  do i = 1, 3
     write(*,'(3F12.6)') (inv(i,j), j=1,3)
  end do

end program inverse3x3
