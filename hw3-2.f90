program gaussj_inverse
  implicit none
  integer, parameter :: n = 3
  real :: A(n,n), aug(n,2*n)
  integer :: i, j, k
  real :: factor, pivot

  ! 輸入矩陣 A
  print *, 'Enter a 3x3 matrix (row by row):'
  do i = 1, n
     read(*,*) (A(i,j), j=1,n)
  end do

  ! 建立增廣矩陣 [A | I]
  aug(:,1:n) = A
  aug(:,n+1:2*n) = 0.0
  do i = 1, n
     aug(i,n+i) = 1.0
  end do

  ! Gauss–Jordan 消去法
  do i = 1, n
     pivot = aug(i,i)
     if (abs(pivot) < 1.0e-8) then
        print *, 'Matrix is singular, cannot invert.'
        stop
     end if

     ! 先把該列的主元素變成 1
     aug(i,:) = aug(i,:) / pivot

     ! 消去其他列的該欄
     do j = 1, n
        if (j /= i) then
           factor = aug(j,i)
           aug(j,:) = aug(j,:) - factor * aug(i,:)
        end if
     end do
  end do

  ! 印出結果
  print *, 'Inverse matrix:'
  do i = 1, n
     print '(3F12.6)', aug(i,n+1:2*n)
  end do

end program gaussj_inverse
