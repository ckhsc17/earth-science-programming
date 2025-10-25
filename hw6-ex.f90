program sum_recursive
  implicit none
  integer :: result

  ! 呼叫遞迴函數
  result = sum_to_n(100)

  print *, "Sum from 1 to 100 = ", result

contains

  recursive function sum_to_n(n) result(total)
    implicit none
    integer, intent(in) :: n
    integer :: total

    if (n <= 1) then
       total = 1
    else
       total = n + sum_to_n(n - 1)
    end if
  end function sum_to_n

end program sum_recursive
