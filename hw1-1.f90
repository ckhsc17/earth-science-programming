! 1. 兩點決定一條直線
program line_from_two_points
  real :: x1, y1, x2, y2, a, b
  print *, '請輸入第一個點 (x1, y1):'
  read *, x1, y1
  print *, '請輸入第二個點 (x2, y2):'
  read *, x2, y2
  if (x1 == x2) then
    print *, '直線為 x =', x1
  else
    a = (y2 - y1) / (x2 - x1)
    b = y1 - a * x1
    print *, '直線方程式: y = ', a, '*x + ', b
  end if
end program line_from_two_points