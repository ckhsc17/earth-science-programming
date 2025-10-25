program plplot_demo
    use plplot
    implicit none
    integer :: i, n
    real*8 :: x, y

    n = 100

    call plinit()             ! 初始化
    call plenv(0.0, 2.0*3.1416, -1.0, 1.0, 0, 0)  ! 設定座標軸
    call pllab("x", "y", "y = sin(x)")             ! 標籤

    do i = 0, n
        x = 2.0 * 3.1416 * dble(i) / dble(n)
        y = sin(x)
        call plpoin(1, x, y, 9)   ! 畫點
    end do

    call plend()
end program plplot_demo