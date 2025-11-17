program position_solver
    implicit none
    integer, parameter :: dp = kind(1.0d0)
    integer, parameter :: nb = 100
    integer :: n, i, iter, max_iter
    real(dp) :: Xi(nb), Yi(nb), Zi(nb), Di(nb)
    real(dp) :: X0, Y0, Z0
    real(dp) :: G(nb,3), D(nb)
    real(dp) :: GT(3,nb), GTG(3,3), GTG_inv(3,3), GTD(3)
    real(dp) :: M(3)
    real(dp) :: DX, DY, DZ, diff, tol
    logical :: converged

    !==============================================================
    ! User input
    !==============================================================
    print *, 'Enter number of stations (>=4):'
    read *, n
    if (n < 4) then
        print *, 'Error: at least 4 stations are required.'
        stop
    endif

    print *, 'Enter station data: Xi Yi Zi Di (space-separated)'
    do i = 1, n
        read *, Xi(i), Yi(i), Zi(i), Di(i)
    enddo

    print *, 'Enter initial position X0 Y0 Z0:'
    read *, X0, Y0, Z0

    !==============================================================
    ! Iteration parameters
    !==============================================================
    max_iter = 50
    tol = 1.0d-6
    converged = .false.

    do iter = 1, max_iter
        call build_G_D(n, Xi, Yi, Zi, Di, X0, Y0, Z0, G, D)
        call mat_transpose(G, GT, n, 3)
        call mat_mult(GT, G, GTG, 3, n, 3)
        call mat_mult_vec(GT, D, GTD, 3, n)
        
        ! 檢查矩陣條件
        if (abs(GTG(1,1)) < 1.0d-15 .or. abs(GTG(2,2)) < 1.0d-15 .or. abs(GTG(3,3)) < 1.0d-15) then
            print *, 'Warning: Matrix is nearly singular at iteration', iter
            print *, 'GTG diagonal:', GTG(1,1), GTG(2,2), GTG(3,3)
        endif
        
        call MATRIXINV(GTG, 3)

        ! M = (GTG)^(-1) * GTD
        call mat_mult_vec(GTG, GTD, M, 3, 3)

        DX = M(1)
        DY = M(2)
        DZ = M(3)

        X0 = X0 + DX
        Y0 = Y0 + DY
        Z0 = Z0 + DZ

        diff = sqrt(DX*DX + DY*DY + DZ*DZ)
        if (diff < tol) then
            converged = .true.
            exit
        endif
    enddo

    !==============================================================
    ! Output result
    !==============================================================
    print *, '-----------------------------------------'
    if (converged) then
        print *, 'Converged in', iter, 'iterations.'
    else
        print *, 'Warning: did not converge after', max_iter, 'iterations.'
    endif
    print *, 'Target position:'
    print *, '  X =', X0
    print *, '  Y =', Y0
    print *, '  Z =', Z0
    print *, '-----------------------------------------'

contains

    !--------------------------------------------------------------
    ! 高斯-牛頓法建立雅可比矩陣 G 和殘差向量 D
    ! subroutine build_G_D(n, Xi, Yi, Zi, Di, X0, Y0, Z0, G, D)
    !     integer, intent(in) :: n
    !     real(dp), intent(in) :: Xi(n), Yi(n), Zi(n), Di(n)
    !     real(dp), intent(in) :: X0, Y0, Z0
    !     real(dp), intent(out) :: G(n,3), D(n)
    !     integer :: i
    !     real(dp) :: dist_calc
    !     do i = 1, n
    !         ! 計算當前估計點到測站i的距離
    !         dist_calc = sqrt((X0 - Xi(i))**2 + (Y0 - Yi(i))**2 + (Z0 - Zi(i))**2)
            
    !         ! 避免除以零
    !         if (dist_calc < 1.0d-10) then
    !             dist_calc = 1.0d-10
    !         endif
            
    !         ! 雅可比矩陣 G
    !         G(i,1) = (X0 - Xi(i)) / dist_calc
    !         G(i,2) = (Y0 - Yi(i)) / dist_calc  
    !         G(i,3) = (Z0 - Zi(i)) / dist_calc
            
    !         ! 殘差向量 D
    !         D(i) = Di(i) - dist_calc
    !     enddo
    ! end subroutine build_G_D
    subroutine build_G_D(n, Xi, Yi, Zi, Di, X0, Y0, Z0, G, D)
        integer, intent(in) :: n
        real(dp), intent(in) :: Xi(n), Yi(n), Zi(n), Di(n)
        real(dp), intent(in) :: X0, Y0, Z0
        real(dp), intent(out) :: G(n,3), D(n)
        integer :: i
        do i = 1, n
            ! 矩陣 G 對應 (Xi-X0), (Yi-Y0), (Zi-Z0)
            G(i,1) = (X0 - Xi(i))
            G(i,2) = (Y0 - Yi(i))
            G(i,3) = (Z0 - Zi(i))
            ! 方程右端 0.5*(Di^2 - (Xi-X0)^2 - (Yi-Y0)^2 - (Zi-Z0)^2)
            D(i) = 0.5d0 * ( Di(i)**2 - (Xi(i)-X0)**2 - (Yi(i)-Y0)**2 - (Zi(i)-Z0)**2 )
        enddo
    end subroutine build_G_D
    !--------------------------------------------------------------
    subroutine mat_transpose(A, AT, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(out) :: AT(n,m)
        integer :: i,j
        do i=1,m
            do j=1,n
                AT(j,i) = A(i,j)
            enddo
        enddo
    end subroutine mat_transpose
    !--------------------------------------------------------------
    subroutine mat_mult(A, B, C, m, n, p)
        integer, intent(in) :: m, n, p
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(in) :: B(n,p)
        real(dp), intent(out) :: C(m,p)
        integer :: i,j,k
        C = 0.0d0
        do i=1,m
            do j=1,p
                do k=1,n
                    C(i,j) = C(i,j) + A(i,k)*B(k,j)
                enddo
            enddo
        enddo
    end subroutine mat_mult
    !--------------------------------------------------------------
    subroutine mat_mult_vec(A, B, C, m, n)
        integer, intent(in) :: m, n
        real(dp), intent(in) :: A(m,n)
        real(dp), intent(in) :: B(n)
        real(dp), intent(out) :: C(m)
        integer :: i,k
        C = 0.0d0
        do i=1,m
            do k=1,n
                C(i) = C(i) + A(i,k)*B(k)
            enddo
        enddo
    end subroutine mat_mult_vec
    !--------------------------------------------------------------
    subroutine MATRIXINV(C, n)
        integer, intent(in) :: n
        real(dp), intent(inout) :: C(n,n)
        integer :: i, j
        integer :: indx(n)
        real(dp) :: y(n,n), D

        y = 0.0d0
        do i=1,n
            y(i,i) = 1.0d0
        enddo

        call LUDCMP(C, n, indx, D)
        do j=1,n
            call LUBKSB(C, n, indx, y(1,j))
        enddo

        do i=1,n
            do j=1,n
                C(i,j) = y(i,j)
            enddo
        enddo
    end subroutine MATRIXINV
    !--------------------------------------------------------------
    subroutine LUDCMP(A, N, INDX, D)
        integer, intent(in) :: N
        real(dp), intent(inout) :: A(N,N)
        integer, intent(out) :: INDX(N)
        real(dp), intent(out) :: D
        integer :: I, IMAX, J, K
        real(dp) :: VV(N), SUM, AAMAX, DUM
        real(dp), parameter :: TINY = 1.0d-12

        D = 1.0d0
        do I=1,N
            AAMAX = 0.0d0
            do J=1,N
                AAMAX = max(AAMAX, abs(A(I,J)))
            enddo
            if (AAMAX == 0.0d0) stop 'Singular matrix'
            VV(I) = 1.0d0 / AAMAX
        enddo

        do J=1,N
            do I=1,J-1
                SUM = A(I,J)
                do K=1,I-1
                    SUM = SUM - A(I,K)*A(K,J)
                enddo
                A(I,J) = SUM
            enddo
            AAMAX = 0.0d0
            do I=J,N
                SUM = A(I,J)
                do K=1,J-1
                    SUM = SUM - A(I,K)*A(K,J)
                enddo
                A(I,J) = SUM
                DUM = VV(I)*abs(SUM)
                if (DUM >= AAMAX) then
                    IMAX = I
                    AAMAX = DUM
                endif
            enddo
            if (J /= IMAX) then
                ! 正確的行交換
                do K=1,N
                    DUM = A(IMAX,K)
                    A(IMAX,K) = A(J,K)
                    A(J,K) = DUM
                enddo
                D = -D
                VV(IMAX) = VV(J)
            endif
            INDX(J) = IMAX
            if (A(J,J) == 0.0d0) A(J,J) = TINY
            if (J /= N) then
                DUM = 1.0d0 / A(J,J)
                A(J+1:N,J) = A(J+1:N,J) * DUM
            endif
        enddo
    end subroutine LUDCMP
    !--------------------------------------------------------------
    subroutine LUBKSB(A, N, INDX, B)
        integer, intent(in) :: N
        real(dp), intent(in) :: A(N,N)
        real(dp), intent(inout) :: B(N)
        integer, intent(in) :: INDX(N)
        integer :: I, II, J, LL
        real(dp) :: SUM

        II = 0
        do I=1,N
            LL = INDX(I)
            SUM = B(LL)
            B(LL) = B(I)
            if (II /= 0) then
                do J=II,I-1
                    SUM = SUM - A(I,J)*B(J)
                enddo
            else if (SUM /= 0.0d0) then
                II = I
            endif
            B(I) = SUM
        enddo
        do I=N,1,-1
            SUM = B(I)
            do J=I+1,N
                SUM = SUM - A(I,J)*B(J)
            enddo
            B(I) = SUM / A(I,I)
        enddo
    end subroutine LUBKSB
    !--------------------------------------------------------------

end program position_solver
