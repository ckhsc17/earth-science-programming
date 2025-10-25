! 1A2B猜數字遊戲
program guess_number_game
  implicit none
  integer :: secret(3), guess(3)
  integer :: a_count, b_count
  integer :: attempts
  character(len=3) :: input_str
  logical :: valid_input
  
  ! 初始化隨機數種子
  call random_seed()
  
  ! 產生不重複的3位密碼
  call generate_secret(secret)
  
  print *, '歡迎來到1A2B猜數字遊戲!'
  print *, '請猜一個3位數字（數字不重複）'
  print *, 'A表示數字正確位置正確，B表示數字正確位置錯誤'
  print *, ''
  
  attempts = 0
  
  ! 使用GOTO製作遊戲迴圈
100 continue
  attempts = attempts + 1
  print *, '第', attempts, '次猜測'
  
  ! 輸入並驗證猜測
200 continue
  print *, '請輸入3位數字（不重複）:'
  read *, input_str
  
  ! 驗證輸入
  call validate_input(input_str, guess, valid_input)
  if (.not. valid_input) then
    print *, '輸入無效！請輸入3個不重複的數字'
    goto 200
  end if
  
  ! 計算A和B
  call calculate_ab(secret, guess, a_count, b_count)
  
  ! 顯示結果
  print *, '結果: ', a_count, 'A', b_count, 'B'
  
  ! 檢查是否猜中（3A）
  if (a_count == 3) then
    print *, '恭喜！你猜中了！'
    print *, '總共猜了', attempts, '次'
    goto 999  ! 結束遊戲
  end if
  
  ! 繼續猜測
  goto 100
  
999 continue
  print *, '遊戲結束'
  
end program guess_number_game

! 產生不重複的3位密碼
subroutine generate_secret(secret)
  implicit none
  integer, intent(out) :: secret(3)
  integer :: i, j, temp
  real :: rand_num
  logical :: duplicate
  
  do i = 1, 3
300   continue
    call random_number(rand_num)
    temp = int(rand_num * 10)
    
    ! 檢查是否重複
    duplicate = .false.
    do j = 1, i-1
      if (secret(j) == temp) then
        duplicate = .true.
        goto 300
      end if
    end do
    
    if (.not. duplicate) then
      secret(i) = temp
    else
      goto 300
    end if
  end do
  
  ! 顯示密碼（除錯用）
  print *, '密碼是:', secret
  
end subroutine generate_secret

! 驗證輸入
subroutine validate_input(input_str, guess, valid)
  implicit none
  character(len=3), intent(in) :: input_str
  integer, intent(out) :: guess(3)
  logical, intent(out) :: valid
  integer :: i, j, digit
  
  valid = .true.
  
  ! 檢查長度
  if (len_trim(input_str) /= 3) then
    valid = .false.
    return
  end if
  
  ! 轉換字符為數字並檢查是否為數字
  do i = 1, 3
    digit = ichar(input_str(i:i)) - ichar('0')
    if (digit < 0 .or. digit > 9) then
      valid = .false.
      return
    end if
    guess(i) = digit
  end do
  
  ! 檢查是否有重複數字
  do i = 1, 3
    do j = i+1, 3
      if (guess(i) == guess(j)) then
        valid = .false.
        return
      end if
    end do
  end do
  
end subroutine validate_input

! 計算A和B的數量
subroutine calculate_ab(secret, guess, a_count, b_count)
  implicit none
  integer, intent(in) :: secret(3), guess(3)
  integer, intent(out) :: a_count, b_count
  integer :: i, j
  
  a_count = 0
  b_count = 0
  
  ! 計算A（位置和數字都正確）
  do i = 1, 3
    if (secret(i) == guess(i)) then
      a_count = a_count + 1
    end if
  end do
  
  ! 計算B（數字正確但位置錯誤）
  do i = 1, 3
    do j = 1, 3
      if (i /= j .and. secret(i) == guess(j)) then
        ! 確保這個數字在對應位置不是A
        if (secret(j) /= guess(j)) then
          b_count = b_count + 1
        end if
      end if
    end do
  end do
  
end subroutine calculate_ab
