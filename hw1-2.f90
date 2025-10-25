! 2. 輸入字串，輸出長度及 a 位置
program string_a_position
  character(len=100) :: str
  integer :: i, str_len, pos
  print *, '請輸入一個包含 a 的字串:'
  read '(A)', str
  str_len = len_trim(str)
  pos = 0
  do i = 1, str_len
    if (str(i:i) == 'a') then
      pos = i
      exit
    end if
  end do
  print *, '字串長度:', str_len
  if (pos > 0) then
    print *, 'a 的位置:', pos
  else
    print *, '字串中沒有 a'
  end if
end program string_a_position