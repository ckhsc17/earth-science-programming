! 3. 小寫字母轉大寫
program lowercase_to_uppercase
    implicit none
    character(len=1) :: ch
    integer :: code

    print *, '請輸入一個小寫字母:'
    read(*,*) ch

    code = ichar(ch)   ! 把字元轉成整數碼
    if (code >= ichar('a') .and. code <= ichar('z')) then
        print *, '大寫字母:', achar(code - 32)
    else
        print *, '輸入不是小寫字母'
    end if
end program lowercase_to_uppercase

