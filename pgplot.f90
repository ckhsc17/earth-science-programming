program test

integer pgopen

istat=PGOPEN('test.ps/vcps') !PostScript

!-- /c color /v vertical

if(istat<=0)stop 'ERR opening for PS file!'

call pgslw(2)

call pgenv(0.0,6.0,0.0,8.0,1,2) !just,iaxis

call pgmove(0.0,0.0)

call pgdraw(6.0,8.0)

call pgend

end program test