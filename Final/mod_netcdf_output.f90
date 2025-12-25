!===============================================================================
! Module: mod_netcdf_output.f90
! Purpose: Write simulation results to NetCDF files
!===============================================================================

module mod_netcdf_output
    use netcdf
    use mod_netcdf_io
    implicit none
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: create_output_netcdf
    ! Purpose: Create NetCDF file for output with dimensions and variables
    !---------------------------------------------------------------------------
    subroutine create_output_netcdf(filename, lon, lat, nx, ny, ncid, ierr)
        character(len=*), intent(in) :: filename
        real(8), intent(in) :: lon(:), lat(:)
        integer, intent(in) :: nx, ny
        integer, intent(out) :: ncid
        integer, intent(out) :: ierr
        
        integer :: lon_dimid, lat_dimid, time_dimid
        integer :: lon_varid, lat_varid, time_varid, eta_varid
        
        ierr = NC_SUCCESS
        
        ! Create NetCDF file
        ierr = nf90_create(filename, NF90_CLOBBER, ncid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error creating output file: ', trim(filename)
            ierr = NC_ERR_FILE
            return
        end if
        
        ! Define dimensions
        ierr = nf90_def_dim(ncid, 'lon', nx, lon_dimid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_def_dim(ncid, 'lat', ny, lat_dimid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_def_dim(ncid, 'time', NF90_UNLIMITED, time_dimid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Define coordinate variables
        ierr = nf90_def_var(ncid, 'lon', NF90_DOUBLE, lon_dimid, lon_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_def_var(ncid, 'lat', NF90_DOUBLE, lat_dimid, lat_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_def_var(ncid, 'time', NF90_DOUBLE, time_dimid, time_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Define eta (surface elevation) variable
        ierr = nf90_def_var(ncid, 'eta', NF90_DOUBLE, &
                           (/lon_dimid, lat_dimid, time_dimid/), eta_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Add attributes
        ierr = nf90_put_att(ncid, lon_varid, 'units', 'degrees_east')
        ierr = nf90_put_att(ncid, lon_varid, 'long_name', 'longitude')
        
        ierr = nf90_put_att(ncid, lat_varid, 'units', 'degrees_north')
        ierr = nf90_put_att(ncid, lat_varid, 'long_name', 'latitude')
        
        ierr = nf90_put_att(ncid, time_varid, 'units', 'seconds')
        ierr = nf90_put_att(ncid, time_varid, 'long_name', 'time since simulation start')
        
        ierr = nf90_put_att(ncid, eta_varid, 'units', 'meters')
        ierr = nf90_put_att(ncid, eta_varid, 'long_name', 'surface elevation')
        ierr = nf90_put_att(ncid, eta_varid, '_FillValue', -9999.0d0)
        
        ! Global attributes
        ierr = nf90_put_att(ncid, NF90_GLOBAL, 'title', 'Tsunami Simulation Results')
        ierr = nf90_put_att(ncid, NF90_GLOBAL, 'source', 'Fortran Tsunami Simulator')
        ierr = nf90_put_att(ncid, NF90_GLOBAL, 'Conventions', 'CF-1.6')
        
        ! End define mode
        ierr = nf90_enddef(ncid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_FILE
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Write coordinate arrays
        ierr = nf90_put_var(ncid, lon_varid, lon)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_put_var(ncid, lat_varid, lat)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = NC_SUCCESS
    end subroutine create_output_netcdf

    !---------------------------------------------------------------------------
    ! Subroutine: write_time_snapshot
    ! Purpose: Write one time snapshot to NetCDF file
    !---------------------------------------------------------------------------
    subroutine write_time_snapshot(ncid, eta, time, time_index, ierr)
        integer, intent(in) :: ncid
        real(8), intent(in) :: eta(:,:)
        real(8), intent(in) :: time
        integer, intent(in) :: time_index
        integer, intent(out) :: ierr
        
        integer :: time_varid, eta_varid
        integer :: start(3), count(3)
        
        ierr = NC_SUCCESS
        
        ! Get variable IDs
        ierr = nf90_inq_varid(ncid, 'time', time_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            return
        end if
        
        ierr = nf90_inq_varid(ncid, 'eta', eta_varid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            return
        end if
        
        ! Write time
        ierr = nf90_put_var(ncid, time_varid, time, start=(/time_index/))
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            return
        end if
        
        ! Write eta
        start = (/1, 1, time_index/)
        count = (/size(eta, 1), size(eta, 2), 1/)
        ierr = nf90_put_var(ncid, eta_varid, eta, start=start, count=count)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            return
        end if
        
        ierr = NC_SUCCESS
    end subroutine write_time_snapshot

    !---------------------------------------------------------------------------
    ! Subroutine: close_output_netcdf
    ! Purpose: Close output NetCDF file
    !---------------------------------------------------------------------------
    subroutine close_output_netcdf(ncid, ierr)
        integer, intent(in) :: ncid
        integer, intent(out) :: ierr
        
        ierr = nf90_close(ncid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_FILE
        else
            ierr = NC_SUCCESS
        end if
    end subroutine close_output_netcdf

end module mod_netcdf_output

