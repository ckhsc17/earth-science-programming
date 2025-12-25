!===============================================================================
! Module: mod_netcdf_io.f90
! Purpose: Read GEBCO NetCDF bathymetry data for tsunami simulation
! Author: Generated for Tsunami Simulation Project
!===============================================================================

module mod_netcdf_io
    use netcdf
    implicit none
    
    ! Error codes
    integer, parameter :: NC_SUCCESS = 0
    integer, parameter :: NC_ERR_FILE = -1
    integer, parameter :: NC_ERR_VAR = -2
    integer, parameter :: NC_ERR_DIM = -3
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: get_netcdf_info
    ! Purpose: Get basic information about the NetCDF file
    !---------------------------------------------------------------------------
    subroutine get_netcdf_info(filename, nx, ny, lon_min, lon_max, lat_min, lat_max, ierr)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: nx, ny
        real(8), intent(out) :: lon_min, lon_max, lat_min, lat_max
        integer, intent(out) :: ierr
        
        integer :: ncid, lon_dimid, lat_dimid, lon_varid, lat_varid
        integer :: lon_size, lat_size
        real(8), allocatable :: lon_temp(:), lat_temp(:)
        
        ierr = NC_SUCCESS
        
        ! Open NetCDF file
        ierr = nf90_open(filename, NF90_NOWRITE, ncid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error opening file: ', trim(filename)
            ierr = NC_ERR_FILE
            return
        end if
        
        ! Get dimension IDs
        ierr = nf90_inq_dimid(ncid, 'lon', lon_dimid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: dimension "lon" not found'
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inq_dimid(ncid, 'lat', lat_dimid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: dimension "lat" not found'
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Get dimension sizes
        ierr = nf90_inquire_dimension(ncid, lon_dimid, len=lon_size)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inquire_dimension(ncid, lat_dimid, len=lat_size)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        nx = lon_size
        ny = lat_size
        
        ! Get coordinate arrays
        allocate(lon_temp(nx), lat_temp(ny))
        
        ierr = nf90_inq_varid(ncid, 'lon', lon_varid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: variable "lon" not found'
            ierr = NC_ERR_VAR
            deallocate(lon_temp, lat_temp)
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inq_varid(ncid, 'lat', lat_varid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: variable "lat" not found'
            ierr = NC_ERR_VAR
            deallocate(lon_temp, lat_temp)
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Read coordinate arrays
        ierr = nf90_get_var(ncid, lon_varid, lon_temp)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            deallocate(lon_temp, lat_temp)
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_get_var(ncid, lat_varid, lat_temp)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_VAR
            deallocate(lon_temp, lat_temp)
            ierr = nf90_close(ncid)
            return
        end if
        
        ! Get min/max values
        lon_min = minval(lon_temp)
        lon_max = maxval(lon_temp)
        lat_min = minval(lat_temp)
        lat_max = maxval(lat_temp)
        
        deallocate(lon_temp, lat_temp)
        ierr = nf90_close(ncid)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_FILE
            return
        end if
        
        ierr = NC_SUCCESS
    end subroutine get_netcdf_info

    !---------------------------------------------------------------------------
    ! Subroutine: read_gebco_bathymetry
    ! Purpose: Read GEBCO elevation data from NetCDF file
    ! Note: GEBCO uses (lat, lon) order, but we output as (nx, ny) for convenience
    !---------------------------------------------------------------------------
    subroutine read_gebco_bathymetry(filename, elevation, lon, lat, nx, ny, ierr)
        character(len=*), intent(in) :: filename
        real(8), allocatable, intent(inout) :: elevation(:,:)
        real(8), allocatable, intent(inout) :: lon(:), lat(:)
        integer, intent(out) :: nx, ny
        integer, intent(out) :: ierr
        
        integer :: ncid, elev_varid, lon_varid, lat_varid
        integer :: lon_dimid, lat_dimid
        integer(2), allocatable :: elev_int16(:,:)  ! GEBCO uses int16
        integer :: lon_size, lat_size
        integer :: ierr_tmp
        
        ierr = NC_SUCCESS
        
        ! Open NetCDF file
        ierr = nf90_open(filename, NF90_NOWRITE, ncid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error opening file: ', trim(filename)
            ierr = NC_ERR_FILE
            return
        end if
        
        ! Get dimension sizes
        ierr = nf90_inq_dimid(ncid, 'lon', lon_dimid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: dimension "lon" not found'
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inq_dimid(ncid, 'lat', lat_dimid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: dimension "lat" not found'
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inquire_dimension(ncid, lon_dimid, len=lon_size)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inquire_dimension(ncid, lat_dimid, len=lat_size)
        if (ierr /= nf90_noerr) then
            ierr = NC_ERR_DIM
            ierr = nf90_close(ncid)
            return
        end if
        
        nx = lon_size
        ny = lat_size
        
        ! Deallocate output arrays if already allocated
        if (allocated(elevation)) deallocate(elevation)
        if (allocated(lon)) deallocate(lon)
        if (allocated(lat)) deallocate(lat)
        
        ! Allocate output arrays directly
        allocate(elevation(nx, ny))
        allocate(lon(nx))
        allocate(lat(ny))
        allocate(elev_int16(nx, ny))  ! Fortran sees (lon, lat)
        
        ! Get variable IDs
        ierr = nf90_inq_varid(ncid, 'elevation', elev_varid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: variable "elevation" not found'
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inq_varid(ncid, 'lon', lon_varid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: variable "lon" not found'
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_inq_varid(ncid, 'lat', lat_varid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: variable "lat" not found'
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ! Read coordinate arrays
        ierr = nf90_get_var(ncid, lon_varid, lon)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: failed to read "lon" variable: ', trim(nf90_strerror(ierr))
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ierr = nf90_get_var(ncid, lat_varid, lat)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: failed to read "lat" variable: ', trim(nf90_strerror(ierr))
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ! Read elevation data (as int16, then convert to real)
        ! Note: NetCDF stores as (lat, lon) = (ny, nx), but we want (lon, lat) = (nx, ny)
        ierr = nf90_get_var(ncid, elev_varid, elev_int16)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Error: failed to read "elevation" variable: ', trim(nf90_strerror(ierr))
            ierr = NC_ERR_VAR
            deallocate(elevation, lon, lat, elev_int16)
            ierr_tmp = nf90_close(ncid)
            return
        end if
        
        ! Convert from int16 to real(8)
        elevation = real(elev_int16, kind=8)
        
        deallocate(elev_int16)
        ierr = nf90_close(ncid)
        if (ierr /= nf90_noerr) then
            write(*,*) 'Warning: nf90_close returned error: ', ierr
        end if
        
        ierr = NC_SUCCESS
    end subroutine read_gebco_bathymetry

end module mod_netcdf_io

