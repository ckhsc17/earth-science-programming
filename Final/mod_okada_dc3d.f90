!===============================================================================
! Module: mod_okada_dc3d.f90
! Purpose: Calculate initial seafloor displacement using Okada (1985) DC3D
!          Finite fault implementation (integrates over rectangular fault plane)
! Reference: Okada, Y. (1985). Surface deformation due to shear and tensile 
!            faults in a half-space. BSSA, 75(4), 1135-1154.
! Note: This module uses the complete DC3D formulation for finite faults,
!       providing more accurate results than the simplified point-source approximation
!       The DC3D subroutines are in DC3D.txt and are compiled separately
!===============================================================================

module mod_okada_dc3d
    ! Reuse the fault_params_type from mod_okada
    use mod_okada, only: fault_params_type, PI, DEG2RAD, RAD2DEG, ALPHA, read_fault_params
    implicit none
    
contains

    !---------------------------------------------------------------------------
    ! Subroutine: compute_initial_displacement_dc3d
    ! Purpose: Compute initial seafloor displacement on grid using DC3D (finite fault)
    !---------------------------------------------------------------------------
    subroutine compute_initial_displacement_dc3d(lon, lat, nx, ny, fault, displacement, ierr)
        real(8), intent(in) :: lon(:), lat(:)
        integer, intent(in) :: nx, ny
        type(fault_params_type), intent(in) :: fault
        real(8), intent(out) :: displacement(nx, ny)
        integer, intent(out) :: ierr
        
        integer :: i, j, iret
        real(8) :: dx, dy
        real(8) :: lon_obs, lat_obs
        real(8) :: R_earth
        real(8) :: x_rot, y_rot, strike_rad
        real(8) :: uz
        real(8) :: alpha_val, depth_val, dip_val
        real(8) :: al1, al2, aw1, aw2
        real(8) :: disl1, disl2, disl3
        real(4) :: alpha_r4, x_r4, y_r4, z_r4, depth_r4, dip_r4
        real(4) :: al1_r4, al2_r4, aw1_r4, aw2_r4
        real(4) :: disl1_r4, disl2_r4, disl3_r4
        real(4) :: ux_r4, uy_r4, uz_r4
        real(4) :: uxx_r4, uyx_r4, uzx_r4, uxy_r4, uyy_r4, uzy_r4
        real(4) :: uxz_r4, uyz_r4, uzz_r4
        
        ! External DC3D subroutine (from DC3D.txt)
        external :: DC3D
        
        R_earth = 6371000.0d0  ! Earth radius in meters
        ierr = 0
        
        ! Initialize displacement
        displacement = 0.0d0
        
        ! Prepare DC3D parameters (convert from fault_params_type)
        alpha_val = ALPHA
        depth_val = fault%depth
        dip_val = fault%dip
        
        ! Fault geometry in local fault coordinate system
        ! DC3D uses ranges [AL1, AL2] x [AW1, AW2]
        al1 = -fault%length / 2.0d0  ! Left edge
        al2 =  fault%length / 2.0d0  ! Right edge
        aw1 = 0.0d0                   ! Top edge (at depth)
        aw2 = fault%width             ! Bottom edge (deeper)
        
        ! Convert slip to dislocation components
        ! DISL1 = strike-slip, DISL2 = dip-slip, DISL3 = tensile (opening)
        disl1 = fault%slip * cos(fault%rake * DEG2RAD)  ! Strike-slip
        disl2 = fault%slip * sin(fault%rake * DEG2RAD)  ! Dip-slip
        disl3 = 0.0d0                                    ! No tensile component
        
        ! Compute displacement at each grid point
        strike_rad = fault%strike * DEG2RAD
        do j = 1, ny
            do i = 1, nx
                lon_obs = lon(i)
                lat_obs = lat(j)
                
                ! Convert lat/lon to meters relative to fault center
                dx = (lon_obs - fault%lon_center) * DEG2RAD * R_earth * cos(fault%lat_center * DEG2RAD)
                dy = (lat_obs - fault%lat_center) * DEG2RAD * R_earth
                
                ! Rotate to fault coordinate system (strike direction)
                x_rot = dx * cos(strike_rad) + dy * sin(strike_rad)
                y_rot = -dx * sin(strike_rad) + dy * cos(strike_rad)
                
                ! Convert to REAL*4 for DC3D (as required by original Okada code)
                alpha_r4 = real(alpha_val, 4)
                x_r4 = real(x_rot, 4)
                y_r4 = real(y_rot, 4)
                z_r4 = 0.0  ! Observation at surface
                depth_r4 = real(depth_val, 4)
                dip_r4 = real(dip_val, 4)
                al1_r4 = real(al1, 4)
                al2_r4 = real(al2, 4)
                aw1_r4 = real(aw1, 4)
                aw2_r4 = real(aw2, 4)
                disl1_r4 = real(disl1, 4)
                disl2_r4 = real(disl2, 4)
                disl3_r4 = real(disl3, 4)
                
                ! Call DC3D subroutine (finite fault integration)
                call DC3D(alpha_r4, x_r4, y_r4, z_r4, depth_r4, dip_r4, &
                         al1_r4, al2_r4, aw1_r4, aw2_r4, &
                         disl1_r4, disl2_r4, disl3_r4, &
                         ux_r4, uy_r4, uz_r4, &
                         uxx_r4, uyx_r4, uzx_r4, &
                         uxy_r4, uyy_r4, uzy_r4, &
                         uxz_r4, uyz_r4, uzz_r4, iret)
                
                ! Convert back to REAL*8
                uz = real(uz_r4, 8)
                
                ! Store vertical displacement
                if (iret == 0) then
                    displacement(i, j) = uz
                else
                    displacement(i, j) = 0.0d0  ! Singular case
                end if
            end do
        end do
        
        write(*,*) 'Initial displacement computed (DC3D finite fault):'
        write(*,*) '  Min: ', minval(displacement), ' m'
        write(*,*) '  Max: ', maxval(displacement), ' m'
        write(*,*) '  Mean: ', sum(displacement) / real(nx * ny, kind=8), ' m'
    end subroutine compute_initial_displacement_dc3d

end module mod_okada_dc3d
