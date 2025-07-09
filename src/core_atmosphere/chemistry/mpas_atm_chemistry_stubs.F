! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!------------------------------------------------------------------------
!  mpas_atm_chemistry_stubs
!
!> \brief Stubs for the chemistry package interface
!> \author CheMPAS-A Developers
!> \date   8 July 2025
!> \details
!>  This module provides stubs for the chemistry package interface,
!>  allowing for the registration of scalars and other interactions
!>  with the chemistry packages without implementing the full functionality.
!-------------------------------------------------------------------------
module mpas_atm_chemistry_stubs

    use iso_fortran_env, only: real64

    implicit none

    private

    public :: scalar_t, scalar_set_t

    type :: scalar_t
        character(len=:), allocatable :: name
        character(len=:), allocatable :: units
        character(len=:), allocatable :: description
        real(real64) :: default_value = 0.0_real64
        real(real64) :: minimum_value = 0.0_real64
        logical :: is_advected = .false.
    end type scalar_t

    type :: scalar_set_t
        type(scalar_t), allocatable :: scalars(:)
    contains
        procedure :: add => scalar_set_add_scalar
        procedure :: get => scalar_set_get_scalar
    end type scalar_set_t

contains

    subroutine scalar_set_add_scalar(self, scalar)
        class(scalar_set_t), intent(inout) :: self
        class(scalar_t), intent(in) :: scalar
        
        ! Intel hates this, so we probably have to do it the long way
        ! Also add logic to prevent duplicates and check for consistent units
        if (.not. allocated(self%scalars)) then
            self%scalars = [scalar]
        else
            self%scalars = [self%scalars, scalar]
        end if
        print *, "Scalar added:", scalar%name
    end subroutine scalar_set_add_scalar

    type(scalar_t) function scalar_set_get_scalar(self, name, units)
        class(scalar_set_t), intent(in) :: self
        character(len=*), intent(in) :: name
        character(len=*), intent(in) :: units
        
        integer :: i
        logical :: found = .false.

        do i = 1, size(self%scalars)
            if (self%scalars(i)%name == name .and. self%scalars(i)%units == units) then
                found = .true.
                print *, "Scalar found:", self%scalars(i)%name, "with value:", self%scalars(i)%default_value
                scalar_set_get_scalar = self%scalars(i)
                return
            else if (self%scalars(i)%name == name) then
                print *, "Warning: Scalar with name", name, "found but units do not match. Expected:", units, "but found:", self%scalars(i)%units
                return
            end if
        end do
        print *, "Scalar not found:", name, "with units:", units

    end function scalar_set_get_scalar

end module mpas_atm_chemistry_stubs