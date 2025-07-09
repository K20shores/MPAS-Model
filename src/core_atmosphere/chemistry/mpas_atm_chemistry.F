! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_atm_chemistry
!
!> \brief Manages interactions with chemistry packages
!> \author CheMPAS-A Developers
!> \date   8 July 2025
!> \details
!>  This module manages the interactions with chemistry packages,
!>  including initialization, time-stepping, and finalization.
!>  It provides a framework for integrating various chemistry models
!>  into the CheMPAS-A system.
!
!-------------------------------------------------------------------------
module mpas_atm_chemistry

    implicit none

    private

    public :: init_chemistry, step_chemistry, finalize_chemistry

    contains

    !------------------------------------------------------------------------
    !  routine mpas_atm_chemistry_init
    !
    !> \brief Initializes the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This routine initializes the chemistry packages, setting up
    !>  necessary parameters and data structures for the simulation.
    !------------------------------------------------------------------------
    subroutine init_chemistry(chemical_species, configs, number_of_grid_cells)

        use mpas_musica, only: musica_init
        use mpas_log, only : mpas_log_write
        use mpas_runtime_vars, only: mpas_var_array
        use mpas_derived_types, only: mpas_pool_type
        use mpas_kind_types, only: StrKIND
        use mpas_pool_routines, only: mpas_pool_get_config 

        type(mpas_var_array),            intent(inout)   :: chemical_species
        type (mpas_pool_type),           intent(in)      :: configs
        integer,                         intent(in)      :: number_of_grid_cells

        integer                           :: error_code
        character(len=:), allocatable     :: error_message
        character(len=StrKIND), pointer   :: filepath

        call mpas_pool_get_config(configs, 'config_micm_file', filepath)
        call mpas_log_write('Initializing chemistry packages...')
        call musica_init(filepath, chemical_species, number_of_grid_cells, error_code, error_message)

    end subroutine init_chemistry


    !------------------------------------------------------------------------
    !  routine mpas_atm_chemistry_step
    !
    !> \brief Steps the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This routine steps the chemistry packages, updating their state
    !>  based on the current simulation time and conditions.
    !------------------------------------------------------------------------
    subroutine step_chemistry()

        use mpas_musica, only: musica_step
        use mpas_log, only : mpas_log_write

        call mpas_log_write('Stepping chemistry packages...')
        call musica_step()

    end subroutine step_chemistry


    !------------------------------------------------------------------------
    !  routine mpas_atm_chemistry_finalize
    !
    !> \brief Finalizes the chemistry packages
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This routine finalizes the chemistry packages, cleaning up
    !>  any resources and data structures used during the simulation.
    !------------------------------------------------------------------------
    subroutine finalize_chemistry()

        use mpas_musica, only: musica_finalize
        use mpas_log, only : mpas_log_write

        call mpas_log_write('Finalizing chemistry packages...')
        call musica_finalize()

    end subroutine finalize_chemistry

end module mpas_atm_chemistry