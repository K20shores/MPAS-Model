! Copyright (c) 2025 The University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at https://mpas-dev.github.io/license.html .
!
!-----------------------------------------------------------------------
!  mpas_musica
!
!> \brief Manages interactions with the MUSICA chemistry package
!> \author CheMPAS-A Developers
!> \date   8 July 2025
!> \details
!>  This module manages the interactions with the MUSICA chemistry package,
!>  including initialization, time-stepping, and finalization for
!>  MICM (ODE solver) and TUV-x (photolysis rate constant calculator).
!
!--------------------------------------------------------------------------
module mpas_musica

    use musica_micm,  only: micm_t
    use musica_state, only: state_t

    implicit none
    
    private

    public :: musica_init, musica_step, musica_finalize

    type(micm_t),  pointer :: micm  => null ( )  ! Pointer to the MICM ODE solver instance
    type(state_t), pointer :: state => null ( )  ! Pointer to the state of the MICM solver

    contains

    !------------------------------------------------------------------------
    !  subroutine musica_init
    !
    !> \brief Initializes the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This subroutine initializes the MUSICA chemistry package,
    !>  setting up necessary parameters and data structures for the simulation.
    !>  For now, we will load fixed configurations for MICM and TUV-x.
    !>  Later, we will gradually replace the fixed configuration elements
    !>  with runtime updates using the MUSICA API
    !------------------------------------------------------------------------
    subroutine musica_init(filename_of_micm_configuration, var_array, number_of_grid_cells, error_code, &
            error_message)

        use mpas_runtime_vars, only : mpas_var_array, mpas_var
        use musica_micm, only : RosenbrockStandardOrder
        use musica_util, only : error_t, string_t

        use mpas_log, only : mpas_log_write

        type(mpas_var_array),          intent(inout) :: var_array
        integer,                       intent(in)    :: number_of_grid_cells
        integer,                       intent(out)   :: error_code
        character(len=:), allocatable, intent(out)   :: error_message
        character(len=*),              intent(in)    :: filename_of_micm_configuration

        type(error_t)  :: error
        type(mpas_var) :: constituent
        type(string_t) :: description
        integer :: i_species

        ! TEMPORARY: Hard-coded options for the MICM solver
        integer :: solver_type = RosenbrockStandardOrder

        call mpas_log_write('Initializing MUSICA chemistry package...')

        micm => micm_t(trim(filename_of_micm_configuration), solver_type, error)
        if (has_error_occurred(error, error_message, error_code)) return

        state => micm%get_state(number_of_grid_cells, error)
        if (has_error_occurred(error, error_message, error_code)) return

        ! Create scalar variables for chemical species
        constituent%group = "chemistry_species"
        constituent%packages = "MUSICA"
        associate( map => state%species_ordering )
        do i_species = 1, map%size( )
            constituent%name = map%name(i_species)
            constituent%units = 'kg kg-1'
            description = micm%get_species_property_string( &
                constituent%name, "__description", error)
            if (has_error_occurred(error, error_message, error_code)) return
            constituent%description = description%value_
            call var_array%add_constituent(constituent)
        end do
        end associate

    end subroutine musica_init

    !------------------------------------------------------------------------
    !  subroutine musica_step
    !
    !> \brief Steps the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This subroutine steps the MUSICA chemistry package, updating its state
    !>  based on the current simulation time and conditions.
    !>  It first calls the TUV-x package to compute photolysis rates,
    !>  then calls the MICM package to solve the ODEs for chemical species
    !>  concentrations.
    !------------------------------------------------------------------------
    subroutine musica_step()

        use mpas_log, only : mpas_log_write

        call mpas_log_write('Stepping MUSICA chemistry package...')

        ! Here we would typically call the TUV-x and MICM packages to perform
        ! the necessary computations, but for now we will just log the step.

    end subroutine musica_step

    !------------------------------------------------------------------------
    !  subroutine musica_finalize
    !
    !> \brief Finalizes the MUSICA chemistry package
    !> \author CheMPAS-A Developers
    !> \date   8 July 2025
    !> \details
    !>  This subroutine finalizes the MUSICA chemistry package,
    !>  cleaning up any resources and data structures used during the simulation.
    !-------------------------------------------------------------------------
    subroutine musica_finalize()

        use mpas_log, only : mpas_log_write

        call mpas_log_write('Finalizing MUSICA chemistry package...')

        ! Here we would typically clean up resources, but for now we do nothing.

    end subroutine musica_finalize

    !-------------------------------------------------------------------------
    !  function has_error_occurred
    !
    !> @brief Evaluate a MUSICA error for failure and convert to error data
    !> @param[in] error The error code to evaluate and convert.
    !> @param[out] error_message The error message.
    !> @param[out] error_code The error code.
    !> @return True for an error, false for success.
    !-------------------------------------------------------------------------
    logical function has_error_occurred(error, error_message, error_code)
        use musica_util, only: error_t

        type(error_t),                 intent(in)  :: error
        character(len=:), allocatable, intent(out) :: error_message
        integer,                       intent(out) :: error_code

        character(len=30) :: error_code_str

        if ( error%is_success( ) ) then
          error_code = 0
          error_message = ''
          has_error_occurred = .false.
          return
        end if
        error_code = error%code( )
        write(error_code_str, '(I30)') error%code( )
        error_message = '[MUSICA Error]: ' // error%category( ) // '[' // &
                        trim( adjustl( error_code_str ) ) // ']: ' // error%message( )
        has_error_occurred = .true.

    end function has_error_occurred

end module mpas_musica
