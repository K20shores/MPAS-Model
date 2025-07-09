module mpas_runtime_vars

    implicit none

    private

    public :: mpas_var, mpas_var_array

    type mpas_var
        character(len=64) :: name = ''
        character(len=64) :: units = '-'
        character(len=64) :: description = 'unknown'
        character(len=64) :: dimensions = 'nVertLevels nCells Time'
        character(len=64) :: group = 'default'
        character(len=64) :: packages = ''
    end type mpas_var


    type mpas_var_array
        ! TBD
    contains
        procedure :: add_constituent
        procedure :: instantiate_in_pool
        final :: destroy_var_array
    end type mpas_var_array


contains


    subroutine add_constituent(self, constituent)

        class (mpas_var_array), intent(inout) :: self
        type (mpas_var), intent(in) :: constituent

        logical :: expected_species

        write(0,*) 'Adding constituent '//trim(constituent % name)

        expected_species = .false.
        if (constituent % name == 'o3') then
            expected_species = .true.
        endif
        if (constituent % name == 'no') then
            expected_species = .true.
        endif
        if (constituent % name == 'no2') then
            expected_species = .true.
        endif

        if (.not. expected_species) then
            write(0, *) 'This species was NOT expected'
        endif

    end subroutine add_constituent


    subroutine instantiate_in_pool(self, name, pool)

        class (mpas_var_array), intent(inout) :: self
        character(len=*), intent(in) :: name
        integer, intent(inout) :: pool


        write(0,*) 'Instantiating var_array'

    end subroutine instantiate_in_pool


    subroutine destroy_var_array(self)

        type (mpas_var_array), intent(inout) :: self


        write(0,*) 'Destroying var_array'

    end subroutine destroy_var_array

end module mpas_runtime_vars
