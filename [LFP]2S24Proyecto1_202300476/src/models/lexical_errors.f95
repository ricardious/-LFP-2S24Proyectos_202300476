module errorModule
    implicit none

    ! Type definition for Error with attributes for character, description, line, and column
    type :: Error
        character(len=:), allocatable :: characterSymbol  ! The character causing the error
        character(len=:), allocatable :: description      ! Error description
        integer :: line, column                           ! Line and column where the error occurred

    contains
        procedure :: createError
    end type Error

contains

    ! Subroutine to create and initialize an Error object
    subroutine createError(this, characterSymbol, description, line, column)
        class(Error), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol  ! The character that caused the error
        character(len=*), intent(in) :: description      ! The description of the error
        integer, intent(in) :: line                      ! The line where the error occurred
        integer, intent(in) :: column                    ! The column where the error occurred

        ! Assigning the values to the Error object attributes
        this%characterSymbol = characterSymbol
        this%description = description
        this%line = line
        this%column = column
    end subroutine createError

end module errorModule
