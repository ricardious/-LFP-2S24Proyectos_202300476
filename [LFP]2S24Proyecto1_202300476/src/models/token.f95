module tokenModule
    implicit none

    ! Define the Token type
    type :: Token
        character(len=:), allocatable :: name     ! Token name (e.g., type of token)
        character(len=:), allocatable :: lexeme   ! Lexeme (the actual string value)
        integer :: line                           ! Line number where the token was found
        integer :: column                         ! Column number where the token was found
    contains
        procedure :: createToken                  ! Procedure to initialize a token
    end type Token

contains

    ! Subroutine to create and initialize a token
    subroutine createToken(this, name, lexeme, line, column)
        class(Token), intent(inout) :: this       ! The token object to be initialized
        character(len=*), intent(in) :: name      ! Name/type of the token
        character(len=*), intent(in) :: lexeme    ! Lexeme string
        integer, intent(in) :: line               ! Line number of the token
        integer, intent(in) :: column             ! Column number of the token

        ! Initialize the token fields
        this%name = name
        this%lexeme = lexeme
        this%line = line
        this%column = column
    end subroutine createToken

end module tokenModule
