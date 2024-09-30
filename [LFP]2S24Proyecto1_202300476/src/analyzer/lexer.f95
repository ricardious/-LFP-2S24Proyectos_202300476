module analyzerModule
    use tokenModule
    use errorModule
    implicit none

    integer, parameter :: MAX_TOKENS = 1000
    integer, parameter :: MAX_ERRORS = 1000

    type :: Analyzer
        integer :: state = 0
        logical :: isSaturation = .false.
        type(Token) :: tokens(MAX_TOKENS)
        type(Error) :: errors(MAX_ERRORS)
        integer :: line = 1
        integer :: column = 1
        integer :: iTokens = 0   
        integer :: iErrors = 0
        integer :: i = 1
        character(len=:), allocatable :: buffer ! Buffer to store characters as they are read

    contains
        procedure :: analyze
        procedure :: initializeState
        procedure :: initializeBuffer
        procedure :: addToken
        procedure :: addError
        procedure :: state0
        procedure :: state1
        procedure :: state2
        procedure :: state3
        procedure :: printTokens
        procedure :: printErrors
        procedure :: resetBufferAndState
        procedure :: create_report
    end type Analyzer

contains

    ! Initialize the state of the analyzer
    subroutine initializeState(this)
        class(Analyzer), intent(inout) :: this
        this%state = 0
    end subroutine initializeState

    ! Analyze the input string and generate tokens and errors
    subroutine analyze(this, input)
        class (Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: input
        integer :: length

        this%iTokens = 0
        this%iErrors = 0
        call this%initializeBuffer()

        length = len_trim(input)
        this%i = 1

        do while (this%i <= length)
            select case (this%state)
                case (0)
                    call this%state0(input(this%i:this%i))
                case (1)
                    call this%state1(input(this%i:this%i))
                case (2)
                    call this%state2(input(this%i:this%i))
                case (3)
                    call this%state3(input(this%i:this%i))
            end select
            this%i = this%i + 1
        end do
        print *, input
        call this%printTokens()
        call this%printErrors()

        call this%create_report("report.html")
    end subroutine analyze

    ! Add a token to the list of tokens
    subroutine addToken(this, name, lexeme, line, column)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: name, lexeme
        integer, intent(in) :: line, column
        type(Token) :: newToken

        if (this%iTokens < MAX_TOKENS) then
            call newToken%createToken(name, lexeme, line, column)
            this%tokens(this%iTokens + 1) = newToken
            this%iTokens = this%iTokens + 1
        else
            print *, "Error: Maximum number of tokens reached."
        end if
    end subroutine addToken

    ! Add an error to the list of errors
    subroutine addError(this, characterSymbol, description, line, column)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol, description
        integer, intent(in) :: line, column
        type(Error) :: newError

        if (this%iErrors < MAX_ERRORS) then
            call newError%createError(characterSymbol, description, line, column)
            this%errors(this%iErrors + 1) = newError
            this%iErrors = this%iErrors + 1
        else
            print *, "Error: Maximum number of errors reached."
        end if
    end subroutine addError

    ! State 0: Initial state
    subroutine state0(this, characterSymbol)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol

        select case (characterSymbol)
            case ('A':'Z', 'a':'z')
                this%buffer = this%buffer // characterSymbol
                this%column = this%column + 1
                this%state = 1
            case ('"')
                this%buffer = this%buffer // characterSymbol
                this%column = this%column + 1
                this%state = 2
            case ('0':'9')
                this%buffer = this%buffer // characterSymbol
                this%column = this%column + 1
                this%state = 3
            case (':')
                call this%addToken('COLON', characterSymbol, this%line, this%column)
                call this%initializeBuffer()
                this%column = this%column + 1
            case ('{')
                call this%addToken('OPEN_BRACE', characterSymbol, this%line, this%column)
                call this%initializeBuffer()
                this%column = this%column + 1
            case ('}')
                call this%addToken('CLOSE_BRACE', characterSymbol, this%line, this%column)
                call this%initializeBuffer()
                this%column = this%column + 1
            case ('%')
                call this%addToken('PERCENT', characterSymbol, this%line, this%column)
                call this%initializeBuffer()
                this%column = this%column + 1
            case (';')
                call this%addToken('SEMICOLON', characterSymbol, this%line, this%column)
                call this%initializeBuffer()
                this%column = this%column + 1
            case (char(10))
                this%line = this%line + 1
                this%column = 1
            case (' ')
                this%column = this%column + 1
            case (char(9))
                this%column = this%column + 1
            case default
                call this%addError(characterSymbol, "Invalid token", this%line, this%column)
                this%column = this%column + 1
        end select
    end subroutine state0

    ! State 1: Reading a keyword
    subroutine state1(this, characterSymbol)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol
        character(len=:), allocatable :: tempBuffer

        if (characterSymbol >= 'A' .and. characterSymbol <= 'Z' .or. &
            characterSymbol >= 'a' .and. characterSymbol <= 'z') then
            this%buffer = this%buffer // characterSymbol
            this%column = this%column + 1
        else
            tempBuffer = trim(this%buffer)
            select case (adjustl(tempBuffer))
                case ("Grafica", "Nombre", "Continente", "Pais", "Poblacion", "Bandera", "Saturacion")
                    call this%addToken("KEYWORD", tempBuffer, this%line, this%column)
                    if (adjustl(tempBuffer) == "Saturacion") then
                        this%isSaturation = .true.
                    end if
                case default
                    call this%addError(tempBuffer, "Invalid token", this%line, this%column)
            end select
            call this%resetBufferAndState()
            this%i = this%i - 1
        end if
    end subroutine state1

    ! State 2: Reading a string
    subroutine state2(this, characterSymbol)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol

        if (characterSymbol == '"') then
            this%buffer = this%buffer // characterSymbol
            call this%addToken("STRING", this%buffer, this%line, this%column)
            call this%initializeBuffer()
            this%column = this%column + 1
            this%state = 0
        else
            this%buffer = this%buffer // characterSymbol
            this%column = this%column + 1
        end if
    end subroutine state2

    ! State 3: Reading a number
    subroutine state3(this, characterSymbol)
        class(Analyzer), intent(inout) :: this
        character(len=*), intent(in) :: characterSymbol
        integer :: number

        if (characterSymbol >= '0' .and. characterSymbol <= '9') then
            this%buffer = this%buffer // characterSymbol
            this%column = this%column + 1
        else if (characterSymbol == ',') then
            this%column = this%column + 1
        else
            call removeCommas(this%buffer)
            read(this%buffer, *) number

            if (this%isSaturation) then
                if (number >= 0 .and. number <= 100) then
                    call this%addToken("PERCENTAGE", this%buffer, this%line, this%column)
                else
                    call this%addError(this%buffer, "Invalid percentage value (must be 0-100)", this%line, this%column)
                end if
                this%isSaturation = .false.
            else
                call this%addToken("NUMBER", this%buffer, this%line, this%column)
            end if

            call this%initializeBuffer()
            this%state = 0
            this%i = this%i - 1
        end if
    end subroutine state3

    ! Reset the buffer and state
    subroutine resetBufferAndState(this)
        class(Analyzer), intent(inout) :: this
        call this%initializeBuffer()
        this%state = 0
    end subroutine resetBufferAndState

    ! Initialize the buffer
    subroutine initializeBuffer(this)
        class(Analyzer), intent(inout) :: this
        if (.not. allocated(this%buffer)) then
            allocate(character(len=100) :: this%buffer)
        end if
        this%buffer = ''
    end subroutine initializeBuffer

    ! Print the tokens
    subroutine printTokens(this)
        class(Analyzer), intent(in) :: this
        integer :: i

        print *, "Printing tokens:"
        do i = 1, this%iTokens
            print "(A6, A15, A15, A25, A15, I3, A15, I3)", &
                "Token: ", trim(this%tokens(i)%name), &
                " Lexeme: ", trim(this%tokens(i)%lexeme), &
                " Line: ", this%tokens(i)%line, &
                " Column: ", this%tokens(i)%column
        end do
    end subroutine printTokens

    ! Print the errors
    subroutine printErrors(this)
        class(Analyzer), intent(in) :: this
        integer :: i

        if (this%iErrors > 0) then
            print *, "Printing errors:"
        end if
        do i = 1, this%iErrors
            print "(A7, A18, A8, A10, A7, I3, A8, I3)", &
                "Error: ", trim(this%errors(i)%description), &
                " Lexeme: ", trim(this%errors(i)%characterSymbol), &
                " Line: ", this%errors(i)%line, &
                " Column: ", this%errors(i)%column
        end do
    end subroutine printErrors

    ! Remove commas from a string (for number parsing)
    subroutine removeCommas(buffer)
        character(len=*), intent(inout) :: buffer
        character(len=len(buffer)) :: tempBuffer
        integer :: i, j

        j = 1
        tempBuffer = ""

        do i = 1, len_trim(buffer)
            if (buffer(i:i) /= ',') then
                tempBuffer(j:j) = buffer(i:i)
                j = j + 1
            end if
        end do

        buffer = trim(tempBuffer)
    end subroutine removeCommas
    
    ! Create an HTML report of the analysis
    subroutine create_report(this, file)
        class(Analyzer), intent(in) :: this
        character(len=*), intent(in) :: file
        integer :: i, iosx
        integer :: unit = 12 ! Unit number for the file

        ! Open the file for writing
        open(unit=unit, file=file, iostat=iosx, status="replace", action="write")
        if ( iosx /= 0 ) stop "Error saving report"

        ! Write the HTML header
        write(unit, '(A)') "<html>"
        write(unit, '(A)') "<head>"
        write(unit, '(A)') "<title>Analysis Report</title>"

        ! Write the CSS for the table and styles
        write(unit, '(A)') "<style>"
        write(unit, '(A)') "body {"
        write(unit, '(A)') "  font-family: Arial, sans-serif;"
        write(unit, '(A)') "  background-image: url('https://img.freepik.com/free-vector/gradient-black-background-with-cubes_23-2149177090.jpg?t=st=1727633629~exp=1727637229~hmac=317841991e7950d842a5c84fbba964ca9516e0c5f4d1353dfc58dd3b60c45fc1&w=1060');"
        write(unit, '(A)') "  background-size: cover;"  ! Ensure the background covers the whole page
        write(unit, '(A)') "  background-position: center;"
        write(unit, '(A)') "  background-repeat: no-repeat;"
        write(unit, '(A)') "  background-attachment: fixed;"  ! Keep the background fixed while scrolling
        write(unit, '(A)') "  color: white;" 
        write(unit, '(A)') "}"

        write(unit, '(A)') "table {"
        write(unit, '(A)') "  width: 100%;"
        write(unit, '(A)') "  max-width: 700px;"
        write(unit, '(A)') "  margin: auto;"
        write(unit, '(A)') "  backdrop-filter: blur(30px);"  ! Adjust blur effect
        write(unit, '(A)') "  border-collapse: collapse;"  ! Merge borders for cleaner look
        write(unit, '(A)') "  border-radius: 10px;"
        write(unit, '(A)') "  overflow: hidden;"
        write(unit, '(A)') "  border: 3px solid rgba(255, 255, 255, 0.5);"  ! Adjust background color and opacity
        write(unit, '(A)') "}"

        write(unit, '(A)') "th, td {"
        write(unit, '(A)') "  padding: 10px;"
        write(unit, '(A)') "  text-align: left;"
        write(unit, '(A)') "  border: 1px solid rgba(255, 255, 255, 0.3);"
        write(unit, '(A)') "}"

        write(unit, '(A)') "th {"
        write(unit, '(A)') "  font-weight: bold;"
        write(unit, '(A)') "  background: rgba(255, 255, 255, 0.1);"
        write(unit, '(A)') "  border-bottom: 2px solid rgba(255, 255, 255, 0.5);"
        write(unit, '(A)') "}"

        write(unit, '(A)') "tr:hover {"
        write(unit, '(A)') "  background-color: rgba(255, 255, 255, 0.1);"  ! Highlight row on hover
        write(unit, '(A)') "}"

        write(unit, '(A)') "h1 {"
        write(unit, '(A)') "  text-align: center;"
        write(unit, '(A)') "  margin-bottom: 20px;"
        write(unit, '(A)') "}"

        write(unit, '(A)') "</style>"

        write(unit, '(A)') "</head>"
        write(unit, '(A)') "<body>"

        if (this%iErrors > 0) then
            write(unit, '(A)') "<h1>Error Analysis Report</h1>"
            write(unit, '(A)') "<table>"
            write(unit, '(A)') "<thead><tr><th>No</th><th>Error</th><th>Description</th><th>Line</th><th>Column</th></tr></thead>"
            write(unit, '(A)') "<tbody>"

            ! Fill the table with error data
            do i = 1, this%iErrors
                write(unit, '(A, I0, A, A, A, A, A, I0, A, I0, A)') "<tr><td>", i, "</td><td>", trim(this%errors(i)%characterSymbol), "</td><td>", &
                trim(this%errors(i)%description), "</td><td>", this%errors(i)%line, "</td><td>", this%errors(i)%column, "</td></tr>"
            end do
            write(unit, '(A)') "</tbody>"
        else
            write(unit, '(A)') "<h1>Clean Analysis Report</h1>"
            write(unit, '(A)') "<table>"
            write(unit, '(A)') "<thead><tr><th>No</th><th>Lexeme</th><th>Token</th><th>Line</th><th>Column</th></tr></thead>"
            write(unit, '(A)') "<tbody>"

            ! Fill the table with token data
            do i = 1, this%iTokens
                write(unit, '(A, I0, A, A, A, A, A, I0, A, I0, A)') "<tr><td>", i, "</td><td>", trim(this%tokens(i)%lexeme), "</td><td>", &
                trim(this%tokens(i)%name), "</td><td>", this%tokens(i)%line, "</td><td>", this%tokens(i)%column, "</td></tr>"
            end do
            write(unit, '(A)') "</tbody>"
        end if

        ! Close the table and HTML tags
        write(unit, '(A)') "</table>"
        write(unit, '(A)') "</body>"
        write(unit, '(A)') "</html>"

        ! Close the file
        close(unit)
        
    end subroutine create_report

end module analyzerModule