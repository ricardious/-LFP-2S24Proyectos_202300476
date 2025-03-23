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
      procedure :: select_best_country
   end type Analyzer

contains

   ! Initialize the state of the analyzer
   subroutine initializeState(this)
      class(Analyzer), intent(inout) :: this
      this%state = 0
   end subroutine initializeState

   ! Analyze the input string and generate tokens and errors
   subroutine analyze(this, input)
      class(Analyzer), intent(inout) :: this
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
      call this%create_report()
      call this%select_best_country
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
         this%buffer = this%buffer//characterSymbol
         this%column = this%column + 1
         this%state = 1
      case ('"')
         this%buffer = this%buffer//characterSymbol
         this%column = this%column + 1
         this%state = 2
      case ('0':'9')
         this%buffer = this%buffer//characterSymbol
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
         this%buffer = this%buffer//characterSymbol
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
         this%buffer = this%buffer//characterSymbol
         call this%addToken("STRING", this%buffer, this%line, this%column)
         call this%initializeBuffer()
         this%column = this%column + 1
         this%state = 0
      else
         this%buffer = this%buffer//characterSymbol
         this%column = this%column + 1
      end if
   end subroutine state2

   ! State 3: Reading a number
   subroutine state3(this, characterSymbol)
      class(Analyzer), intent(inout) :: this
      character(len=*), intent(in) :: characterSymbol
      integer :: number

      if (characterSymbol >= '0' .and. characterSymbol <= '9') then
         this%buffer = this%buffer//characterSymbol
         this%column = this%column + 1
      else if (characterSymbol == ',') then
         this%column = this%column + 1
      else
         call removeCommas(this%buffer)
         read (this%buffer, *) number

         if (this%isSaturation) then
            if (number >= 0 .and. number <= 100) then
               call this%addToken("SATURATION", this%buffer, this%line, this%column)
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
         allocate (character(len=100) :: this%buffer)
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
   subroutine create_report(this)
      class(Analyzer), intent(in) :: this
      character(len=100) :: file
      integer :: i, iosx
      integer :: unit = 12 ! Unit numbtimestamper for the file
      character(8)  :: date ! Variable to store the current date (8 characters, e.g., 'YYYYMMDD')
      character(6) :: time ! Variable to store the current time (6 characters, e.g., 'HHMMSS')

      ! Call the intrinsic subroutine to get the current date and time
      call date_and_time(DATE=date, TIME=time)

      ! Construct the output file path using the date and time
      file = "./data/output/report_"//trim(adjustl(date))//"_"//trim(adjustl(time))//".html"
      ! The file name will look something like 'report_YYYYMMDD_HHMMSS.html'

      ! Open the file for writing
      open (unit=unit, file=file, iostat=iosx, status="new", action="write")
      if (iosx /= 0) stop "Error saving report"

      ! Write the HTML header
      write (unit, '(A)') "<html>"
      write (unit, '(A)') "<head>"
      write (unit, '(A)') "<title>Analysis Report</title>"

      ! Write the CSS for the table and styles
      write (unit, '(A)') "<style>"
      write (unit, '(A)') "body {"
      write (unit, '(A)') "  font-family: Arial, sans-serif;"
        write(unit, '(A)') "  background-image: url('https://img.freepik.com/free-vector/gradient-black-background-with-cubes_23-2149177090.jpg?t=st=1727633629~exp=1727637229~hmac=317841991e7950d842a5c84fbba964ca9516e0c5f4d1353dfc58dd3b60c45fc1&w=1060');"
      write (unit, '(A)') "  background-size: cover;"  ! Ensure the background covers the whole page
      write (unit, '(A)') "  background-position: center;"
      write (unit, '(A)') "  background-repeat: no-repeat;"
      write (unit, '(A)') "  background-attachment: fixed;"  ! Keep the background fixed while scrolling
      write (unit, '(A)') "  color: white;"
      write (unit, '(A)') "}"

      write (unit, '(A)') "table {"
      write (unit, '(A)') "  width: 100%;"
      write (unit, '(A)') "  max-width: 700px;"
      write (unit, '(A)') "  margin: auto;"
      write (unit, '(A)') "  backdrop-filter: blur(30px);"  ! Adjust blur effect
      write (unit, '(A)') "  border-collapse: collapse;"  ! Merge borders for cleaner look
      write (unit, '(A)') "  border-radius: 10px;"
      write (unit, '(A)') "  overflow: hidden;"
      write (unit, '(A)') "  border: 3px solid rgba(255, 255, 255, 0.5);"  ! Adjust background color and opacity
      write (unit, '(A)') "}"

      write (unit, '(A)') "th, td {"
      write (unit, '(A)') "  padding: 10px;"
      write (unit, '(A)') "  text-align: left;"
      write (unit, '(A)') "  border: 1px solid rgba(255, 255, 255, 0.3);"
      write (unit, '(A)') "}"

      write (unit, '(A)') "th {"
      write (unit, '(A)') "  font-weight: bold;"
      write (unit, '(A)') "  background: rgba(255, 255, 255, 0.1);"
      write (unit, '(A)') "  border-bottom: 2px solid rgba(255, 255, 255, 0.5);"
      write (unit, '(A)') "}"

      write (unit, '(A)') "tr:hover {"
      write (unit, '(A)') "  background-color: rgba(255, 255, 255, 0.1);"  ! Highlight row on hover
      write (unit, '(A)') "}"

      write (unit, '(A)') "h1 {"
      write (unit, '(A)') "  text-align: center;"
      write (unit, '(A)') "  margin-bottom: 20px;"
      write (unit, '(A)') "}"

      write (unit, '(A)') "</style>"

      write (unit, '(A)') "</head>"
      write (unit, '(A)') "<body>"

      if (this%iErrors > 0) then
         write (unit, '(A)') "<h1>Error Analysis Report</h1>"
         write (unit, '(A)') "<table>"
         write (unit, '(A)') "<thead><tr><th>No</th><th>Error</th><th>Description</th><th>Line</th><th>Column</th></tr></thead>"
         write (unit, '(A)') "<tbody>"

         ! Fill the table with error data
         do i = 1, this%iErrors
write(unit, '(A, I0, A, A, A, A, A, I0, A, I0, A)') "<tr><td>", i, "</td><td>", trim(this%errors(i)%characterSymbol), "</td><td>", &
               trim(this%errors(i)%description), "</td><td>", this%errors(i)%line, "</td><td>", this%errors(i)%column, "</td></tr>"
         end do
         write (unit, '(A)') "</tbody>"
      else
         write (unit, '(A)') "<h1>Clean Analysis Report</h1>"
         write (unit, '(A)') "<table>"
         write (unit, '(A)') "<thead><tr><th>No</th><th>Lexeme</th><th>Token</th><th>Line</th><th>Column</th></tr></thead>"
         write (unit, '(A)') "<tbody>"

         ! Fill the table with token data
         do i = 1, this%iTokens
        write (unit, '(A, I0, A, A, A, A, A, I0, A, I0, A)') "<tr><td>", i, "</td><td>", trim(this%tokens(i)%lexeme), "</td><td>", &
               trim(this%tokens(i)%name), "</td><td>", this%tokens(i)%line, "</td><td>", this%tokens(i)%column, "</td></tr>"
         end do
         write (unit, '(A)') "</tbody>"
      end if

      ! Close the table and HTML tags
      write (unit, '(A)') "</table>"
      write (unit, '(A)') "</body>"
      write (unit, '(A)') "</html>"

      ! Close the file
      close (unit)

   end subroutine create_report

   subroutine select_best_country(this)
      ! Número de tokens en la lista
      class(Analyzer), intent(in) :: this
      character(len=100) :: selected_country, percentage, temp_country, temp_population, temp_flag
      real :: min_saturation
      integer :: i, iostat
      integer :: value
      logical :: found_saturation

      min_saturation = 100.0
      found_saturation = .false.

      ! Buscar entre los tokens el país con la menor saturación
      do i = 1, this%iTokens
         if (trim(this%tokens(i)%name) == "SATURATION") then
            found_saturation = .true.

            percentage = this%tokens(i)%lexeme

            read (percentage, '(I10)', iostat=iostat) value

            if (iostat /= 0) print *, "Error converting saturation to integer"
            if (value <= min_saturation) then
               min_saturation = value
               temp_country = this%tokens(i - 8)%lexeme  ! Suponiendo que el país está 8 posiciones antes del token de saturación
               temp_population = this%tokens(i - 4)%lexeme
               temp_flag = this%tokens(i + 5)%lexeme
            end if
         end if
      end do

      if (found_saturation) then
         print *, temp_country
         print *, temp_population
         print *, temp_flag
      else
         print *, "No saturation tokens found"
      end if
   end subroutine select_best_country

   ! Generate a graph using Graphviz
   subroutine generate_graph(this)
      class(Analyzer), intent(in) :: this
      character(len=100) :: file
      integer :: i, iosx, unit
      character(len=:), allocatable :: graphviz_command
      character(len=100) :: continent
      real :: total_saturation
      integer :: country_count

      ! Open the file for writing the Graphviz dot file
      unit = 13
      file = "./data/output/graph.dot"
      open (unit=unit, file=file, iostat=iosx, status="new", action="write")
      if (iosx /= 0) stop "Error saving graph"

      ! Write the Graphviz header
      write (unit, '(A)') "digraph G {"
      write (unit, '(A)') "  node [shape=box, style=filled];"
      write (unit, '(A)') "  label=""Country Saturation Distribution"";"

      ! Initialize variables for continent processing
      continent = ""
      total_saturation = 0.0
      country_count = 0

      ! Process tokens to generate the graph
      do i = 1, this%iTokens
         select case (trim(this%tokens(i)%name))
         case ("CONTINENT")
            ! If we are starting a new continent, process the previous one
            if (continent /= "") then
               call write_continent_node(unit, continent, total_saturation, country_count)
            end if
            ! Start a new continent
            continent = trim(this%tokens(i)%lexeme)
            total_saturation = 0.0
            country_count = 0
         case ("COUNTRY")
            ! Write the country node
            call write_country_node(unit, this%tokens(i)%lexeme, this%tokens(i + 4)%lexeme)
            ! Accumulate saturation for the continent
            total_saturation = total_saturation + real(this%tokens(i + 4)%lexeme)
            country_count = country_count + 1
         end select
      end do

      ! Process the last continent
      if (continent /= "") then
         call write_continent_node(unit, continent, total_saturation, country_count)
      end if

      ! Write the Graphviz footer
      write (unit, '(A)') "}"

      ! Close the file
      close (unit)

      ! Generate the graph using Graphviz
      graphviz_command = "dot -Tpng ./data/output/graph.dot -o ./data/output/graph.png"
      call execute_command_line(graphviz_command)

   end subroutine generate_graph

   ! Write a country node to the Graphviz file
   subroutine write_country_node(unit, country, saturation)
      integer, intent(in) :: unit
      character(len=*), intent(in) :: country, saturation
      character(len=10) :: color

      ! Determine the color based on saturation
      select case (int(saturation))
      case (0:15)
         color = "white"
      case (16:30)
         color = "blue"
      case (31:45)
         color = "green"
      case (46:60)
         color = "yellow"
      case (61:75)
         color = "orange"
      case (76:100)
         color = "red"
      end select

      ! Write the country node
      write (unit, '(A, A, A, A, A)') "  ", trim(country), " [label=""", trim(country), "\n", trim(saturation), "%"", fillcolor=", color, "];"
   end subroutine write_country_node

   ! Write a continent node to the Graphviz file
   subroutine write_continent_node(unit, continent, total_saturation, country_count)
      integer, intent(in) :: unit
      character(len=*), intent(in) :: continent
      real, intent(in) :: total_saturation
      integer, intent(in) :: country_count
      character(len=10) :: color
      real :: avg_saturation

      ! Calculate the average saturation
      avg_saturation = total_saturation/country_count

      ! Determine the color based on average saturation
      select case (int(avg_saturation))
      case (0:15)
         color = "white"
      case (16:30)
         color = "blue"
      case (31:45)
         color = "green"
      case (46:60)
         color = "yellow"
      case (61:75)
         color = "orange"
      case (76:100)
         color = "red"
      end select

      ! Write the continent node
      write (unit, '(A, A, A, A, A)') "  ", trim(continent), " [label=""", trim(continent), "\n", trim(avg_saturation), "%"", fillcolor=", color, "];"
   end subroutine write_continent_node

end module analyzerModule
