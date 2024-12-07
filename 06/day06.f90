program day06
  implicit none
  type :: guard
    integer :: xpos
    integer :: ypos
    integer :: direction
  end type
  character(len=256) :: line
  character(len=:), allocatable :: lines(:)
  character(len=:), allocatable :: loopblocks(:)
  integer :: ios
  integer :: count = 2
  integer :: inputlen
  integer :: numx
  integer :: numl
  type(guard) :: g
  type(guard) :: lg

  read(*,'(A)', iostat=ios, size=inputlen, advance="no") line
  allocate(character(len=inputlen) :: lines(inputlen))
  allocate(character(len=inputlen) :: loopblocks(inputlen))
  lines(1) = line
  loopblocks(1) = line
  do while (count <= inputlen)
     read(*,'(A)', iostat=ios) line
     if (ios == -1) then
       exit
     end if

     lines(count) = line
     loopblocks(count) = line
     count = count + 1
  end do
  g = find_guard(lines)

  do while (is_in_bounds(g, lines))
    call board_tick(lines, g)
  end do

  numx = count_chars(lines, "X")
  print *, numx

  lg = find_guard(loopblocks)
  g = find_guard(loopblocks)
  do while (is_in_bounds(lg, loopblocks))
    call loop_board_tick(loopblocks, lg, g)
  end do

  numl = count_chars(loopblocks, "L")
  print *, numl

  contains

  function is_in_bounds(g, board) result(r)
    logical :: r
    character(len=:), intent(in), allocatable :: board(:)
    type(guard), intent(in) :: g
    r = g%xpos > 0 .and. g%ypos > 0 .and. g%xpos <= size(lines) .and. g%ypos <= size(lines)
  end function is_in_bounds

  function find_guard(board) result(g)
    implicit none
    character(len=:), intent(in), allocatable :: board(:)
    integer :: y
    integer :: x
    type(guard) :: g

    do y = 1, size(board)
      do x = 1, size(board)
        if (board(y)(x:x) == "^") then
          g%xpos = x
          g%ypos = y
          g%direction = 0
        else if (board(y)(x:x) == ">") then
          g%xpos = x
          g%ypos = y
          g%direction = 1
        else if (board(y)(x:x) == "v") then
          g%xpos = x
          g%ypos = y
          g%direction = 2
        else if (board(y)(x:x) == "<") then
          g%xpos = x
          g%ypos = y
          g%direction = 3
        end if
      end do
    end do
  end function find_guard

  function count_chars(board, char) result(c)
    implicit none
    character(len=:), intent(in), allocatable :: board(:)
    character(1), intent(in) :: char
    integer :: c
    integer :: x
    integer :: y

    c = 0

    do y = 1, size(board)
      do x = 1, size(board)
        if (board(y)(x:x) == char) then
          c = c + 1
        end if
      end do
    end do
  end function count_chars

  function can_create_loop(board, g) result(c)
    implicit none
    character(len=:), intent(in), allocatable :: board(:)
    type(guard), intent(in) :: g
    type(guard) :: cg
    logical :: c
    integer :: x

    cg%xpos = g%xpos
    cg%ypos = g%ypos
    cg%direction = g%direction

    x = 0
    do while (is_in_bounds(cg, board) .and. .not. x > 100000) ! lol
      x = x + 1
      call move_guard(board, cg)
      if (cg%xpos == g%xpos .and. cg%ypos == g%ypos .and. cg%direction == g%direction) then
        exit
      end if
    end do

    c = is_in_bounds(cg, board)
  end function can_create_loop

  subroutine board_tick(board, g)
    implicit none
    character(len=:), allocatable :: board(:)
    type(guard) :: g

    board(g%ypos)(g%xpos:g%xpos) = "X"

    call move_guard(board, g)
  end subroutine board_tick

  subroutine loop_board_tick(board, g, startg)
    implicit none
    character(len=:), allocatable :: board(:)
    character(len=1) :: orig
    type(guard) :: g
    type(guard) :: startg

    if (g%direction == 0) then
      orig = board(g%ypos-1)(g%xpos:g%xpos)
      if (board(g%ypos-1)(g%xpos:g%xpos) == ".") then
        board(g%ypos-1)(g%xpos:g%xpos) = "#"
      end if
    else if (g%direction == 1) then
      orig = board(g%ypos)(g%xpos+1:g%xpos+1)
      if (board(g%ypos)(g%xpos+1:g%xpos+1) == ".") then
        board(g%ypos)(g%xpos+1:g%xpos+1) = "#"
      end if
    else if (g%direction == 2) then
      orig = board(g%ypos+1)(g%xpos:g%xpos)
      if (board(g%ypos+1)(g%xpos:g%xpos) == ".") then
        board(g%ypos+1)(g%xpos:g%xpos) = "#"
      end if
    else
      orig = board(g%ypos)(g%xpos-1:g%xpos-1)
      if (board(g%ypos)(g%xpos-1:g%xpos-1) == ".") then
        board(g%ypos)(g%xpos-1:g%xpos-1) = "#"
      end if
    end if

    if (can_create_loop(board, startg)) then
      if (g%direction == 0) then
        board(g%ypos-1)(g%xpos:g%xpos) = "L"
      else if (g%direction == 1) then
        board(g%ypos)(g%xpos+1:g%xpos+1) = "L"
      else if (g%direction == 2) then
        board(g%ypos+1)(g%xpos:g%xpos) = "L"
      else
        board(g%ypos)(g%xpos-1:g%xpos-1) = "L"
      end if
    else
      if (g%direction == 0) then
        board(g%ypos-1)(g%xpos:g%xpos) = orig
      else if (g%direction == 1) then
        board(g%ypos)(g%xpos+1:g%xpos+1) = orig
      else if (g%direction == 2) then
        board(g%ypos+1)(g%xpos:g%xpos) = orig
      else
        board(g%ypos)(g%xpos-1:g%xpos-1) = orig
      end if
    end if

    call move_guard(board, g)
  end subroutine loop_board_tick

  subroutine move_guard(board, g)
    implicit none
    character(len=:), allocatable :: board(:)
    type(guard) :: g

    if (g%direction == 0) then
      if (g%ypos == 1 .or. g%ypos > 1 .and. board(g%ypos - 1)(g%xpos:g%xpos) /= "#") then
        g%ypos = g%ypos - 1
      else if (board(g%ypos-1)(g%xpos:g%xpos) == "#") then
        g%direction = mod(g%direction + 1, 4)
      end if
    else if (g%direction == 1) then
      if (g%xpos == size(board) .or. g%xpos <= size(board) - 1 .and. board(g%ypos)(g%xpos+1:g%xpos+1) /= "#") then
        g%xpos = g%xpos + 1
      else if (board(g%ypos)(g%xpos+1:g%xpos+1) == "#") then
        g%direction = mod(g%direction + 1, 4)
      end if
    else if (g%direction == 2) then
      if (g%ypos == size(board) .or. g%ypos <= size(board) .and. board(g%ypos + 1)(g%xpos:g%xpos) /= "#") then
        g%ypos = g%ypos + 1
      else if (board(g%ypos+1)(g%xpos:g%xpos) == "#") then
        g%direction = mod(g%direction + 1, 4)
      end if
    else if (g%direction == 3) then
      if (g%xpos == 1 .or. g%xpos > 1 .and. board(g%ypos)(g%xpos-1:g%xpos-1) /= "#") then
        g%xpos = g%xpos - 1
      else if (board(g%ypos)(g%xpos-1:g%xpos-1) == "#") then
        g%direction = mod(g%direction + 1, 4)
      end if
    end if
  end subroutine move_guard
end program day06
