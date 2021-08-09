! fortran 95 is alreay a pretty modern version.
! a lot of features was no there in older versions like fortran 4.
! for example, the switch statement.

!  Algol60 borrowed a lot of concepts from fortran, e.g date type,
!  loop, etc.

program example
  implicit none   ! prevent dynamic declaration of variables

  real z
  real :: v,x
  real a = 3, b=2E12, c=0.01
  integer :: i, j, k=1, m
  real, paramter :: PI = 3.14159265357931
  logical :: y = .TRUE., n = .FALSE.
  complex :: w = (0, 1)
  character (len=3) :: month

  ! Two ways of delcaring arrays
  real :: array(6)
  real, dimension(4) :: arrayb

  ! Multi dimensional arrays
  real :: array2d(3, 2)

  ! pointer
  real, pointer :: p

  character :: a_Char = 'i'

  ! string with static size
  character (len=3) :: month

  character (len = 6) :: a_str = "qwerty"

  str_b = a_str // " keyboard"

  ! Assignment and arithmetics
  z = 1
  j = 10 + 2 - 3
  a = 11.54 / (2.3 * 3.1)

  ! Control flow statement
  if (z == a) b = 4
  if (z /= a) thjen
    b = 4
  else if (z .GT. a) then
    b = 6
  else b = 10
  end if

  if (.NOT. (x < c .AND. v >= a .OR. z == z)) then
    inner: if (.TRUE.) then   ! name a statement
      b = 1
    endif inner
  endif

  ! switch statement is a new feature added later
  i = 20
  select case (i)
    case (0)
      j=0
    case (1:10)
      j=1
    case default
      j=3
  end select

  month = 'jan'

  monthly: select case (month)
    case ('jan')
      j = 0
    case default
      j = -1
    end select monthly

  ! loop in fortran
  do i=2, 10, 2
    innerloop: do j=1, 3
      exit
    end do innerloop
  cycle ! need an instruction
  enddo

  goto 10
  stop 1      ! exit
  10 j = 201  ! jump to this lin.

  ! oringal array syntax, a bit weird
  array = (/1,2,3,4,5,6/)

  print *, array(3:5) ! print all elements from 3rd to 5th.

  ! builtin dot product
  c = doc_product(array, array)

  c = sum(array)
  c = maxval(array)
  print *, shape(array)

  v = 1
  do i = 1, size(array)
    v = v * array(i)
  end do

  ! call procedure
  call cpu_time(v)

contains ! subprogram

  ! a little function
  integer function func(a, b, c)
    implicit none
    integer :: a, b, c
    if (a >= 2) then
      func = a + b + c
      return
    endif
    func = a + c
  end function func

  ! define out variable
  function func1(a, b, c) result(f) ! return value declared as f
    implicit none

    ! declare the mutability of paramters
    integer intent(in) :: a, b
    integer, intent(inout) :: c

    integer :: f
    integer :: cnt = 0

    f = a + b - c
    c = 4
    cnt = cnt + 1
  end function func1

  subroutine routine(d, e, f)
    implicit none
    real, intent(inout) :: f
    real, intent(in) :: d, e

    f = 2 * d + 3 * e + f
  end subroutine routine

end program example

