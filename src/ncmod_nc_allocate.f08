!function nc_allocate(n, len, val1, val2, val3)

  !use hamodule

  !implicit none

  !interface num_len
    !module procedure num_len_int_2, num_len_int_4, num_len_real_4, num_len_real_8
  !end interface num_len

  !integer, intent(in) :: n
  !integer, intent(in), dimension(n) :: len
  !type(ncvalues), intent(out), dimension(:), allocatable, optional :: val1
  !type(ncvalues), intent(out), dimension(:,:), allocatable, optional :: val2
  !type(ncvalues), intent(out), dimension(:,:,:), allocatable, optional :: val3
  !integer :: allocate_error_status
  !character(nf90_max_name) :: allocate_error_message

  !integer :: nc_allocate, un = 6

  !select case(n)
  !case(1)
    !allocate( & !
      !val1(len(1)), & !
      !stat = allocate_error_status, & !
      !errmsg = allocate_error_message & !
      !)
  !case(2)
     !allocate( & !
       !val2(len(1), len(2)), & !
       !stat = allocate_error_status, & !
       !errmsg = allocate_error_message & !
       !)
  !case(3)
     !allocate( val3(len(1), len(2), len(3)), & !
       !stat = allocate_error_status, & !
       !errmsg = allocate_error_message & !
       !)
  !end select

  !nc_allocate = allocate_error_status
  
  !if(allocate_error_status /= 0) then
    !write(un, '(a)') ''
    !write(un, '(a)') 'WARNING: Failed to allocate val' // &
    !number_to_string( &
      !iv = int(n, 4), &
      !len = num_len(n) &
      !) // &
    !' with error status ' // &
    !number_to_string( &
      !iv = int(allocate_error_status, 4), &
      !len = num_len(allocate_error_status) &
      !) // ' (' //  trim(allocate_error_message) // ')'
    !write(un, '(a)') ''
  !end if

  !return

!end function nc_allocate
