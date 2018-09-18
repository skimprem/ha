subroutine nc_reader(nc_file, verbose_phrase)
  use netcdf
  use hamodule
  implicit none
  type(ncfile), intent(inout) :: nc_file
  integer :: i, j, stdout
  character(*), intent(in), optional :: verbose_phrase
  character(10000), allocatable :: temp_string
  character(len=:), allocatable :: frmt
  logical :: verbose_mode
  temp_string = ''
  stdout = 6
  verbose_mode = .false.
  if((present(verbose_phrase) .eqv. .true.) .and. (trim(adjustl(verbose_phrase)) /= '')) then
    verbose_mode = .true.
  end if
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 1x, a)'
    write(stdout, frmt) verbose_phrase, 'begin nc_reader(): '
  end if
! ###########################################################################################
! 1. READ HEADER OF FILE
! ###########################################################################################
  nc_file%mode = nf90_nowrite
  call nc_error_check(&
       'nc_open',&
       nf90_open(&
         path = nc_file%path,&          ! intent(in)
         mode = nc_file%mode,&          ! intent(in)
         ncid = nc_file%ncid,&          ! intent(out)
         chunksize = nc_file%chunksize& ! intent(inout)
         )&
       )
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, a)'
    write(stdout, frmt) verbose_phrase, 'nc_open(): done!'
    frmt = '(a, 4x, 2a)'
    write(stdout, frmt) verbose_phrase, 'path: ', nc_file%path
    write(stdout, frmt) verbose_phrase, 'mode: ', number_to_string(nc_file%mode)
    write(stdout, frmt) verbose_phrase, 'ncid: ', number_to_string(nc_file%ncid)
    write(stdout, frmt) verbose_phrase, 'chunksize: ', number_to_string(nc_file%chunksize)
  end if
  call nc_error_check(&
       'nc_inquire',&
       nf90_inquire(&
         ncid = nc_file%ncid,&                     ! intent(in)
         ndimensions = nc_file%ndimensions,&       ! intent(out)
         nvariables = nc_file%nvariables,&         ! intent(out)
         nattributes = nc_file%nattributes,&       ! intent(out)
         unlimiteddimid = nc_file%unlimiteddimid,& ! intent(out)
         formatnum = nc_file%formatnum&            ! intent(out)
         )&
       )
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, a)'
    write(stdout, frmt) verbose_phrase, 'nc_inquire(): done!'
    frmt = '(a, 4x, 2a)'
    write(stdout, frmt) verbose_phrase, 'inquired ndimensions: ', number_to_string(nc_file%ndimensions)
    write(stdout, frmt) verbose_phrase, 'inquired nvariables: ', number_to_string(nc_file%nvariables)
    write(stdout, frmt) verbose_phrase, 'inquired nattributes: ', number_to_string(nc_file%nattributes)
    write(stdout, frmt) verbose_phrase, 'inquired unlimiteddimid: ', number_to_string(nc_file%unlimiteddimid)
  end if
  allocate(nc_file%attribute(nc_file%nattributes))
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, a)'
    write(stdout, frmt) verbose_phrase, 'allocated nattributes: done!'
  end if
! ###########################################################################################
! 2. READ GLOBAL ATTRIBUTES
! ###########################################################################################
  do i = 1, nc_file%nattributes
    nc_file%attribute(i)%attnum = i
    temp_string = ''
    call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = nc_file%ncid,&                  ! intent(in)
          varid = nf90_global,&                  ! intent(in)
          attnum = nc_file%attribute(i)%attnum,& ! intent(in)
          name = temp_string&                    ! intent(out)
          )&
        )
    nc_file%attribute(i)%name = trim(adjustl(temp_string))
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 4x, a)'
      write(stdout, frmt) verbose_phrase, 'nc_inq_attname(): done!'
      frmt = '(a, 6x, 2a)'
      write(stdout, frmt) verbose_phrase, 'attnum: ', number_to_string(nc_file%attribute(i)%attnum)
      write(stdout, frmt) verbose_phrase, 'name: ', nc_file%attribute(i)%name
    end if
    call nc_error_check(&
         'nc_inquire_attribute',&
         nf90_inquire_attribute(&
           ncid = nc_file%ncid,&                ! intent(in)
           varid = nf90_global,&                ! intent(in)
           name = nc_file%attribute(i)%name,&   ! intent(in)
           xtype = nc_file%attribute(i)%xtype,& ! intent(out)
           len = nc_file%attribute(i)%len&      ! intent(out)
           )&
         )
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 4x, a)'
      write(stdout, frmt) verbose_phrase, 'nc_inquire_attribute(): done!'
      frmt = '(a, 6x, 2a)'
      write(stdout, frmt) verbose_phrase, 'xtype: ', nc_xtype_info(nc_file%attribute(i)%xtype)
      write(stdout, frmt) verbose_phrase, 'len: ', number_to_string(nc_file%attribute(i)%len)
    end if
    if(verbose_mode .eqv. .true.) then
      call get_att_xtype(&
           ncid = nc_file%ncid,&                    ! intent(in)
           varid = nf90_global,&                    ! intent(in)
           attribute = nc_file%attribute(i),&       ! intent(inout)
           verbose_phrase = verbose_phrase//' ') ! intent(in)
    else
      call get_att_xtype(&
           ncid = nc_file%ncid,&                    ! intent(in)
           varid = nf90_global,&                    ! intent(in)
           attribute = nc_file%attribute(i)&        ! intent(inout)
           )
    end if
  end do
! ###########################################################################################
! READ DIMENSIONS
! ###########################################################################################
  allocate(nc_file%dimension(nc_file%ndimensions))
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, a)'
    write(stdout, frmt) verbose_phrase, 'allocated ndimensions: done!'
  end if
  do i = 1, nc_file%ndimensions
    nc_file%dimension(i)%dimid = i
    temp_string = ''
    call nc_error_check(&
      'nc_inquire_dimension',&
      nf90_inquire_dimension(&
          ncid = nc_file%ncid,&                ! intent(in)
          dimid = nc_file%dimension(i)%dimid,& ! intent(in)
          name = temp_string,&                 ! intent(out)
          len = nc_file%dimension(i)%len&      ! intent(out)
        )&
      )
    nc_file%dimension(i)%name = trim(adjustl(temp_string))
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 4x, a)'
      write(stdout, frmt) verbose_phrase, 'nc_inquire_dimensions(): done!'
      frmt = '(a, 6x, 2a)'
      write(stdout, frmt) verbose_phrase, 'name: ', nc_file%dimension(i)%name
      write(stdout, frmt) verbose_phrase, 'len: ', number_to_string(nc_file%dimension(i)%len)
    end if
  end do
! ###########################################################################################
! 3. READ VARIABLES
! ###########################################################################################
  allocate(nc_file%variable(nc_file%nvariables))
  if(verbose_mode .eqv. .true.) then
    frmt = '(a, 2x, a)'
    write(stdout, frmt) verbose_phrase, 'allocated nvariables: done!'
  end if
  do i = 1, nc_file%nvariables
    nc_file%variable(i)%varid = i
    temp_string = ''
    call nc_error_check(&
      'nc_inquire_variable',&
      nf90_inquire_variable(&
        ncid = nc_file%ncid,&               ! intent(in)
        varid = nc_file%variable(i)%varid,& ! intent(in)
        name = temp_string,&                ! intent(out)
        xtype = nc_file%variable(i)%xtype,& ! intent(out)
        ndims = nc_file%variable(i)%ndims,& ! intent(out)
        natts = nc_file%variable(i)%natts&  ! intent(out)
        )&
      )
    nc_file%variable(i)%name = trim(adjustl(temp_string))
    if(verbose_mode .eqv. .true.) then
      frmt = '(a, 4x, a)'
      write(stdout, frmt) verbose_phrase, 'nc_inquire_variable(): done!'
      frmt = '(a, 6x, 2a)'
      write(stdout, frmt) verbose_phrase, 'name: ', nc_file%variable(i)%name
      write(stdout, frmt) verbose_phrase, 'xtype: ', nc_xtype_info(nc_file%variable(i)%xtype)
      write(stdout, frmt) verbose_phrase, 'ndims: ', number_to_string(nc_file%variable(i)%ndims)
      write(stdout, frmt) verbose_phrase, 'natts: ', number_to_string(nc_file%variable(i)%natts)
    end if
    if(nc_file%variable(i)%ndims > 0) then
      allocate(nc_file%variable(i)%dimids(nc_file%variable(i)%ndims))
      call nc_error_check(&
        'nc_inquire_variable',&
        nf90_inquire_variable(&
          ncid = nc_file%ncid,&                ! intent(in)
          varid = nc_file%variable(i)%varid,&  ! intent(in)
          dimids = nc_file%variable(i)%dimids& ! intent(out), dimension(:)
          )&
        )
      if(verbose_mode .eqv. .true.) then
        frmt = '(a, 4x, a)'
        write(stdout, frmt) verbose_phrase, 'nc_inquire_variable(): done!'
        frmt = '(a, 6x, 2a)'
        write(stdout, frmt)&
        (verbose_phrase, 'dimids: ', number_to_string(nc_file%variable(i)%dimids(j)), j = 1, nc_file%variable(i)%ndims)
      end if
    end if
    stop
! *******************************************************************************************
! 3.1 READ VARIABLE ATTRIBUTES
! *******************************************************************************************
    allocate(nc_file%variable(i)%attribute(nc_file%variable(i)%natts))
    if(verbose_mode .eqv. .true.) then
      write(stdout, frmt) verbose_phrase, 'allocated variable attributes: done!'
    end if
    do j = 1, nc_file%variable(i)%natts
      nc_file%variable(i)%attribute(j)%attnum = j
      temp_string = ''
      call nc_error_check(&
        'nc_inq_attname',&
        nf90_inq_attname(&
          ncid = nc_file%ncid,&                              ! intent(in)
          varid = nc_file%variable(i)%varid,&                ! intent(in)
          attnum = nc_file%variable(i)%attribute(j)%attnum,& ! intent(in)
          name = temp_string&                                ! intent(out)
          )&
        )
      nc_file%variable(i)%attribute(j)%name = trim(adjustl(temp_string))
      if(verbose_mode .eqv. .true.) then
        write(stdout, frmt) verbose_phrase, 'nc_inq_attname(): done!'
        write(stdout, frmt) verbose_phrase, 'attnum: ', number_to_string(nc_file%variable(i)%attribute(j)%attnum)
        write(stdout, frmt) verbose_phrase, 'name: ', nc_file%variable(i)%attribute(j)%name
      end if
      call nc_error_check(&
        'nc_inquire_attribute',&
        nf90_inquire_attribute(&
          ncid = nc_file%ncid,&                            ! intent(in)
          varid = nc_file%variable(i)%varid,&              ! intent(in)
          name = nc_file%variable(i)%attribute(j)%name,&   ! intent(in)
          xtype = nc_file%variable(i)%attribute(j)%xtype,& ! intent(out)
          len = nc_file%variable(i)%attribute(j)%len&      ! intent(out)
          )&
        )
      if(verbose_mode .eqv. .true.) then
        write(stdout, frmt) verbose_phrase, 'nc_inquire_attribute(): done!'
        write(stdout, frmt) verbose_phrase, 'xtype: ', nc_xtype_info(nc_file%variable(i)%attribute(j)%xtype)
        write(stdout, frmt) verbose_phrase, 'len: ', number_to_string(nc_file%variable(i)%attribute(j)%len)
      end if
      if(verbose_mode .eqv. .true.) then
        call get_att_xtype(&
             ncid = nc_file%ncid, &
             varid = nc_file%variable(i)%varid,&
             attribute = nc_file%variable(i)%attribute(j),&
             verbose_phrase = verbose_phrase//''&
             )
      else
        call get_att_xtype(&
             ncid = nc_file%ncid,&
             varid = nc_file%variable(i)%varid,&
             attribute = nc_file%variable(i)%attribute(j)&
             )
      end if
    end do
! *******************************************************************************************
! 3.2 
! *******************************************************************************************
    !do j = 1, nc_file%variable(i)%ndims
      !call nc_error_check(&
        !'nc_inquire_dimension',&
        !nf90_inquire_dimension(&
          !ncid = nc_file%ncid,&
          !dimid = nc_file%variable(i)%dimids(j),&
          !len = nc_file%variable(i)%dimension(nc_file%variable(i)%dimids(j))%len&
          !!)&
        !)
    !end do
    !allocate(&
      !nc_file%variable(i)%dimension(nc_file%variable(i)%ndims),&
      !)

    !if(verbose_mode .eqv. .true.) then
      !call get_var_xtype(&
        !ncid = nc_file%ncid,&
        !varid = nc_file%variable(i)%varid,&
        !xtype = nc_file%variable(i)%xtype,&
        !ndims = nc_file%variable(i)%ndims,&
        !len = nc_file%variable(i)%len,&
        !value = nc_file%variable(i)%value,&
        !verbose_phrase = verbose_phrase, ''&
        !)
    !else
      !call get_var_xtype(&
        !ncid = nc_file%ncid,&
        !varid = nc_file%variable(i)%varid,&
        !xtype = nc_file%variable(i)%xtype,&
        !ndims = nc_file%variable(i)%ndims,&
        !len = nc_file%variable(i)%len,&
        !value = nc_file%variable(i)%value&
        !)
    !end if
  end do
  if(verbose_mode .eqv. .true.) then
    write(stdout, frmt) verbose_phrase, 'end nc_reader()'
  end if
  return
end subroutine nc_reader
