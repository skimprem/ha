subroutine print_help(type_help)
  implicit none
  character(*), intent(in), optional :: type_help

  !print '(a)', 'Usage: ha [OPTION]... [FILE]...'
  print '(a)', 'Usage: ha [OPTION]...'
  print '(a)', ''
  print '(a)', 'Calculates the coefficients of spherical harmonics over a regular grid' 
  print '(a)', 'using various algorithms'
  print '(a)', ''
  print '(a)', '  -nf, --ncfile  set the source file with grid data in NetCDF format'
  print '(a)', ''
  print '(a)', '  -nm, --ncmode  set the output mode for the source file with grid data using the following parameters:'
  print '(a)', "                 'view' - print the information about the grid file along with the data"
  !print '(a)', "                 'viewdata' - print only data"
  !print '(a)', "                 'viewinfo' - print only information about the grid file"
  print '(a)', ''
  print '(a)', '  -hm, --hamode  set expand method using the following parameters:'
  print '(a)', "                 'dh' - Driscoll and Healy sampling theorem"
  print '(a)', "                 'ls' - Least squares inversion"
  print '(a)', ''
  print '(a)', '  -h, --help     print help'
  print '(a)', ''



  if(present(type_help)) then
    select case(type_help)
    case('stop')
      !stop 'Stopped!'
      stop
    end select
  end if

  return
end subroutine print_help
