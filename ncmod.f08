module ncmodule

  use netcdf

  include "ncmod/ncmod_nc_types.f08"

contains

  include "ncmod/ncmod_get_att_xtype.f08"
  !include "ncmod/ncmod_get_var_xtype.f08"
  include "ncmod/ncmod_nc_xtype_info.f08"
  include "ncmod/ncmod_nc_error_check.f08"
  include "ncmod/ncmod_nc_reader.f08"
  !include "ncmod/ncmod_nc_print_info.f08"
  !include "ncmod/ncmod_nc_print_data.f08"
  !include "ncmod/ncmod_nc_value_print.f08"
  !include "ncmod/ncmod_nc_allocate.f08"
  !include "ncmod/ncmod_nc_value_conv.f08"
  !include "ncmod/ncmod_nc_variable_conv.f08"
  include "ncmod/ncmod_nc_attribute_print.f08"

end module ncmodule
