## package version check

if(packageVersion("microxanox") != package_version(microxanox_version_required))
  microxanox_version_check_result <- "**Stop**: incompatability in microxanox versions required and installed."

if(packageVersion("microxanox") == package_version(microxanox_version_required))
  microxanox_version_check_result <- microxanox_version_required

if(microxanox_version_check_result != microxanox_version_required)
  print(microxanox_version_check_result)
if(microxanox_version_check_result == microxanox_version_required)
  print(paste0("**OK**: microxanox package check passed. Working with version ", microxanox_version_check_result))

microxanox_path <- paste0("microxanox_v", microxanox_version_required)