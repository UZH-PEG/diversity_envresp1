## package version check

if (packageVersion("microxanox") < package_version(microxanox_release)) {
  stop(paste0("microxanox version needs to be at least ", microxanox_release))
}

print(paste0("Version of microxanox: ", microxanox_release))
