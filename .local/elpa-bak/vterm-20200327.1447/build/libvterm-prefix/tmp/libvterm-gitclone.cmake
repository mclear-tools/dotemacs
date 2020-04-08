
if(NOT "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitinfo.txt" IS_NEWER_THAN "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt")
  message(STATUS "Avoiding repeated git clone, stamp file is up to date: '/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt'")
  return()
endif()

execute_process(
  COMMAND ${CMAKE_COMMAND} -E rm -rf "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm"
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to remove directory: '/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm'")
endif()

# try the clone 3 times in case there is an odd git clone issue
set(error_code 1)
set(number_of_tries 0)
while(error_code AND number_of_tries LESS 3)
  execute_process(
    COMMAND "/usr/local/bin/git"  clone --no-checkout "https://github.com/neovim/libvterm.git" "libvterm"
    WORKING_DIRECTORY "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src"
    RESULT_VARIABLE error_code
    )
  math(EXPR number_of_tries "${number_of_tries} + 1")
endwhile()
if(number_of_tries GREATER 1)
  message(STATUS "Had to git clone more than once:
          ${number_of_tries} times.")
endif()
if(error_code)
  message(FATAL_ERROR "Failed to clone repository: 'https://github.com/neovim/libvterm.git'")
endif()

execute_process(
  COMMAND "/usr/local/bin/git"  checkout 65dbda3ed214f036ee799d18b2e693a833a0e591 --
  WORKING_DIRECTORY "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm"
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to checkout tag: '65dbda3ed214f036ee799d18b2e693a833a0e591'")
endif()

set(init_submodules TRUE)
if(init_submodules)
  execute_process(
    COMMAND "/usr/local/bin/git"  submodule update --recursive --init 
    WORKING_DIRECTORY "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm"
    RESULT_VARIABLE error_code
    )
endif()
if(error_code)
  message(FATAL_ERROR "Failed to update submodules in: '/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm'")
endif()

# Complete success, update the script-last-run stamp file:
#
execute_process(
  COMMAND ${CMAKE_COMMAND} -E copy
    "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitinfo.txt"
    "/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt"
  RESULT_VARIABLE error_code
  )
if(error_code)
  message(FATAL_ERROR "Failed to copy script-last-run stamp file: '/Users/roambot/.emacs.d/.local/elpa/vterm-20200327.1447/build/libvterm-prefix/src/libvterm-stamp/libvterm-gitclone-lastrun.txt'")
endif()

