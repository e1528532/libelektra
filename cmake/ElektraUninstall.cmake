#
# Much faster is:
# xargs rm < install_manifest.txt
#

file (READ "@CMAKE_CURRENT_BINARY_DIR@/install_manifest.txt" files)
string (REGEX REPLACE "\n" ";" files "${files}")
foreach (file ${files})
	message(STATUS "Uninstalling \"$ENV{DESTDIR}${file}\"")
	exec_program(
		${CMAKE_COMMAND}
		ARGS "-E remove \"$ENV{DESTDIR}${file}\""
		OUTPUT_VARIABLE rm_out
		RETURN_VALUE rm_retval
	)
	if (NOT "${rm_retval}" STREQUAL 0)
		message (FATAL_ERROR "Problem when removing \"$ENV{DESTDIR}${file}\"")
	endif (NOT "${rm_retval}" STREQUAL 0)
endforeach (file)
