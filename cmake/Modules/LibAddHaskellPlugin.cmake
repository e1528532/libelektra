include (LibAddMacros)

# ~~~
# Allows one to add plugins written in Haskell, setting up the include paths and
# libraries automatically.
#
# Expects that plugins make use of Cabal as their build system.
#
# MODULES:
#  the name of the Haskell modules to be compiled
#  by default it assumes there is a single module called Elektra.<pluginName>
# NO_SHARED_SANDBOX:
#  By default all Haskell plugins and the bindings are compiled in a shared sandbox to
#  speed up compilation times by only compiling commonly-used libraries once. Set this
#  flag to use an independent sandbox instead in case there are e.g. library version conflicts
# SANDBOX_ADD_SOURCES:
#  additional source paths which should be added to the Cabal sandbox
#  required if the build should depend on Haskell libraries not available on hackage
# ADDITIONAL_SOURCES:
#  in case your plugin depends on other files than *.hs and *.lhs Haskell files and the default
#  Cabal file and c test file and setup file, you can specify them here
# ~~~
macro (add_haskell_plugin target)
	cmake_parse_arguments (ARG
			       "" # optional keywords
			       "MODULE" # one value keywords
			       "MODULES;SANDBOX_ADD_SOURCES;ADDITIONAL_SOURCES" # multi value keywords
			       ${ARGN})

	set (PLUGIN_NAME ${target})
	set (PLUGIN_NAME_CAPITALIZED ${target})
	string (SUBSTRING ${PLUGIN_NAME} 0 1 FIRST_LETTER)
	string (TOUPPER ${FIRST_LETTER} FIRST_LETTER)
	string (REGEX REPLACE "^.(.*)" "${FIRST_LETTER}\\1" PLUGIN_NAME_CAPITALIZED "${PLUGIN_NAME}")
	string (TOUPPER ${PLUGIN_NAME} PLUGIN_NAME_UPPERCASE)

	if (DEPENDENCY_PHASE)
		find_package (Haskell)
		find_package (Pluginprocess)

		# set by find_program
		if (PLUGINPROCESS_FOUND)
			if (HASKELL_FOUND)
				check_binding_included ("haskell" HAVE_HASKELL_BINDING)
				if (HAVE_HASKELL_BINDING)

					# needed for HsFFI.h
					execute_process (COMMAND ${GHC_EXECUTABLE} --print-libdir
							 OUTPUT_VARIABLE GHC_LIB_DIR
							 OUTPUT_STRIP_TRAILING_WHITESPACE)

					set (GHC_INCLUDE_DIRS
					     ${CMAKE_CURRENT_BINARY_DIR}/dist/build/Elektra # for the haskell function stubs
					     ${GHC_LIB_DIR}/include # for HsFFI.h
					     )

					set (CABAL_OPTS "--prefix=${CMAKE_INSTALL_PREFIX}")
					if (BUILD_SHARED OR BUILD_FULL) # shared variants of ghc libraries have the ghc version as a suffix
						set (GHC_DYNAMIC_SUFFIX "-ghc${GHC_VERSION}")
						if (APPLE)
							set (GHC_DYNAMIC_ENDING ".dylib")
						else (APPLE)
							set (GHC_DYNAMIC_ENDING ".so")
						endif (APPLE)
						set (CABAL_OPTS "${CABAL_OPTS};--enable-shared")
					elseif (BUILD_STATIC)
						set (GHC_DYNAMIC_ENDING ".a")
						set (CABAL_OPTS "${CABAL_OPTS};--disable-shared")
					endif ()

					# ~~~
					# since we want to continue to use our cmake add_plugin macro
					# we compile via the c compiler instead of ghc
					# so we must feed it with the ghc library paths manually
					# inspired by https://github.com/jarrett/cpphs/blob/master/Makefile
					# use HSrts_thr for the threaded version of the rts
					# ~~~
					set (GHC_RTS_PATH "${GHC_LIB_DIR}/rts")
					find_library (GHC_RTS_LIB "HSrts${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_RTS_PATH})

					execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest base
							 OUTPUT_VARIABLE GHC_BASE_NAME
							 OUTPUT_STRIP_TRAILING_WHITESPACE)
					set (GHC_BASE_PATH "${GHC_LIB_DIR}/${GHC_BASE_NAME}")
					find_library (GHC_BASE_LIB "HS${GHC_BASE_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_BASE_PATH})

					execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest integer-gmp
							 OUTPUT_VARIABLE GHC_GMP_NAME
							 OUTPUT_STRIP_TRAILING_WHITESPACE)
					set (GHC_GMP_PATH "${GHC_LIB_DIR}/${GHC_GMP_NAME}")
					find_library (GHC_GMP_LIB "HS${GHC_GMP_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_GMP_PATH})

					execute_process (COMMAND ${GHC-PKG_EXECUTABLE} latest ghc-prim
							 OUTPUT_VARIABLE GHC_PRIM_NAME
							 OUTPUT_STRIP_TRAILING_WHITESPACE)
					set (GHC_PRIM_PATH "${GHC_LIB_DIR}/${GHC_PRIM_NAME}")
					find_library (GHC_PRIM_LIB "HS${GHC_PRIM_NAME}${GHC_DYNAMIC_SUFFIX}" PATHS ${GHC_PRIM_PATH})

					if (GHC_RTS_LIB)
						if (GHC_BASE_LIB)
							if (GHC_GMP_LIB)
								if (GHC_PRIM_LIB)

									set (
										PLUGIN_HASKELL_NAME
										"${CMAKE_CURRENT_BINARY_DIR}/libHS${target}${GHC_DYNAMIC_SUFFIX}${GHC_DYNAMIC_ENDING}"
										)

									set (GHC_LIBS
									     ${GHC_RTS_LIB}
									     ${GHC_BASE_LIB}
									     ${GHC_GMP_LIB}
									     gmp
									     ${GHC_PRIM_LIB}
									     ${PLUGIN_HASKELL_NAME})

									# GHC's structure differs between OSX and Linux
									# On OSX we need to link iconv and Cffi additionally
									if (APPLE)
										find_library (GHC_FFI_LIB Cffi PATHS "${GHC_LIB_DIR}/rts")
										if (GHC_FFI_LIB)
											set (GHC_LIBS ${GHC_LIBS} ${GHC_FFI_LIB} iconv)
										else (GHC_FFI_LIB)
											remove_plugin (${target} "GHC_FFI_LIB not found")
										endif (GHC_FFI_LIB)
									endif (APPLE)

									# configure include paths
									configure_file ("${CMAKE_CURRENT_SOURCE_DIR}/${target}.cabal.in"
											"${CMAKE_CURRENT_BINARY_DIR}/${target}.cabal"
											@ONLY)

									# configure the haskell plugin base file for the current plugin
									configure_file (
										"${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.c.in"
										"${CMAKE_CURRENT_BINARY_DIR}/haskell.c"
										@ONLY)

									# same for the header
									configure_file (
										"${CMAKE_SOURCE_DIR}/src/plugins/haskell/haskell.h.in"
										"${CMAKE_CURRENT_BINARY_DIR}/haskell.h"
										@ONLY)

									# copy the readme so the macro in haskell.c finds it
									file (COPY
									      "${CMAKE_CURRENT_SOURCE_DIR}/README.md"
									      DESTINATION
									      "${CMAKE_CURRENT_BINARY_DIR}")

									# same for the setup logic, depending on whether a custom one exists
									# use the default suitable for almost everything
									set (CABAL_CUSTOM_SETUP_FILE
									     "${CMAKE_CURRENT_SOURCE_DIR}/Setup.hs.in")
									if (NOT EXISTS ${CABAL_CUSTOM_SETUP_FILE})
										set (CABAL_CUSTOM_SETUP_FILE
										     "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in")
									endif (NOT EXISTS ${CABAL_CUSTOM_SETUP_FILE})
									configure_file (${CABAL_CUSTOM_SETUP_FILE}
											"${CMAKE_CURRENT_BINARY_DIR}/Setup.hs"
											@ONLY)

									set (SANDBOX_ADD_SOURCES "${ARG_SANDBOX_ADD_SOURCES};")
									set (SANDBOX_ADD_SOURCES
									     "${ARG_SANDBOX_ADD_SOURCES};../../bindings/haskell/")

									configure_haskell_sandbox (SHARED_SANDBOX
												   ${HASKELL_SHARED_SANDBOX}
												   SANDBOX_ADD_SOURCES
												   ${SANDBOX_ADD_SOURCES}
												   DEPENDS
												   c2hs_haskell)

									# Grab potential haskell source files
									file (GLOB_RECURSE PLUGIN_SOURCE_FILES
											   "${CMAKE_CURRENT_SOURCE_DIR}/*.hs"
											   "${CMAKE_CURRENT_SOURCE_DIR}/*.lhs")
									set (PLUGIN_SOURCE_FILES
									     "${PLUGIN_SOURCE_FILES}"
									     "${CMAKE_CURRENT_SOURCE_DIR}/${target}.cabal.in"
									     "${CMAKE_CURRENT_SOURCE_DIR}/testmod_${target}.c"
									     "${CMAKE_SOURCE_DIR}/src/plugins/haskell/Setup.hs.in"
									     "${ARG_ADDITIONAL_SOURCES}")

									add_custom_command (OUTPUT ${PLUGIN_HASKELL_NAME}
											    COMMAND ${CABAL_EXECUTABLE} configure
												    ${CABAL_OPTS} -v0
											    COMMAND ${CABAL_EXECUTABLE} build -v0
											    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
											    DEPENDS c2hs_haskell
												    ${PLUGIN_SOURCE_FILES}
												    ${HASKELL_ADD_SOURCES_TARGET})
									add_custom_target (${target} ALL DEPENDS ${PLUGIN_HASKELL_NAME})

									if (BUILD_SHARED OR BUILD_FULL)
										install (DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/haskell"
												   DESTINATION
												   "lib${LIB_SUFFIX}/elektra/")
									endif (BUILD_SHARED OR BUILD_FULL)

									set (HASKELL_RPATH
									     "${GHC_RTS_PATH}"
									     "${GHC_BASE_PATH}"
									     "${GHC_PRIM_PATH}"
									     "${GHC_GMP_PATH}"
									     "${CMAKE_INSTALL_RPATH}"
									     "${CMAKE_INSTALL_PREFIX}/lib${LIB_SUFFIX}/elektra/haskell")

								else (GHC_PRIM_LIB)
									remove_plugin (${target} "GHC_PRIM_LIB not found")
								endif (GHC_PRIM_LIB)
							else (GHC_GMP_LIB)
								remove_plugin (${target} "GHC_GMP_LIB not found")
							endif (GHC_GMP_LIB)
						else (GHC_BASE_LIB)
							remove_plugin (${target} "GHC_BASE_LIB not found")
						endif (GHC_BASE_LIB)
					else (GHC_RTS_LIB)
						remove_plugin (${target} "GHC_RTS_LIB not found")
					endif (GHC_RTS_LIB)

				else (HAVE_HASKELL_BINDING)
					remove_plugin (${target} "haskell bindings are not included in the cmake configuration")
				endif (HAVE_HASKELL_BINDING)
			else (HASKELL_FOUND)
				remove_plugin (${target} ${HASKELL_NOTFOUND_INFO})
			endif (HASKELL_FOUND)
		else (PLUGINPROCESS_FOUND)
			remove_plugin (${target} "${PLUGINPROCESS_NOTFOUND_INFO}, but required for haskell plugins")
		endif (PLUGINPROCESS_FOUND)
	endif (DEPENDENCY_PHASE)

	# compile our c wrapper which takes care of invoking the haskell runtime
	# the actual haskell plugin gets linked in dynamically as a library
	add_plugin (${target}
		    SOURCES ${CMAKE_CURRENT_BINARY_DIR}/haskell.h
			    ${CMAKE_CURRENT_BINARY_DIR}/haskell.c
		    INCLUDE_DIRECTORIES ${GHC_INCLUDE_DIRS}
		    LINK_LIBRARIES ${GHC_LIBS}
		    LINK_ELEKTRA elektra-pluginprocess
		    DEPENDS ${target}
			    c2hs_haskell)
	add_plugintest (${target} MEMLEAK)
	if (TARGET elektra-${target})
		if (DEPENDENCY_PHASE AND (BUILD_SHARED OR BUILD_FULL))
			set_target_properties (elektra-${target} PROPERTIES INSTALL_RPATH "${HASKELL_RPATH}")
		endif (DEPENDENCY_PHASE AND (BUILD_SHARED OR BUILD_FULL))
		if (ADDTESTING_PHASE AND BUILD_TESTING AND (BUILD_SHARED OR BUILD_FULL))
			get_target_property (HASKELL_RPATH elektra-${target} INSTALL_RPATH)
			set_target_properties (testmod_${target} PROPERTIES INSTALL_RPATH "${HASKELL_RPATH}")
		endif (ADDTESTING_PHASE AND BUILD_TESTING AND (BUILD_SHARED OR BUILD_FULL))
	endif (TARGET elektra-${target})

	mark_as_advanced (GHC_FFI_LIB GHC_RTS_LIB GHC_BASE_LIB GHC_GMP_LIB GHC_PRIM_LIB)
endmacro (add_haskell_plugin)

# ~~~
# Allows adding sandbox sources for haskell plugins which will be executed in a serial manner
# to avoid cabal concurrency issues https://github.com/haskell/cabal/issues/2220. Also initializes
# sandboxes and will install the dependencies into them.
#
# SANDBOX_ADD_SOURCES:
#  additional source paths which should be added to the cabal sandbox
#  required if the build should depend on haskell libraries not available on hackage
# WORKING_DIRECTORY:
#  in case your plugin depends on other files than *.hs and *.lhs haskell files and the default
#  cabal file and c test file and setup file, you can specify them here
# DEPENDS:
#  additional targets this call should be dependent on
# ~~~
macro (configure_haskell_sandbox)
	cmake_parse_arguments (ARG
			       "" # optional keywords
			       "SHARED_SANDBOX" # one value keywords
			       "SANDBOX_ADD_SOURCES;DEPENDS" # multi value keywords
			       ${ARGN})

	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/cabal.sandbox.config"
		COMMAND ${CABAL_EXECUTABLE} sandbox init ${ARG_SHARED_SANDBOX} -v0)

	if (ARG_SANDBOX_ADD_SOURCES)
		foreach (SANDBOX_ADD_SOURCE ${ARG_SANDBOX_ADD_SOURCES})
			add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/cabal.sandbox.config"
					    COMMAND ${CABAL_EXECUTABLE} sandbox add-source "${SANDBOX_ADD_SOURCE}" -v0
					    APPEND)
		endforeach (SANDBOX_ADD_SOURCE ${ARG_SANDBOX_ADD_SOURCES})
	endif (ARG_SANDBOX_ADD_SOURCES)

	if (BUILD_SHARED OR BUILD_FULL)
		set (CABAL_OPTS "--enable-shared")
	endif (BUILD_SHARED OR BUILD_FULL)

	# This only takes care about building the internal dependencies
	# Everything should already be preinstalled when setting up the shared sandbox
	# Therefore we use the --offline flag to make the build guaranteed independent
	add_custom_command (OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/cabal.sandbox.config"
			    COMMAND ${CABAL_EXECUTABLE} install ${CABAL_OPTS} --only-dependencies --offline -v0 || true
			    APPEND)
endmacro (configure_haskell_sandbox)
