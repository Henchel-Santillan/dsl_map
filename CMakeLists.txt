cmake_minimum_required(VERSION 3.3)
project(dsl_map VERSION 1.0.0 LANGUAGES CXX)

include(GNUInstallDirs)

add_library(dsl_map INTERFACE)

# Use dsl::chain_map as the target
add_library(dsl::chain_map ALIAS dsl_map)
target_include_directories(dsl_map INTERFACE 
                           "$<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>"
                           "$<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>")

list(APPEND headers "${CMAKE_CURRENT_SOURCE_DIR}/include/chain_map.h"
                    "${CMAKE_CURRENT_SOURCE_DIR}/include/set.h")
target_sources(dsl_map INTERFACE "$<BUILD_INTERFACE:${headers}>")

# Add GoogleTest (hash is updatable and can be configured depending on needed version)
include(FetchContent)
FetchContent_Declare(googletest 
                     URL https://github.com/google/googletest/archive/8ded48c37be09d8cc3665af1b414c5d53c0862e7.zip)     # Commit 3893, Apr. 28 2022

# For Windows: Prevent overriding the parent project's compiler/linker settings
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)

if (${CMAKE_VERSION} VERSION_GREATER "3.2")
        include (CMakePackageConfigHelpers)
        install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/include"
                DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")

        configure_package_config_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/dsl_mapConfig.cmake.in"
                                      "${CMAKE_CURRENT_BINARY_DIR}/dsl_mapConfig.cmake"
                                      INSTALL_DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/cmake/dsl_map")

        install(FILES "${CMAKE_CURRENT_BINARY_DIR}/dsl_mapConfig.cmake"
                DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/cmake/dsl_map")

        export(TARGETS dsl_map NAMESPACE dsl:: FILE "${CMAKE_CURRENT_BINARY_DIR}/dsl_mapTargets.cmake")

        install(TARGETS dsl_map 
                EXPORT dsl_mapTargets)
        install(EXPORT dsl_mapTargets
                NAMESPACE dsl::
                DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/cmake/dsl_map")
        
        set(CMAKE_SIZEOF_VOID_P_BACKUP ${CMAKE_SIZEOF_VOID_P})
        unset(CMAKE_SIZEOF_VOID_P)

        write_basic_package_version_file("${CMAKE_CURRENT_BINARY_DIR}/dsl_mapConfigVersion.cmake"
                                        COMPATIBILITY SameMajorVersion)
        set(CMAKE_SIZEOF_VOID_P ${CMAKE_SIZEOF_VOID_P_BACKUP})

        install(FILES "${CMAKE_CURRENT_BINARY_DIR}/dsl_mapConfigVersion.cmake"
                DESTINATION "${CMAKE_INSTALL_DATAROOTDIR}/cmake/dsl_map")
endif()
