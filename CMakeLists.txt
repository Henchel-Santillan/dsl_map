# Build
cmake_minimum_required(VERSION 3.14...3.19)
project(ds_map
        VERSION 1.0.0
        DESCRIPTION "stl-compatible hash map data structure"
        LANGUAGES CXX)

if (CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME)
    set(CMAKE_CXX_STANDARD 20)
    set(CMAKE_CXX_EXTENSIONS OFF)
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
endif()

include(GNUInstallDirs)

add_library(${PROJECT_NAME} INTERFACE)
target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_20)

target_include_directories(${PROJECT_NAME} INTERFACE
        $<BUILD_INTERFACE:${${PROJECT_NAME}_SOURCE_DIR}>
        $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>)


# Test (googletest)
include(FetchContent)
FetchContent_Declare(googletest
        URL https://github.com/google/googletest/archive/609281088cfefc76f9d0ce82e1ff6c30cc3591e5.zip)
set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(googletest)


# Install
install(TARGETS ${PROJECT_NAME}
        EXPORT ${PROJECT_NAME}_Targets
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})

include(CMakePackageConfigHelpers)
write_basic_package_version_file("${PROJECT_NAME}ConfigVersion.cmake"
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion)

if (NOT INCLUDE_INSTALL_DIR)
    set(INCLUDE_INSTALL_DIR include/)
endif()

configure_package_config_file (
        "cmake/${PROJECT_NAME}Config.cmake.in"
        "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        INSTALL_DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake
        PATH_VARS INCLUDE_INSTALL_DIR)

install(EXPORT ${PROJECT_NAME}_Targets
        FILE ${PROJECT_NAME}Targets.cmake
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(FILES "${PROJECT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        "${PROJECT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
        DESTINATION ${CMAKE_INSTALL_DATAROOTDIR}/${PROJECT_NAME}/cmake)

install(DIRECTORY ${PROJECT_SOURCE_DIR}/include/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/${PROJECT_NAME})