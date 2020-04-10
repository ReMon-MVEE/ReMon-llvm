add_custom_target(sync)

set(SYNC_SOURCES
  libsync.cpp)

set(x86_64_SOURCES
  ${SYNC_SOURCES})

set(i386_SOURCES
  ${SYNC_SOURCES})

set(i686_SOURCES
  ${i386_SOURCES})

#include_directories(..)

set(SYNC_CFLAGS ${SANITIZER_COMMON_CFLAGS})

#add_compiler_rt_component(sync)

foreach(arch ${SYNC_SUPPORTED_ARCH})
  if(CAN_TARGET_${arch})
    add_compiler_rt_runtime(clang_rt.sync
     SHARED
     ARCHS ${arch}
     SOURCES ${${arch}_SOURCES}
     CFLAGS ${SYNC_CFLAGS}
     PARENT_TARGET sync)
  endif()
endforeach()
