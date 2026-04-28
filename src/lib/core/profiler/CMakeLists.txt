target_sources(${core_lib} PRIVATE pic_profiler.F90)

if(PIC_DISABLE_PROFILER)
  target_compile_definitions(${core_lib} PRIVATE PIC_DISABLE_PROFILER)
endif()

if(PIC_USE_NVTX)
  target_compile_definitions(${core_lib} PRIVATE PIC_USE_NVTX)
endif()
