! SPDX-License-Identifier: MIT
! Copyright (c) 2025 Jorge Luis Galvez Vallejo
!! General cuda/hip agnostic module to interface to GPU runtimes
module pic_gpu_runtime
  !! gpu runtime interfaces via iso_c
   use iso_c_binding, only: c_int, c_size_t
   implicit none

   private

   public :: gpugetmeminfo, gpugetdevicecount, gpugetdevice

#ifdef HAVE_CUDA
   interface
      function cudaMemGetInfo(freeMem, totalMem) bind(C, name="cudaMemGetInfo")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: cudaMemGetInfo
         integer(c_size_t), intent(out) :: freeMem, totalMem
      end function cudaMemGetInfo
      function cudaGetDevice(device) bind(C, name="cudaGetDevice")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: cudaGetDevice
         integer(c_int), intent(out) :: device
      end function cudaGetDevice
      function cudaGetDeviceCount(count) bind(C, name="cudaGetDeviceCount")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: cudaGetDeviceCount
         integer(c_int), intent(out) :: count
      end function cudaGetDeviceCount
   end interface

#elif defined(HAVE_HIP)
   interface
      function hipMemGetInfo(freeMem, totalMem) bind(C, name="hipMemGetInfo")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: hipMemGetInfo
         integer(c_size_t), intent(out) :: freeMem, totalMem
      end function hipMemGetInfo
      function hipGetDevice(device) bind(C, name="hipGetDevice")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: hipGetDevice
         integer(c_int), intent(out) :: device
      end function hipGetDevice
      function hipGetDeviceCount(count) bind(C, name="hipGetDeviceCount")
         use iso_c_binding, only: c_int, c_size_t
         implicit none
         integer(c_int) :: hipGetDeviceCount
         integer(c_int), intent(out) :: count
      end function hipGetDeviceCount
   end interface

#endif

contains

   subroutine gpugetmeminfo(freeMem, totalMem, ierr)
    !! get free and total memory from the GPU runtime
      integer(c_size_t), intent(out) :: freeMem, totalMem
      integer(c_int), intent(out) :: ierr
#ifdef HAVE_CUDA
      ierr = cudaMemGetInfo(freeMem, totalMem)
#elif defined(HAVE_HIP)
      ierr = hipMemGetInfo(freeMem, totalMem)
#else
      freeMem = 0_c_size_t
      totalMem = 0_c_size_t
      ierr = -1
#endif
   end subroutine gpugetmeminfo

   subroutine gpugetdevice(device_id, ierr)
    !! get current device id from the GPU runtime
      integer(c_int), intent(out) :: device_id, ierr
#ifdef HAVE_CUDA
      ierr = cudaGetDevice(device_id)
#elif defined(HAVE_HIP)
      ierr = hipGetDevice(device_id)
#else
      ierr = -1_c_int
      device_id = -1_c_int
#endif
   end subroutine gpugetdevice

   subroutine gpugetdevicecount(device_count, ierr)
    !! get device count available from the GPU runtime
      integer(c_int), intent(out) :: device_count, ierr
#ifdef HAVE_CUDA
      ierr = cudaGetDeviceCount(device_count)
#elif defined(HAVE_HIP)
      ierr = hipGetDeviceCount(device_count)
#else
      ierr = -1_c_int
      device_count = 0_c_int
#endif
   end subroutine gpugetdevicecount

end module pic_gpu_runtime
