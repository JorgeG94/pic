module pic_gpu
   use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_ptr, &
                                                                c_double, c_float, c_char, c_null_ptr, c_null_char, c_double_complex
   implicit none

   private
#ifdef HAVE_CUDA

   ! in reality, we do not need A LOT of the cuda runtime API
   ! the beauty of open source is that if you need something, make a PR :)
   public :: cudaMalloc
   public :: cudaFree
   public :: cudaMallocAsync
   public :: cudaFreeAsync
   public :: cudaHostAlloc
   public :: cudaFreeHost
   public :: cudaHostRegister
   public :: cudaHostUnregister
   public :: cudaGetErrorString
   public :: cudaGetLastError
   public :: cudaPeekAtLastError
   public :: cudaDeviceSynchronize
   public :: cudaStreamCreate
   public :: cudaStreamCreateWithFlags
   public :: cudaStreamDestroy
   public :: cudaStreamSynchronize
   public :: cudaStreamWaitEvent
   public :: cudaEventCreate
   public :: cudaEventDestroy
   public :: cudaEventRecord
   public :: cudaEventSynchronize
   public :: cudaEventElapsedTime
   public :: cudaMemcpy
   public :: cudaMemcpyAsync
   public :: cudaMemcpy2D
   public :: cudaMemcpy2DAsync
   public :: cudaMemcpy3D
   public :: cudaMemcpy3DAsync
   public :: cudaMemset
   public :: cudaMemsetAsync
   public :: cudaGetDeviceCount
   public :: cudaGetDeviceProperties
   public :: cudaSetDevice
   public :: cudaGetDevice
   public :: cudaLaunchHostFunc
   public :: cudaDeviceReset

   public :: cudaSuccess

   public :: cudaMemcpyHostToDevice
   public :: cudaMemcpyDeviceToHost
   public :: cudaMemcpyDeviceToDevice
   public :: cudaMemcpyHostToHost

   public :: cudaMemoryTypeHost
   public :: cudaMemoryTypeDevice
   public :: cudaMemoryTypeUnregistered
   public :: cudaMemoryTypeManaged

#endif

end module pic_gpu
