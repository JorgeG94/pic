module pic_gpu
   use, intrinsic :: iso_c_binding, only: c_int, c_size_t, c_ptr, &
                                                                c_double, c_float, c_char, c_null_ptr, c_null_char, c_double_complex
   implicit none

   private

   ! in reality, we do not need A LOT of the cuda runtime API
   ! the beauty of open source is that if you need something, make a PR :)
   public :: cudaMalloc
   public :: cudaFree
   public :: cudaMallocAsync
   public :: cudaFreeAsync
   public :: cudaMallocHost
   public :: cudaHostAlloc
   public :: cudaFreeHost
   public :: cudaHostRegister
   public :: cudaHostUnregister
   public :: cudaGetErrorString
   !public :: cudaGetLastError
   !public :: cudaPeekAtLastError
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
   ! public :: cudaMemcpy3D
   ! public :: cudaMemcpy3DAsync
   public :: cudaMemset
   public :: cudaMemsetAsync
   public :: cudaGetDeviceCount
   !public :: cudaGetDeviceProperties
   public :: cudaSetDevice
   public :: cudaGetDevice
   public :: cudaLaunchHostFunc
   public :: cudaDeviceReset
   public :: cudaMemGetInfo

   public :: cudaSuccess

   public :: cudaMemcpyHostToHost
   public :: cudaMemcpyHostToDevice
   public :: cudaMemcpyDeviceToHost
   public :: cudaMemcpyDeviceToDevice
   public :: cudaMemcpyDefault

   public :: cudaMemoryTypeHost
   public :: cudaMemoryTypeDevice
   public :: cudaMemoryTypeUnregistered
   public :: cudaMemoryTypeManaged

   integer(c_int), public, parameter :: cudaHostAllocDefault = int(z'00', c_int)
   integer(c_int), public, parameter :: cudaHostAllocPortable = int(z'01', c_int)
   integer(c_int), public, parameter :: cudaHostAllocMapped = int(z'02', c_int)
   integer(c_int), public, parameter :: cudaHostAllocWriteCombined = int(z'04', c_int)

   integer(c_int), public, parameter :: cudaHostRegisterDefault = 0
   integer(c_int), public, parameter :: cudaHostRegisterPortable = 1
   integer(c_int), public, parameter :: cudaHostRegisterMapped = 2
   integer(c_int), public, parameter :: cudaHostRegisterIoMemory = 4

   integer(c_int), public, parameter :: cudaStreamDefault = int(z'00', c_int)
   integer(c_int), public, parameter :: cudaStreamNonBlocking = int(z'01', c_int)

   integer(c_int), public, parameter :: cudaEventDefault = int(z'00', c_int)
   integer(c_int), public, parameter :: cudaEventBlockingSync = int(z'01', c_int)
   integer(c_int), public, parameter :: cudaEventDisableTiming = int(z'02', c_int)
   integer(c_int), public, parameter :: cudaEventInterprocess = int(z'04', c_int)

   integer(c_int), public, parameter :: cudaMemAttachGlobal = int(z'01', c_int)
   integer(c_int), public, parameter :: cudaMemAttachHost = int(z'02', c_int)
   integer(c_int), public, parameter :: cudaMemAttachSingle = int(z'04', c_int)

   integer(c_int), public, parameter :: cudaCpuDeviceId = -1
   integer(c_int), public, parameter :: cudaInvalidDeviceId = -2

   enum, bind(C)
      enumerator :: cudaMemcpyHostToHost = 0
      enumerator :: cudaMemcpyHostToDevice = 1
      enumerator :: cudaMemcpyDeviceToHost = 2
      enumerator :: cudaMemcpyDeviceToDevice = 3
      enumerator :: cudaMemcpyDefault = 4
   end enum

   enum, bind(C)
      enumerator :: cudaMemoryTypeUnregistered = 0
      enumerator :: cudaMemoryTypeHost = 1
      enumerator :: cudaMemoryTypeDevice = 2
      enumerator :: cudaMemoryTypeManaged = 3
   end enum

   enum, bind(C)
      enumerator :: cudaSuccess = 0
      ! comment means the above variable
      enumerator :: cudaErrorInvalidValue = 1
      enumerator :: cudaErrorMemoryAllocation = 2
      enumerator :: cudaErrorInitializationError = 3
      enumerator :: cudaErrorCudartUnloading = 4
      enumerator :: cudaErrorProfilerDisabled = 5
      enumerator :: cudaErrorProfilerNotInitialized = 6
      !Deprecated^
      enumerator :: cudaErrorProfilerAlreadyStarted = 7
      !    Deprecated
      enumerator :: cudaErrorProfilerAlreadyStopped = 8
      !    Deprecated
      enumerator :: cudaErrorInvalidConfiguration = 9
      enumerator :: cudaErrorInvalidPitchValue = 12
      enumerator :: cudaErrorInvalidSymbol = 13
      enumerator :: cudaErrorInvalidHostPointer = 16
      !   Deprecated
      enumerator :: cudaErrorInvalidDevicePointer = 17
      !   Deprecated
      enumerator :: cudaErrorInvalidTexture = 18
      enumerator :: cudaErrorInvalidTextureBinding = 19
      enumerator :: cudaErrorInvalidChannelDescriptor = 20
      enumerator :: cudaErrorInvalidMemcpyDirection = 21
      enumerator :: cudaErrorAddressOfConstant = 22
      !   Deprecated
      enumerator :: cudaErrorTextureFetchFailed = 23
      !   Deprecated
      enumerator :: cudaErrorTextureNotBound = 24
      !   Deprecated
      enumerator :: cudaErrorSynchronizationError = 25
      !   Deprecated
      enumerator :: cudaErrorInvalidFilterSetting = 26
      enumerator :: cudaErrorInvalidNormSetting = 27
      enumerator :: cudaErrorMixedDeviceExecution = 28
      !   Deprecated
      enumerator :: cudaErrorNotYetImplemented = 31
      !   Deprecated
      enumerator :: cudaErrorMemoryValueTooLarge = 32
      !   Deprecated
      enumerator :: cudaErrorStubLibrary = 34
      enumerator :: cudaErrorInsufficientDriver = 35
      enumerator :: cudaErrorCallRequiresNewerDriver = 36
      enumerator :: cudaErrorInvalidSurface = 37
      enumerator :: cudaErrorDuplicateVariableName = 43
      enumerator :: cudaErrorDuplicateTextureName = 44
      enumerator :: cudaErrorDuplicateSurfaceName = 45
      enumerator :: cudaErrorDevicesUnavailable = 46
      enumerator :: cudaErrorIncompatibleDriverContext = 49
      enumerator :: cudaErrorMissingConfiguration = 52
      enumerator :: cudaErrorPriorLaunchFailure = 53
      ! Deprecated
      enumerator :: cudaErrorLaunchMaxDepthExceeded = 65
      enumerator :: cudaErrorLaunchFileScopedTex = 66
      enumerator :: cudaErrorLaunchFileScopedSurf = 67
      enumerator :: cudaErrorSyncDepthExceeded = 68
      enumerator :: cudaErrorLaunchPendingCountExceeded = 69
      enumerator :: cudaErrorInvalidDeviceFunction = 98
      enumerator :: cudaErrorNoDevice = 100
      enumerator :: cudaErrorInvalidDevice = 101
      enumerator :: cudaErrorDeviceNotLicensed = 102
      enumerator :: cudaErrorSoftwareValidityNotEstablished = 103
      enumerator :: cudaErrorStartupFailure = 127
      enumerator :: cudaErrorInvalidKernelImage = 200
      enumerator :: cudaErrorDeviceUninitialized = 201
      enumerator :: cudaErrorMapBufferObjectFailed = 205
      enumerator :: cudaErrorUnmapBufferObjectFailed = 206
      enumerator :: cudaErrorArrayIsMapped = 207
      enumerator :: cudaErrorAlreadyMapped = 208
      enumerator :: cudaErrorNoKernelImageForDevice = 209
      enumerator :: cudaErrorAlreadyAcquired = 210
      enumerator :: cudaErrorNotMapped = 211
      enumerator :: cudaErrorNotMappedAsArray = 212
      enumerator :: cudaErrorNotMappedAsPointer = 213
      enumerator :: cudaErrorECCUncorrectable = 214
      enumerator :: cudaErrorUnsupportedLimit = 215
      enumerator :: cudaErrorDeviceAlreadyInUse = 216
      enumerator :: cudaErrorPeerAccessUnsupported = 217
      enumerator :: cudaErrorInvalidPtx = 218
      enumerator :: cudaErrorInvalidGraphicsContext = 219
      enumerator :: cudaErrorNvlinkUncorrectable = 220
      enumerator :: cudaErrorJitCompilerNotFound = 221
      enumerator :: cudaErrorUnsupportedPtxVersion = 222
      enumerator :: cudaErrorJitCompilationDisabled = 223
      enumerator :: cudaErrorUnsupportedExecAffinity = 224
      enumerator :: cudaErrorUnsupportedDevSideSync = 225
      enumerator :: cudaErrorContained = 226
      enumerator :: cudaErrorInvalidSource = 300
      enumerator :: cudaErrorFileNotFound = 301
      enumerator :: cudaErrorSharedObjectSymbolNotFound = 302
      enumerator :: cudaErrorSharedObjectInitFailed = 303
      enumerator :: cudaErrorOperatingSystem = 304
      enumerator :: cudaErrorInvalidResourceHandle = 400
      enumerator :: cudaErrorIllegalState = 401
      enumerator :: cudaErrorLossyQuery = 402
      enumerator :: cudaErrorSymbolNotFound = 500
      enumerator :: cudaErrorNotReady = 600
      enumerator :: cudaErrorIllegalAddress = 700
      enumerator :: cudaErrorLaunchOutOfResources = 701
      enumerator :: cudaErrorLaunchTimeout = 702
      enumerator :: cudaErrorLaunchIncompatibleTexturing = 703
      enumerator :: cudaErrorPeerAccessAlreadyEnabled = 704
      enumerator :: cudaErrorPeerAccessNotEnabled = 705
      enumerator :: cudaErrorSetOnActiveProcess = 708
      enumerator :: cudaErrorContextIsDestroyed = 709
      enumerator :: cudaErrorAssert = 710
      enumerator :: cudaErrorTooManyPeers = 711
      enumerator :: cudaErrorHostMemoryAlreadyRegistered = 712
      enumerator :: cudaErrorHostMemoryNotRegistered = 713
      enumerator :: cudaErrorHardwareStackError = 714
      enumerator :: cudaErrorIllegalInstruction = 715
      enumerator :: cudaErrorMisalignedAddress = 716
      enumerator :: cudaErrorInvalidAddressSpace = 717
      enumerator :: cudaErrorInvalidPc = 718
      enumerator :: cudaErrorLaunchFailure = 719
      enumerator :: cudaErrorCooperativeLaunchTooLarge = 720
      enumerator :: cudaErrorTensorMemoryLeak = 721
      enumerator :: cudaErrorNotPermitted = 800
      enumerator :: cudaErrorNotSupported = 801
      enumerator :: cudaErrorSystemNotReady = 802
      enumerator :: cudaErrorSystemDriverMismatch = 803
      enumerator :: cudaErrorCompatNotSupportedOnDevice = 804
      enumerator :: cudaErrorMpsConnectionFailed = 805
      enumerator :: cudaErrorMpsRpcFailure = 806
      enumerator :: cudaErrorMpsServerNotReady = 807
      enumerator :: cudaErrorMpsMaxClientsReached = 808
      enumerator :: cudaErrorMpsMaxConnectionsReached = 809
      enumerator :: cudaErrorMpsClientTerminated = 810
      enumerator :: cudaErrorCdpNotSupported = 811
      enumerator :: cudaErrorCdpVersionMismatch = 812
      enumerator :: cudaErrorStreamCaptureUnsupported = 900
      enumerator :: cudaErrorStreamCaptureInvalidated = 901
      enumerator :: cudaErrorStreamCaptureMerge = 902
      enumerator :: cudaErrorStreamCaptureUnmatched = 903
      enumerator :: cudaErrorStreamCaptureUnjoined = 904
      enumerator :: cudaErrorStreamCaptureIsolation = 905
      enumerator :: cudaErrorStreamCaptureImplicit = 906
      enumerator :: cudaErrorCapturedEvent = 907
      enumerator :: cudaErrorStreamCaptureWrongThread = 908
      enumerator :: cudaErrorTimeout = 909
      enumerator :: cudaErrorGraphExecUpdateFailure = 910
      enumerator :: cudaErrorExternalDevice = 911
      enumerator :: cudaErrorInvalidClusterSize = 912
      enumerator :: cudaErrorFunctionNotLoaded = 913
      enumerator :: cudaErrorInvalidResourceType = 914
      enumerator :: cudaErrorInvalidResourceConfiguration = 915
      enumerator :: cudaErrorUnknown = 999
      enumerator :: cudaErrorApiFailureBase = 10000
   end enum

   type, bind(c), public :: cudaStream_t
      type(c_ptr) :: stream
   end type cudaStream_t

   type, bind(c), public :: cudaEvent_t
      type(c_ptr) :: event
   end type cudaEvent_t

   type, bind(c), public :: cudaPointerAttribute_t
      integer(kind(cudaMemoryTypeHost)) :: mem_type
      integer(c_int) :: device
      type(c_ptr) :: device_pointer
      type(c_ptr) :: host_pointer
      integer(c_int) :: is_manager
   end type cudaPointerAttribute_t

   interface

      integer(kind(cudaSuccess)) function cudaMalloc(ptr, size) bind(C, name="cudaMalloc")
      !!  cudaError_t cudaMalloc ( void** devPtr, size_t size )
         import :: c_ptr, c_size_t, c_int, cudaSuccess
         implicit none
         type(c_ptr) :: ptr
         integer(c_size_t), value :: size
      end function cudaMalloc

      integer(kind(cudaSuccess)) function cudaFree(ptr) bind(C, name="cudaFree")
         import :: c_ptr, c_int, cudaSuccess
         implicit none
         type(c_ptr), value :: ptr
      end function cudaFree

      integer(kind(cudaSuccess)) function cudaMallocAsync(ptr, size, stream) bind(C, name="cudaMallocAsync")
         import :: c_ptr, c_size_t, c_int, cudaSuccess, cudaStream_t
         implicit none
         type(c_ptr) :: ptr
         integer(c_size_t), value :: size
         type(cudaStream_t), value :: stream
      end function cudaMallocAsync

      integer(kind(cudaSuccess)) function cudaFreeAsync(ptr, stream) bind(C, name="cudaFreeAsync")
         import :: c_ptr, c_int, cudaSuccess, cudaStream_t
         implicit none
         type(c_ptr), value :: ptr
         type(cudaStream_t), value :: stream
      end function cudaFreeAsync

      integer(kind(cudaSuccess)) function cudaHostAlloc(ptr, size, flags) bind(C, name="cudaHostAlloc")
         import :: c_ptr, c_size_t, c_int, cudaSuccess
         implicit none
         type(c_ptr) :: ptr
         integer(c_size_t), value :: size
         integer(c_int), value :: flags
      end function cudaHostAlloc

      integer(kind(cudaSuccess)) function cudaFreeHost(ptr) bind(C, name="cudaFreeHost")
         import :: c_ptr, c_int, cudaSuccess
         implicit none
         type(c_ptr), value :: ptr
      end function cudaFreeHost

      integer(kind(cudaSuccess)) function cudaHostRegister(ptr, size, flags) bind(C, name="cudaHostRegister")
         import :: c_ptr, c_size_t, c_int, cudaSuccess
         implicit none
         type(c_ptr), value :: ptr
         integer(c_size_t), value :: size
         integer(c_int), value :: flags
      end function cudaHostRegister

      integer(kind(cudaSuccess)) function cudaHostUnregister(ptr) bind(C, name="cudaHostUnregister")
         import :: c_ptr, c_int, cudaSuccess
         implicit none
         type(c_ptr), value :: ptr
      end function cudaHostUnregister

      type(c_ptr) function cudaGetErrorString(error) bind(C, name="cudaGetErrorString")
         import :: c_int, c_ptr
         implicit none
         integer(c_int), value :: error
      end function cudaGetErrorString

      integer(kind(cudaSuccess)) function cudaDeviceSynchronize() bind(C, name="cudaDeviceSynchronize")
         import :: cudaSuccess
         implicit none
      end function cudaDeviceSynchronize

      integer(kind(cudaSuccess)) function cudaStreamCreate(stream) bind(C, name="cudaStreamCreate")
         import :: cudaStream_t, cudaSuccess
         implicit none
         type(cudaStream_t) :: stream
      end function cudaStreamCreate

      integer(kind(cudaSuccess)) function cudaStreamCreateWithFlags(stream, flags) bind(C, name="cudaStreamCreateWithFlags")
         import :: cudaStream_t, cudaSuccess, c_int
         implicit none
         type(cudaStream_t)  :: stream
         integer(c_int), value :: flags
      end function cudaStreamCreateWithFlags

      integer(kind(cudaSuccess)) function cudaStreamDestroy(stream) bind(C, name="cudaStreamDestroy")
         import :: cudaStream_t, cudaSuccess
         implicit none
         type(cudaStream_t) :: stream
      end function cudaStreamDestroy

      integer(kind(cudaSuccess)) function cudaStreamSynchronize(stream) bind(C, name="cudaStreamSynchronize")
         import :: cudaStream_t, cudaSuccess
         implicit none
         type(cudaStream_t) :: stream
      end function cudaStreamSynchronize

      integer(kind(cudaSuccess)) function cudaStreamWaitEvent(stream, event, flags) bind(C, name="cudaStreamWaitEvent")
         import :: cudaStream_t, cudaEvent_t, cudaSuccess, c_int
         implicit none
         type(cudaStream_t) :: stream
         type(cudaEvent_t) :: event
         integer(c_int), value :: flags
      end function cudaStreamWaitEvent

      integer(kind(cudaSuccess)) function cudaEventCreate(event) bind(C, name="cudaEventCreate")
         import :: cudaEvent_t, cudaSuccess
         implicit none
         type(cudaEvent_t) :: event
      end function cudaEventCreate

      integer(kind(cudaSuccess)) function cudaEventDestroy(event) bind(C, name="cudaEventDestroy")
         import :: cudaEvent_t, cudaSuccess
         implicit none
         type(cudaEvent_t) :: event
      end function cudaEventDestroy

      integer(kind(cudaSuccess)) function cudaEventRecord(event, stream) bind(C, name="cudaEventRecord")
         import :: cudaEvent_t, cudaStream_t, cudaSuccess
         implicit none
         type(cudaEvent_t) :: event
         type(cudaStream_t), value :: stream
      end function cudaEventRecord

      integer(kind(cudaSuccess)) function cudaEventSynchronize(event) bind(C, name="cudaEventSynchronize")
         import :: cudaEvent_t, cudaSuccess
         implicit none
         type(cudaEvent_t) :: event
      end function cudaEventSynchronize

      integer(kind(cudaSuccess)) function cudaEventElapsedTime(elapsed, start, stop) bind(C, name="cudaEventElapsedTime")
         import :: c_float, cudaEvent_t, cudaSuccess
         implicit none
         real(c_float) :: elapsed
         type(cudaEvent_t) :: start
         type(cudaEvent_t) :: stop
      end function cudaEventElapsedTime

      integer(kind(cudaSuccess)) function cudaMemcpy(dst, src, count, kind) bind(C, name="cudaMemcpy")
         import :: c_ptr, c_size_t, c_int, cudaSuccess
         implicit none
         type(c_ptr) :: dst
         type(c_ptr) :: src
         integer(c_size_t), value :: count
         integer(c_int), value :: kind
      end function cudaMemcpy
      integer(kind(cudaSuccess)) function cudaMemcpyAsync(dst, src, count, kind, stream) bind(C, name="cudaMemcpyAsync")
         import :: c_ptr, c_size_t, c_int, cudaSuccess, cudaStream_t
         implicit none
         type(c_ptr) :: dst
         type(c_ptr) :: src
         integer(c_size_t), value :: count
         integer(c_int), value :: kind
         type(cudaStream_t), value :: stream
      end function cudaMemcpyAsync

      integer(kind(cudaSuccess)) function cudaMemcpy2D(dst, dpitch, src, spitch, width, height, kind) bind(C, name="cudaMemcpy2D")
         import :: c_ptr, c_size_t, c_int, cudaSuccess
         implicit none
         type(c_ptr) :: dst
         integer(c_size_t), value :: dpitch
         type(c_ptr) :: src
         integer(c_size_t), value :: spitch
         integer(c_size_t), value :: width
         integer(c_size_t), value :: height
         integer(c_int), value :: kind
      end function cudaMemcpy2D

      integer(kind(cudaSuccess)) function cudaMemcpy2DAsync(dst, dpitch, src, spitch, width, height, kind, stream) bind(C, name="cudaMemcpy2DAsync")
         import :: c_ptr, c_size_t, c_int, cudaSuccess, cudaStream_t
         implicit none
         type(c_ptr) :: dst
         integer(c_size_t), value :: dpitch
         type(c_ptr) :: src
         integer(c_size_t), value :: spitch
         integer(c_size_t), value :: width
         integer(c_size_t), value :: height
         integer(c_int), value :: kind
         type(cudaStream_t), value :: stream
      end function cudaMemcpy2DAsync

      integer(kind(cudaSuccess)) function cudaMemset(ptr, value, count) bind(C, name="cudaMemset")
         import :: c_ptr, c_int, c_size_t, cudaSuccess
         implicit none
         type(c_ptr) :: ptr
         integer(c_int), value :: value
         integer(c_size_t), value :: count
      end function cudaMemset

      integer(kind(cudaSuccess)) function cudaMemsetAsync(ptr, value, count, stream) bind(C, name="cudaMemsetAsync")
         import :: c_ptr, c_int, c_size_t, cudaSuccess, cudaStream_t
         implicit none
         type(c_ptr) :: ptr
         integer(c_int), value :: value
         integer(c_size_t), value :: count
         type(cudaStream_t), value :: stream
      end function cudaMemsetAsync

      integer(kind(cudaSuccess)) function cudaGetDeviceCount(count) bind(C, name="cudaGetDeviceCount")
         import :: c_int, cudaSuccess
         implicit none
         integer(c_int) :: count
      end function cudaGetDeviceCount

      !integer(kind(cudaSuccess)) function cudaGetDeviceProperties(prop, device) bind(C, name="cudaGetDeviceProperties")
      !   import :: c_int, cudaSuccess
      !   implicit none
      !   type(c_ptr) :: prop
      !   integer(c_int), value :: device
      !end function cudaGetDeviceProperties

      integer(kind(cudaSuccess)) function cudaSetDevice(device) bind(C, name="cudaSetDevice")
         import :: c_int, cudaSuccess
         implicit none
         integer(c_int), value :: device
      end function cudaSetDevice

      integer(kind(cudaSuccess)) function cudaGetDevice(device) bind(C, name="cudaGetDevice")
         import :: c_int, cudaSuccess
         implicit none
         integer(c_int) :: device
      end function cudaGetDevice

      integer(kind(cudaSuccess)) function cudaLaunchHostFunc(stream, func, arg) bind(C, name="cudaLaunchHostFunc")
         import :: cudaStream_t, c_ptr, cudaSuccess
         implicit none
         type(cudaStream_t) :: stream
         procedure(c_ptr), pointer :: func
         type(c_ptr), value :: arg
      end function cudaLaunchHostFunc

      integer(kind(cudaSuccess)) function cudaDeviceReset() bind(C, name="cudaDeviceReset")
         import :: cudaSuccess
         implicit none
      end function cudaDeviceReset

      integer(kind(cudaSucess)) function cudaMemGetInfo(free, total) bind(C, name="cudaMemGetInfo")
         import :: cudaSuccess, c_size_t
         implicit none
         integer(c_size_t) :: free
         integer(c_size_t) :: total
         !integer(kind(cudaSuccess)) :: cudaMemGetInfo
      end function cudaMemGetInfo

   end interface

end module pic_gpu
