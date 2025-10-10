!! device handling routines
module pic_device
  !! contains the general routines that interface to the pic_gpu_runtime module
  !! there are not C interfaces in this file
   use iso_c_binding, only: c_int, c_size_t
   use pic_types, only: dp, int32
   use pic_gpu_runtime, only: gpugetmeminfo, gpugetdevice, gpugetdevicecount
   implicit none

   private

   public :: pic_device_type
   public :: to_string
   public :: get_gpu_information
   public :: get_device_id
   public :: get_gpu_memory_info

   type :: pic_device_type
    !! general device container, contains the id, free, total , and used memory
      real(dp) :: free_mb = 0.0_dp
      real(dp) :: total_mb = 0.0_dp
      real(dp) :: used_mb = 0.0_dp
      integer(c_int) :: device_id = -1_c_int
   contains
      procedure, non_overridable :: get_device_info => get_gpu_information
   end type pic_device_type

   interface to_string
    !! convenient way to print the pic_device_type using to_string(my_device)
      procedure :: to_string_device
   end interface

contains

   subroutine get_gpu_information(self)
    !! call my_device%get_gpu_informatio() style subroutine to populate the object
      class(pic_device_type), intent(inout) :: self

      call get_gpu_memory_info(self)
      call get_device_id(self)

   end subroutine get_gpu_information

   function to_string_device(self) result(str)
    !! cute printing routine for the pic device type, transforms the contents into a
    !! string of chars
      class(pic_device_type), intent(in) :: self
      character(len=:), allocatable :: str
      character(len=100) :: temp_str
      integer(int32) :: total_len

      total_len = len("Device ID:   ") + 6 + &
                  len("Free memory: ") + 20 + &
                  len("Total memory:") + 20 + &
                  len("Used memory: ") + 20 + 3*len(new_line('a'))

      allocate (character(len=total_len) :: str)

      write (temp_str, '(I0)') self%device_id
      str = "Device ID:   "//trim(temp_str)//new_line('a')

      write (temp_str, '(F10.2)') self%free_mb
      str = str//"Free memory: "//trim(adjustl(temp_str))//" MB"//new_line('a')

      write (temp_str, '(F10.2)') self%total_mb
      str = str//"Total memory: "//trim(adjustl(temp_str))//" MB"//new_line('a')

      write (temp_str, '(F10.2)') self%used_mb
      str = str//"Used memory:  "//trim(adjustl(temp_str))//" MB"
   end function to_string_device

   subroutine get_device_id(mem)
    !! routine to get the device id for the device_type
      type(pic_device_type), intent(inout) :: mem
      integer(c_int) :: ierr, device_id

      call gpugetdevice(device_id, ierr)

      if (ierr == 0_c_int) then
         mem%device_id = device_id
      else
         mem%device_id = -1_c_int
      end if

   end subroutine get_device_id

   subroutine get_gpu_memory_info(mem)
    !! routine to get the free and total memory for the device_type object
    !! can be used independently with call get_gpu_memory_info(device_object)
      type(pic_device_type), intent(inout) :: mem
      integer(c_size_t) :: freeMem, totalMem
      integer(c_int)    :: ierr

      call gpugetmeminfo(freeMem, totalMem, ierr)

      if (ierr == 0_c_int) then
         mem%free_mb = real(freeMem, kind=dp)/1024.0_dp/1024.0_dp
         mem%total_mb = real(totalMem, kind=dp)/1024.0_dp/1024.0_dp
         mem%used_mb = mem%total_mb - mem%free_mb
      else
         mem = pic_device_type()   ! zero it
      end if

   end subroutine get_gpu_memory_info

end module pic_device
