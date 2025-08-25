# A simple loading example

PIC comes with a super duper cool `find_package` file that allows you to find PIC
as long as the `PIC_ROOT` environment variable is set and if the policy:

```
cmake_policy(SET CMP0144 NEW)
```
is set to "NEW". This is new good cmake behavior. Now simply:

```
mkdir build
cd build
cmake ../
make
./example
```

and you should see:

```
 ========================================
          _____  _____  _____
         |  __ \\|_   _|/ ____|
         | |__) | | | | |
         |  ___/  | | | |
         | |     _| |_| |____
         |_|    |_____|\\_____|

                PIC LIBRARY

 ========================================
 ```
