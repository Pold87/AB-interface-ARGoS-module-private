# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.9

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/volker/Documents/bcvoting

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/volker/Documents/bcvoting/build_antergos

# Include any dependencies generated for this target.
include loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/depend.make

# Include the progress variables for this target.
include loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/progress.make

# Include the compile flags for this target's objects.
include loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/flags.make

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/flags.make
loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o: ../loop_functions/environment_classification_loop_functions/environment_classification_loop_function.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/home/volker/Documents/bcvoting/build_antergos/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o"
	cd /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions && /usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o -c /home/volker/Documents/bcvoting/loop_functions/environment_classification_loop_functions/environment_classification_loop_function.cpp

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.i"
	cd /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /home/volker/Documents/bcvoting/loop_functions/environment_classification_loop_functions/environment_classification_loop_function.cpp > CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.i

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.s"
	cd /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions && /usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /home/volker/Documents/bcvoting/loop_functions/environment_classification_loop_functions/environment_classification_loop_function.cpp -o CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.s

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.requires:

.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.requires

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.provides: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.requires
	$(MAKE) -f loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/build.make loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.provides.build
.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.provides

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.provides.build: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o


# Object files for target environment_classification_loop_functions
environment_classification_loop_functions_OBJECTS = \
"CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o"

# External object files for target environment_classification_loop_functions
environment_classification_loop_functions_EXTERNAL_OBJECTS =

loop_functions/environment_classification_loop_functions/libenvironment_classification_loop_functions.so: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o
loop_functions/environment_classification_loop_functions/libenvironment_classification_loop_functions.so: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/build.make
loop_functions/environment_classification_loop_functions/libenvironment_classification_loop_functions.so: controllers/epuck_environment_classification/libepuck_environment_classification.so
loop_functions/environment_classification_loop_functions/libenvironment_classification_loop_functions.so: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/home/volker/Documents/bcvoting/build_antergos/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX shared module libenvironment_classification_loop_functions.so"
	cd /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/environment_classification_loop_functions.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/build: loop_functions/environment_classification_loop_functions/libenvironment_classification_loop_functions.so

.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/build

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/requires: loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/environment_classification_loop_function.cpp.o.requires

.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/requires

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/clean:
	cd /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions && $(CMAKE_COMMAND) -P CMakeFiles/environment_classification_loop_functions.dir/cmake_clean.cmake
.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/clean

loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/depend:
	cd /home/volker/Documents/bcvoting/build_antergos && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/volker/Documents/bcvoting /home/volker/Documents/bcvoting/loop_functions/environment_classification_loop_functions /home/volker/Documents/bcvoting/build_antergos /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions /home/volker/Documents/bcvoting/build_antergos/loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : loop_functions/environment_classification_loop_functions/CMakeFiles/environment_classification_loop_functions.dir/depend
