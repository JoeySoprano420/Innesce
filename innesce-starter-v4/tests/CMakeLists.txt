add_executable(test_durations tests/test_durations.cpp)
target_link_libraries(test_durations PRIVATE innesce_core)
add_test(NAME durations COMMAND test_durations)

add_executable(test_truth tests/test_truth.cpp)
target_link_libraries(test_truth PRIVATE innesce_core)
add_test(NAME truth COMMAND test_truth)

add_executable(test_quarantine tests/test_quarantine.cpp)
target_link_libraries(test_quarantine PRIVATE innesce_core)
add_test(NAME quarantine COMMAND test_quarantine)

add_executable(test_lanes tests/test_lanes.cpp)
target_link_libraries(test_lanes PRIVATE innesce_core)
add_test(NAME lanes COMMAND test_lanes)
