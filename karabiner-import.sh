#!/bin/sh

cli=/Applications/Karabiner.app/Contents/Library/bin/karabiner

$cli set option.pointing_reverse_horizontal_scrolling 1
/bin/echo -n .
$cli set option.pointing_reverse_vertical_scrolling 1
/bin/echo -n .
$cli set parameter.acceleration_of_pointer 3
/bin/echo -n .
$cli set parameter.maximum_speed_of_pointer 100
/bin/echo -n .
$cli set parameter.mousekey_high_speed_of_pointer 150
/bin/echo -n .
$cli set remap.jis_yen2backslash 1
/bin/echo -n .
$cli set remap.jis_yen2backslash_always 1
/bin/echo -n .
$cli set remap.pc_application2commandL 1
/bin/echo -n .
$cli set remap.pointing_relative_middleclick_to_scroll 1
/bin/echo -n .
$cli set repeat.initial_wait 416
/bin/echo -n .
$cli set repeat.wait 33
/bin/echo -n .
/bin/echo
