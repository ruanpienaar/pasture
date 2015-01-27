#!/bin/bash
erl -name debug_shell -setcookie pasture_dev -remsh $1
