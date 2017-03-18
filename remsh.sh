#!/bin/bash
erl -name debug_shell -setcookie pasture -remsh $1
