#!/usr/bin/env bash

id=$(wmctrl -l | grep env | cut -d ' ' -f1)
xprop -id $id -f _NET_WM_STATE 32a -set _NET_WM_STATE _NET_WM_STATE_SKIP_TASKBAR
