#!/bin/bash
synclient TouchpadOff=$(synclient -l | grep -c 'TouchpaOff.*=.*0')
