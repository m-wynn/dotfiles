#!/bin/sh

import -window root /tmp/screenshot.png
corruster /tmp/screenshot.png /tmp/screenshot.png
i3lock -i /tmp/screenshot.png
