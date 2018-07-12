#!/bin/bash
kill -9 `pgrep resonera-exe`
stack exec resonera-exe &
sleep 0.6
echo -e "server running with PID={`pgrep resonera-exe`} (if that's empty, then the server is not running)"