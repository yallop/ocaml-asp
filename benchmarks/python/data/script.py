#! /usr/bin/python3

# Copyright (C) 2012 Colin Watson <cjwatson@ubuntu.com>.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

"""Trivial script(1) workalike, but without reading from standard input."""

import os
import pty
import select
import sys

filename = sys.argv[1]
command = sys.argv[2]

pid, master = pty.fork()
if pid == 0:  # child
    os.execlp("sh", "sh", "-c", command)

# parent
with open(filename, "wb") as logfile:
    try:
        while True:
            rfds, _, _ = select.select([master], [], [])
            if master in rfds:
                data = os.read(master, 65536)
                os.write(1, data)
                logfile.write(data)
                logfile.flush()
    except (IOError, OSError):
        pass

pid, status = os.wait()
returncode = 0
if os.WIFSIGNALED(status):
    returncode = -os.WTERMSIG(status)
elif os.WIFEXITED(status):
    returncode = os.WEXITSTATUS(status)
else:
    # Should never happen
    raise RuntimeError("Unknown child exit status!")
os.close(master)
sys.exit(returncode)
