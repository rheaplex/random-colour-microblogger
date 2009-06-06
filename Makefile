# Makefile -  The Makefile for random-colour-microblogger.
# Copyright (C) 2009  Rob Myers rob@robmyers.org
#
# This file is part of random-colour-microblogger.
#
# random-colour-microblogger is free software; you can redistribute it and/or 
# modify it under the terms of the GNU General Public License as published 
# by the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# random-colour-microblogger is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

all: random-colour-microblogger

random-colour-microblogger: random-colour-microblogger.lisp
	sbcl --noinform --load "random-colour-microblogger.lisp" \
	--eval "(sb-ext:save-lisp-and-die \"random-colour-microblogger\" \
		:executable t :toplevel #'run)"

clean:
	rm -f random-colour-microblogger
	rm -f *.fasl

distclean: clean

