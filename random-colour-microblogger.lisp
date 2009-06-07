;; randon-colour-microblogger.lisp - Microblog random colours.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This file is part of microblog-bot
;;
;; microblog-bot is free software; you can redistribute it and/or 
;; modify it under the terms of the GNU General Public License as published 
;; by the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; microblog-bot is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'asdf)
(asdf:oos 'asdf:load-op 'cl-twit)

(defmethod maybe (fun &key (probability 0.5) (default nil))
  "Call fun with args if random(0..1) is less than probability."
  (if (< (random 1.0) probability)
      (funcall fun)
    default))

(defmethod choose-one-of ((choices list))
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defmethod maybe-choose-one-of ((choices list) &key (probability 0.5))
  "Choose one of the list or return nil."
  (maybe #'(lambda () (choose-one-of choices)) :probability probability))

(defmethod call-one-of ((choices list))
  (funcall (choose-one-of choices)))

(defmethod maybe-call-one-of ((choices list))
  (maybe (choose-one-of choices)))

(defmethod join-strings (&rest strings)
  "Concatenate a list of strings."
  (let ((all (or (car strings) "")))
    (dolist (s (cdr strings))
      (when (not (or (not s) (equal s "")))
	(setf all (concatenate 'string all
			       (if (equal all "")
				   ""
				 " ") 
			       s))))
    all))

(defun hue ()
  (choose-one-of '("red" "orange" "yellow" "green" "blue" "purple")))

(defun saturation ()
  (choose-one-of '("very pale" "pale" "greyish" "moderately saturated" "strongish" "strong" "vivid")))

;; How to handle "medium" understandably?

(defun brightness ()
  (choose-one-of '("very dark" "dark" "medium" "light" "very light")))

(defun in-between-hue ()
  (choose-one-of '("orangish red" "reddish orange" "yellowish orange" "orangeish yellow" "greenish yellow" "yellowish green" "bluish green" "greenish blue" "purplish blue" 
		   "bluish purple" "reddish purple" "purplish red")))

(defun hsv ()
  (format nil "~a ~a ~a" (brightness) (saturation) (call-one-of '(hue in-between-hue))))

(defun combined-hsv ()
  (choose-one-of '("white" "black" "grey")))

(defun gloss ()
  (choose-one-of '("very matt" "matt" "silk" "silky" "glossy" "very glossy")))

(defun highlight-colour ()
  (choose-one-of '("plastic" "metallic")))

(defun polish ()
  (choose-one-of '("very polished" "polished" "rough" "very rough")))

(defun transparency ()
  (choose-one-of '("transparent" "nearly transparent" "semi-transparent" "nearly opaque" "opaque")))

(defun glow ()
  (choose-one-of '("very faintly glowing" "faintly glowing" "glowing" "brightly glowing" "very brightly glowing")))

(defun colour ()
  (join-strings (maybe #'glow :probability 0.05) (maybe #'transparency :probability 0.05) (maybe #'polish :probability 0.05) (maybe #'highlight-colour :probability 0.05)
		(maybe #'gloss :probability 0.05) 
		;; combined-hsv needs to be much lower probability
		(if (< (random 1.0) 0.95)
		    (hsv)
		    (combined-hsv))))

(defun run () 
  (when (/= (length *posix-argv*) 3)
    (format t "Please pass identi.ca username and password as command line arguents.~%")
    (quit))
  (setf *random-state* (make-random-state t))
  (ignore-errors
    (setf cl-twit::*source* "random-colour")
    (setf cl-twit::*base-url* "http://identi.ca/api")
    (cl-twit:with-session ((second *posix-argv*) (third *posix-argv*) :authenticatep t)
      (cl-twit:update (colour))
      (cl-twit:logout)))
  (quit))
