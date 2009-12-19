;; random-form-microblogger.lisp - Microblog random forms.
;; Copyright (C) 2009  Rob Myers rob@robmyers.org
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'asdf)
(asdf:oos 'asdf:load-op 'microblog-bot)

(defun maybe (fun &key (probability 0.5) (default nil))
  "Call fun with args if random(0..1) is less than probability."
  (if (< (random 1.0) probability)
      (funcall fun)
    default))

(defun choose-one-of (choices)
  "Choose one of the parameters randomly."
  (nth (random (list-length choices)) 
       choices))

(defun maybe-choose-one-of (choices &key (probability 0.5))
  "Choose one of the list or return nil."
  (maybe #'(lambda () (choose-one-of choices)) :probability probability))

(defun call-one-of (choices)
  (funcall (choose-one-of choices)))

(defun maybe-call-one-of (choices)
  (maybe (choose-one-of choices)))

(defun join-strings (&rest strings)
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

(defun scale ()
  "The 3d dimensions of the form"
  (join-strings (maybe-choose-one-of '("massive" "large" "small" "tiny"))
		(maybe-choose-one-of '("fat" "narrow" "slim" "thin" "wide"))
		(maybe-choose-one-of '("shallow" "deep"))
		(maybe-choose-one-of '("tall" "short" "low" "stretched"))))

(defun style ()
  "The artistic, design or industrial style of the object"
  (choose-one-of '("abstract" "cuboidal" "cubist" "erotic" "futuristic" 
		   "organic" "spiky" "streamlined")))

(defun qualities ()
  "The object's textural or physical properties"
  (maybe-choose-one-of '("blurry" "cracked" "furry" "jagged" "ragged" "smeary"
		   "wobbly")))

(defun feel ()
  "The objects emotional or tactile feel"
  (maybe-choose-one-of '("angry" "carefree" "cold" "happy" "harsh" "warm")))

;; Colour(s),  n from cybernetic, optional?

(defun medium ()
  "The substance of the object"
  (maybe-choose-one-of '("chocolate" "ceramic" "crackle glazed" "foam rubber" 
		   "glass" "glazed" "impasto" "neon" "polished steel" 
		   "polychrome steel" "spraycan stencil" "wireframe" 
		   "welded steel")))

(defun shape ()
  "The object's shape or form"
  (choose-one-of '("anthropomorphic" "architectural"
		   "conical" "cuboidal" "cylindrical" "organic" 
		   "ovoidal" "flat" "toroidal" "triangular"
		   "zoomorphic")))

(defun thing ()
  "The kind of thing"
  (choose-one-of '("artefact" "artwork" "device" "form" "item" "object" "shape"
		   "thing")))

(defun random-form ()
  "A possible (albeit often improbable) physical form"
  (concatenate 'string 
	       (join-strings "A" (qualities) (scale) (style) (feel) (medium) 
			     (shape) (thing))
	       "."))

(defclass random-form-bot (microblog-bot:microblog-bot)
  ())

(defmethod microblog-bot:constant-task ((bot random-form-bot))
  "Dent a possible artwork."
    (twit:update (random-form)))

(defun make-microblog-bot ()
  "Make the bot."
  (assert (>= (length sb-ext:*posix-argv*) 2))
  (setf *random-state* (make-random-state t))
  (microblog-bot:set-microblog-service "https://identi.ca/api" "random-form")
  (make-instance 'random-form-bot
		 :nickname (second sb-ext:*posix-argv*)
		 :password (third sb-ext:*posix-argv*)
		 :source-url 
		 "http://robmyers.org/git/?p=random-form-microblogger.git"))

(defun run ()
  "Configure and run the bot."
  (microblog-bot:run-bot (make-microblog-bot)))

(defun run-once ()
  "Configure and run the bot just once."
  (microblog-bot:run-bot-once (make-microblog-bot))
  #+sbcl (quit))
