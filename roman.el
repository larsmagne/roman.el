;;; roman.el --- parsing and generating Roman numerals
;; Copyright (C) 2014 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; roman.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; roman.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(defvar roman-numeral-mapping
  '((1000 . "M")
    (500 . "D")
    (100 . "C")
    (50 . "L")
    (10 . "X")
    (5 . "V")
    (1 . "I")))

(defun format-roman-numeral (number)
  "Format NUMBER into a Roman numeral.
Roman numerals look like \"XCVII\"."
  (let ((values roman-numeral-mapping)
	roman)
    (while (> number 0)
      (while (>= number (caar values))
	(push (cdar values) roman)
	(setq number (- number (caar values))))
      (when (and (> number 0)
		 (> number (caadr values))
		 (memq (/ number (expt 10 (truncate (log number 10)))) '(9 4)))
	;; If we have a number beginning with 9 or 4, we want to
	;; generate a subtractive number, using the next smaller
	;; number where the first digit is 1 -- XC, for instance, for
	;; 90.
	(let ((subs (cdr values)))
	  (while (not (= (/ (caar subs)
			    (expt 10 (truncate (log (caar subs) 10))))
			 1))
	    (pop subs))
	  (push (concat (cdar subs) (cdar values)) roman)
	  (setq number (- number (+ (* (caar subs) 4) (caadr values))))))
      (pop values))
    (apply 'concat (nreverse roman))))

(defun parse-roman-numeral (roman)
  "Return the numerical value of a Roman numeral.
Roman numerals look like \"XCVII\"."
  (let (result)
    (dotimes (i (length roman))
      (let ((number (car (rassoc (string (aref roman i))
				 roman-numeral-mapping))))
	(if (and result
		 (> number (car result)))
	    (push (- number (pop result)) result)
	  (push number result))))
    (reduce '+ result)))

(provide 'roman)

;;; roman.el ends here
