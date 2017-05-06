;;; spaceline-all-the-icons-separators.el --- Definitions of
;;; different separators for Spaceline using All The Icons

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; URL: https://github.com/domtronn/spaceline-all-the-icons.el

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
(require 'spaceline)
(require 'all-the-icons)

;; Custom settings for separators
(defcustom spaceline-all-the-icons-separator-type 'slant
  "Choose the spaceline separator type to use."
  :group 'spaceline-all-the-icons
  :type `(radio
          (const :tag "None" none)
          (const :tag ,(format "Slant  %s / %s"
                               (all-the-icons-alltheicon "slant-left")
                               (all-the-icons-alltheicon "slant-right")) slant)
          (const :tag ,(format "Wave   %s / %s"
                               (all-the-icons-alltheicon "wave-left")
                               (all-the-icons-alltheicon "wave-right")) wave)
          (const :tag ,(format "Cup    %s / %s"
                               (all-the-icons-alltheicon "cup-left")
                               (all-the-icons-alltheicon "cup-right")) cup)
          (const :tag ,(format "Arrow  %s / %s"
                               (all-the-icons-alltheicon "arrow-left")
                               (all-the-icons-alltheicon "arrow-right")) arrow)))

(defcustom spaceline-all-the-icons-separators-invert-direction nil
  "Whether or not to invert the separator direction."
  :group 'spaceline-all-the-icons
  :type 'boolean)

;; Functions to return separator specific render time info
(defun spaceline-all-the-icons-separators--get-type ()
  "Function to return `spaceline-all-the-icons-separator-type'."
  spaceline-all-the-icons-separator-type)

(defun spaceline-all-the-icons-separators--get-direction (dir)
  "Function to get direction DIR based on `spaceline-all-the-icons-separators-invert-direction'."
  (if spaceline-all-the-icons-separators-invert-direction
      (if (equal dir "right") "left" "right") dir))

(defmacro define-spaceline-all-the-icons--separator (name direction start-face end-face &optional invert)
  "Macro to define separator used by `spaceline-all-the-icons'.

Creates a separator with NAME choosing the icon DIRECTION while
transitioning from START-FACE to END-FACE.  START-FACE and
END-FACE can be a function returning a face or a face.  When
INVERT is defined, it will invert the direction of the
separator."
  `(spaceline-define-segment
       ,(intern (format "all-the-icons-separator-%s" name))
     (let ((separator (spaceline-all-the-icons-separators--get-type))
           (direction (spaceline-all-the-icons-separators--get-direction ,direction))
           (sf (if (functionp ,start-face) (funcall ,start-face) ,start-face))
           (ef (if (functionp ,end-face) (funcall ,end-face) ,end-face)))

       (when spaceline-all-the-icons-separators-invert-direction
         (setq sf (prog1 ef (setq ef sf))))

       (when (and (eq separator 'slant)          ;; Special case for slant
                  (equal direction "left"))      ;; need to invert the faces for left direction
         (setq sf (prog1 ef (setq ef sf))))

       (unless (or (eq separator 'none)
                   (string= (spaceline-all-the-icons--face-background sf) (spaceline-all-the-icons--face-background ef)))
        (propertize (all-the-icons-alltheicon (format "%s-%s" separator direction) :v-adjust 0.0)
                    'face `(:height ,(spaceline-all-the-icons--height 1.6)
                            :family ,(all-the-icons-alltheicon-family)
                            :foreground ,(spaceline-all-the-icons--face-background sf)
                            :background ,(spaceline-all-the-icons--face-background ef)))))
     :skip-alternate t :tight t :when (if ,invert (not active) active)))

(define-spaceline-all-the-icons--separator left-active-1 "right" spaceline-highlight-face-func 'powerline-active1)
(define-spaceline-all-the-icons--separator left-active-2 "right" 'powerline-active1 spaceline-highlight-face-func)
(define-spaceline-all-the-icons--separator left-active-3 "right" spaceline-highlight-face-func 'mode-line)
(define-spaceline-all-the-icons--separator left-active-4 "right" 'mode-line 'powerline-active2)

(define-spaceline-all-the-icons--separator right-active-1 "left" 'mode-line 'powerline-active2)
(define-spaceline-all-the-icons--separator right-active-2 "left" 'powerline-active1 'mode-line)

(define-spaceline-all-the-icons--separator minor-mode-right "right" spaceline-highlight-face-func 'powerline-active2)
(define-spaceline-all-the-icons--separator minor-mode-left  "left"  spaceline-highlight-face-func 'powerline-active2)
  
(define-spaceline-all-the-icons--separator left-inactive "right" 'powerline-inactive1 'powerline-inactive2 t)
(define-spaceline-all-the-icons--separator right-inactive "left" 'powerline-inactive1 'powerline-inactive2 t)

(define-spaceline-all-the-icons--separator paradox-1 "right" spaceline-highlight-face-func 'powerline-active1)
(define-spaceline-all-the-icons--separator paradox-2 "right" 'powerline-active1 'powerline-active2)
(define-spaceline-all-the-icons--separator paradox-3 "left" 'mode-line 'powerline-active2)
(define-spaceline-all-the-icons--separator paradox-4 "right" 'mode-line 'powerline-active2)

(provide 'spaceline-all-the-icons-separators)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons-separators.el ends here
