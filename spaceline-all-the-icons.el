;;; spaceline-all-the-icons.el --- A Spaceline theme using All The Icons

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3") (all-the-icons "2.4.0"))
;; URL: https://github.com/domtronn/spaceline-all-the-icons.el
;; Keywords: convenience, lisp, tools

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
(require 'bookmark)

;; toggle-on/-off  Toggle Switch
;; link/-broken    Chain with break
(defgroup spaceline-all-the-icons nil
  "Customize the the Spaceline All The Icons mode line and theming."
  :prefix "spaceline-all-the-icons-"
  :group 'spaceline
  :group 'appearance)

(defgroup spaceline-all-the-icons-icon-set nil
  "Customize which Icon Sets to use for various segments of the Spaceline All The Icons theme."
  :prefix "spaceline-all-the-icons-icon-set-"
  :group 'appearance
  :group 'spaceline
  :group 'spaceline-all-the-icons)

(defcustom spaceline-all-the-icons-icon-set-modified '("link" . "chain-broken")
  "The Icon set to use for the modified indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type '(radio
          (const :tag "Toggle Switch     On / Off" ("toggle-on" . "toggle-off"))
          (const :tag "Chain Links       Solid / Broken" ("link" . "chain-broken"))))

(defcustom spaceline-all-the-icons-icon-set-bookmark
  '((icon (on . "bookmark") (off . "bookmark-o"))
    (echo (on . "Bookmark") (off . "Remove Bookmark")))
  "The Icon set to use for the bookmark indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type '(radio
          (const :tag "Bookmark Icon" ((icon (on . "bookmark") (off . "bookmark-o"))
                                       (echo (on . "Bookmark") (off . "Remove Bookmark"))))
          (const :tag "Heart Icon   " ((icon (on . "heart") (off . "heart-o"))
                                       (echo (on . "Like") (off . "Unlike"))))
          (const :tag "Star Icon    " ((icon (on . "star") (off . "star-o"))
                                       (echo (on . "Star") (off . "Unstar"))))))

(defcustom spaceline-all-the-icons-icon-set-dedicated
  '(("thumb-tack" . "faicon") ("pin" . "octicon"))
  "The Icon set to use for the dedicated window indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type '(radio
          (const :tag "Thumb Tack / Pin " (("thumb-tack" . "faicon")
                                           ("pin" . "octicon")))
          (const :tag "Sticky Note      " (("sticky-note" . "faicon")
                                           ("sticky-note-o" . "faicon")))))

(defcustom spaceline-all-the-icons-icon-set-window-numbering 'circle
  "The Icon set to use for the modified indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type '(radio
          (const :tag "Circle Outline Icons" circle)
          (const :tag "Circle Solid Icons" solid)
          (const :tag "Square Icons" square)))

(defcustom spaceline-all-the-icons-window-number-always-visible nil
  "Whether or not to show the window number all the time or when there are multiple windows."
  :group 'spaceline-all-the-icons
  :type 'boolean)

(defcustom spaceline-all-the-icons-slim-render nil
  "Whether or not to render all information as slimly as possible.

This changes the way the mode line is rendered to be as slim as
possible, allowing more information to displayed on narrower windows/frames."
  :group 'spaceline-all-the-icons
  :type 'boolean)

;;; First Divider Segments
(spaceline-define-segment
    all-the-icons-modified "An `all-the-icons' segment depiciting the current buffers state"
    (let* ((buffer-state (format-mode-line "%*"))
           (icon (cond
                  ((equal buffer-state "-") (car spaceline-all-the-icons-icon-set-modified))
                  ((equal buffer-state "*") (cdr spaceline-all-the-icons-icon-set-modified))
                  ((equal buffer-state "%") "lock"))))
      (propertize (all-the-icons-faicon icon :v-adjust -0.0)
                  'face `(:family ,(all-the-icons-faicon-family) :height 1.1 :inherit)))
     :tight t)

(spaceline-define-segment
    all-the-icons-bookmark "An `all-the-icons' segment allowing for easy bookmarking of files"
    (progn
      (unless (boundp 'bookmark-alist) (bookmark-all-names)) ;; Force bookmarks to load
      (let-alist spaceline-all-the-icons-icon-set-bookmark
        (let* ((bookmark-name (buffer-file-name))
               (bookmark (find-if (lambda (it) (equal bookmark-name (car it))) bookmark-alist)))

          (propertize (all-the-icons-faicon (if bookmark .icon.on .icon.off) :v-adjust 0.1)
                      'pointer   'hand
                      'help-echo  (if bookmark .echo.off .echo.on)
                      'face      `(:family ,(all-the-icons-faicon-family) :inherit)
                      'local-map  (make-mode-line-mouse-map
                                   'mouse-1
                                   `(lambda () (interactive)
                                      (if ,(car bookmark)
                                          (bookmark-delete ,(car bookmark))
                                          (bookmark-set ,bookmark-name))
                                      (force-window-update)))))))

    :when (buffer-file-name) :enabled nil)

(spaceline-define-segment
    all-the-icons-dedicated "An `all-he-icons' segment to indicate and allow for making windows dedicated"
    (pcase-let*
        ((window (get-buffer-window (current-buffer)))
         (dedicated (window-dedicated-p window))
         (`(,icon . ,family) (funcall (if dedicated 'car 'cadr) spaceline-all-the-icons-icon-set-dedicated))

         (icon-f (intern (format "all-the-icons-%s" family)))
         (family-f (intern (format "all-the-icons-%s-family" family))))

      (propertize (funcall icon-f icon)
                  'pointer    'hand
                  'display    '(raise 0.1)
                  'face       `(:family ,(funcall family-f) :inherit)
                  'help-echo  "Toggle `window-dedidcated' for this window"
                  'local-map  (make-mode-line-mouse-map
                               'mouse-1
                               `(lambda () (interactive)
                                  (set-window-dedicated-p ,window (not ,dedicated))
                                  (force-window-update)))))
    :enabled nil)

(spaceline-define-segment
    all-the-icons-window-number "An `all-the-icons' segment depicting the current window number"
    (let ((face '(:height 1.4 :inherit))
          (window-num
           (cond
            ((bound-and-true-p winum-mode) (winum-get-number))
            ((bound-and-true-p window-numbering-mode) (window-numbering-get-number)))))
      (if (> window-num 9) window-num ;; Return the string version of the window number
        (let ((icon
               (pcase spaceline-all-the-icons-icon-set-window-numbering
                 ('solid   (format "%c" (+ window-num 10121)))
                 ('circle  (format "%c" (+ window-num 9311)))
                 ('square  (progn
                             (plist-put face :height 1.2)
                             (plist-put face :family (all-the-icons-material-family))
                             (all-the-icons-material (format "filter_%s" window-num) :v-adjust -0.2))))))

          (propertize icon 'face face))))

    :when (and
           (or (bound-and-true-p winum-mode)
               (bound-and-true-p window-numbering-mode))
           (or spaceline-all-the-icons-window-number-always-visible
               (> (length (window-list)) 1))))

(spaceline-define-segment
    all-the-icons-buffer-size "An `all-the-icons' segment depicting the buffer size"
    (propertize (format-mode-line "%I")
                'face '(:height 0.9 :inherit)
                'display '(raise 0.1))
    :tight t)

(provide 'spaceline-all-the-icons)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons.el ends here
