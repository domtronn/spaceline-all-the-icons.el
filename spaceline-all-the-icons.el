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

(defcustom spaceline-all-the-icons-highlight-file-name nil
  "Whether or not to highlight the file name as part of the buffer id."
  :group 'spaceline-all-the-icons
  :type 'boolean)

(defcustom spaceline-all-the-icons-file-name-highlight nil
  "The Color to highlight the file name part of the path.  e.g. #123123."
  :group 'spaceline-all-the-icons
  :type 'string)

;;; Helper functions
(defun spaceline-all-the-icons--separator (&optional padding)
  "Wrapper to render vertical line separator with optional PADDING."
  (propertize (format "%s|%s" (or padding "") (or padding ""))
              'face '(:height 0.9 :inherit)
              'display '(raise 0.2)))

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

;;; Second Divider Segments
(spaceline-define-segment
    all-the-icons-projectile "An `all-the-icons' segment to indicate the current `projectile' project"
    (let ((help-echo "Switch Project")
          (local-map (make-mode-line-mouse-map 'mouse-1 'projectile-switch-project))
          (project-id (if (and (fboundp 'projectile-project-p) (projectile-project-p))
                          (projectile-project-name) "×")));;
      (if spaceline-all-the-icons-slim-render
          (propertize project-id
                      'display '(raise 0.1)
                      'pointer 'hand
                      'help-echo help-echo
                      'local-map local-map)
        (concat
         (spaceline-all-the-icons--separator) " "
         (propertize project-id
                     'pointer 'hand
                     'display '(raise 0.2)
                     'face '(:height 0.8 :inherit)
                     'help-echo help-echo
                     'local-map local-map)
         " " (spaceline-all-the-icons--separator))))
    :tight t)

(spaceline-define-segment
    all-the-icons-mode-icon "An `all-the-icons' segment indicating the current buffer's mode with an icon"
    (let ((icon (all-the-icons-icon-for-buffer)))
      (propertize icon
                  'help-echo (format "Major-mode: `%s'" major-mode)
                  'display `(raise ,(if spaceline-all-the-icons-slim-render -0.1 0.0))
                  'face `(:height ,(if spaceline-all-the-icons-slim-render 1.3 1.1)
                          :family ,(all-the-icons-icon-family-for-buffer)
                          :inherit)))
    :when (not (symbolp (all-the-icons-icon-for-buffer))))

(spaceline-define-segment
    all-the-icons-buffer-id "An `all-the-icons' segment to display current buffer id"
    (let* ((buffer-id (if (buffer-file-name)
                      (file-truename (buffer-file-name))
                      (format-mode-line "%b")))

           (project-root    (ignore-errors (file-truename (projectile-project-root))))
           (buffer-relative (or (cadr (split-string buffer-id project-root)) buffer-id))

           (path (file-name-directory buffer-relative))
           (file (file-name-nondirectory buffer-relative))

           (height (if spaceline-all-the-icons-slim-render 1.0 0.8))
           (raise  (if spaceline-all-the-icons-slim-render 0.1 0.2))
           (help-echo (format "Major-mode: `%s'" major-mode))

           (file-face `(:height ,height :inherit))
           (show-path? (and (not spaceline-all-the-icons-slim-render) path active)))

      (when (and spaceline-all-the-icons-highlight-file-name show-path?)
        (plist-put file-face :height height)
        (plist-put file-face :background (face-background default-face))
        (plist-put file-face :foreground (or spaceline-all-the-icons-file-name-highlight
                                             (face-background highlight-face))))

      (concat
       (propertize (if show-path? path "")
                   'face `(:height ,height :inherit)
                   'display `(raise ,raise)
                   'help-echo help-echo)
       (propertize file
                   'face `(or ,file-face )
                   'display `(raise ,raise)
                   'help-echo help-echo)))
    :tight t)

;;; Third Divider Segments
(spaceline-define-segment
    all-the-icons-process "An `all-the-icons' segment to depict the current process"
    (propertize
     (concat
      (when (or (symbolp (all-the-icons-icon-for-buffer)) mode-line-process) (format-mode-line "%m"))
      (when mode-line-process (format-mode-line mode-line-process)))
     'face `(:height 0.8 :inherit)
     'display '(raise 0.2))
    :tight t)

(spaceline-define-segment
    all-the-icons-position "An `all-the-icons' Line & Column indicator"
    (propertize (format-mode-line "%l:%c")
                'face `(:height 0.9 :inherit)
                'display '(raise 0.1))
    :tight t)

(spaceline-define-segment
    all-the-icons-region-info "An `all-the-icons' indicator of the currently highlighted region"
    (let ((lines (count-lines (region-beginning) (region-end)))
          (words (count-words (region-beginning) (region-end)))

          (height (if spaceline-all-the-icons-slim-render 0.9 0.8))
          (display (if spaceline-all-the-icons-slim-render 0.1 0.2))
          (region-format (if spaceline-all-the-icons-slim-render "%s:%s" "(%s, %s)")))
      (concat
       (propertize (format "%s " (all-the-icons-octicon "pencil" :v-adjust 0.1))
                   'face `(:family ,(all-the-icons-octicon-family) :inherit))
       (propertize (format region-format lines words)
                   'face `(:height ,height :inherit)
                   'display `(raise ,display))))
    :when mark-active :tight t)

(spaceline-define-segment
    all-the-icons-fullscreen "An `all-the-icons' indicator to toggle fullscreen settings"
    (let* ((fullscreen? (frame-parameter nil 'fullscreen))
           (icon (all-the-icons-material (if fullscreen? "fullscreen_exit" "fullscreen"))))

      (propertize icon
                  'pointer 'hand
                  'display '(raise -0.2)
                  'help-echo "Toggle frame fullscreen"
                  'face `(:height 1.3 :family ,(all-the-icons-material-family) :inherit)
                  'local-map (make-mode-line-mouse-map 'mouse-1 'toggle-frame-fullscreen)))

    :tight t :enabled t)

(spaceline-define-segment
    all-the-icons-text-scale "An `all-the-icons' indicator to show how much text has been scaled"
    (let* ((zoom (if (equal (substring text-scale-mode-lighter 0 1) "+") "in" "out"))
           (icon (all-the-icons-material (format "zoom_%s" zoom)))
           (text (substring text-scale-mode-lighter 1)))

      (when (not (equal text "0"))
        (concat
         (propertize icon
                     'display '(raise -0.2)
                     'face `(:family ,(all-the-icons-material-family) :height 1.2 :inherit))
         (propertize text 'display '(raise 0.1)))))
    :tight t :enabled t)

(provide 'spaceline-all-the-icons)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons.el ends here
