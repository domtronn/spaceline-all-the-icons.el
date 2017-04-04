;;; spaceline-all-the-icons-segments.el --- Segments used by Spaceline All The Icons Theme

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (all-the-icons "2.4.0") (spaceline "2.0.0"))
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
(require 'all-the-icons)

(defmacro define-spaceline-all-the-icons--icon-set-getter (name)
  "Macro to create a getter function for icon set NAME."
  `(defun ,(intern (format "spaceline-all-the-icons-icon-set-%s" name)) ()
     ,(format "The Icon set to use for the `all-the-icons-%s' indicator." name)
     (let* ((icon-name (symbol-value (intern ,(format "spaceline-all-the-icons-icon-set-%s" name))))
            (icon-set (symbol-value (intern ,(format "spaceline-all-the-icons-icon-set--%s" name))))
            (result (cdr (assoc icon-name icon-set))))
       (unless result (error "Unable to find key `%s' - See `spaceline-all-the-icons-icon-set-%s'" icon-name ,name))
       (car result))))

;;; Modified Icon
(define-spaceline-all-the-icons--icon-set-getter "modified")
(defcustom spaceline-all-the-icons-icon-set-modified 'chain
  "The Icon set to use for the `all-the-icons-modified' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Toggle Switch     - %s / %s"
                               (all-the-icons-faicon "toggle-on")
                               (all-the-icons-faicon "toggle-off")) toggle)
          (const :tag ,(format "Chain Links       - %s / %s"
                               (all-the-icons-faicon "link")
                               (all-the-icons-faicon "chain-broken")) chain)
          (const :tag ,(format "Radio Buttons     - %s / %s"
                               (all-the-icons-faicon "circle-o")
                               (all-the-icons-faicon "dot-circle-o")) circle)))

(defconst spaceline-all-the-icons-icon-set--modified
  '((chain ("link" . "chain-broken"))
    (toggle ("toggle-on" . "toggle-off"))
    (circle ("circle-o" . "dot-circle-o"))))

;;; Bookmark Icon
(define-spaceline-all-the-icons--icon-set-getter "bookmark")
(defcustom spaceline-all-the-icons-icon-set-bookmark 'bookmark
  "The Icon set to use for the `all-the-icons-bookmark' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Bookmark Icon  - %s / %s"
                               (all-the-icons-faicon "bookmark")
                               (all-the-icons-faicon "bookmark-o")) bookmark)
          (const :tag ,(format "Heart Icon     - %s / %s"
                               (all-the-icons-faicon "heart")
                               (all-the-icons-faicon "heart-o")) heart)
          (const :tag ,(format "Star Icon      - %s / %s"
                               (all-the-icons-faicon "star")
                               (all-the-icons-faicon "star-o")) star)))

(defconst spaceline-all-the-icons-icon-set--bookmark
  '((bookmark ((icon (on . "bookmark") (off . "bookmark-o"))
               (echo (on . "Bookmark") (off . "Remove Bookmark"))))
    (heart    ((icon (on . "heart") (off . "heart-o"))
               (echo (on . "Like") (off . "Unlike"))))
    (star     ((icon (on . "star") (off . "star-o"))
               (echo (on . "Star") (off . "Unstar"))))))

;;; Dedicated Icon
(define-spaceline-all-the-icons--icon-set-getter "dedicated")
(defcustom spaceline-all-the-icons-icon-set-dedicated 'pin
  "The Icon set to use for the `all-the-icons-dedicated' window indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Pin          - %s / %s"
                               (all-the-icons-octicon "pin" :v-adjust 0.0)
                               (all-the-icons-faicon "thumb-tack" :v-adjust 0.0)) pin)
          (const :tag ,(format "Sticky Note  - %s / %s"
                               (all-the-icons-faicon "sticky-note" :v-adjust 0.0)
                               (all-the-icons-faicon "sticky-note-o" :v-adjust 0.0)) sticky-note)))

(defconst spaceline-all-the-icons-icon-set--dedicated
  '((sticky-note (("sticky-note" . faicon)
                  ("sticky-note-o" . faicon)))
    (pin         (("thumb-tack" . faicon)
                  ("pin" . octicon)))))

;; Window Numbering Icon
(defcustom spaceline-all-the-icons-icon-set-window-numbering 'circle
  "The Icon set to use for the `all-the-icons-window-number' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag "Circle Outline - ①" circle)
          (const :tag "Circle Solid   - ➊" solid)
          (const :tag "Normal String  - 1" string)
          (const :tag ,(format "Square         - %s" (all-the-icons-material "filter_1" :v-adjust 0.0)) square)))

;; Git Statistics Icon
(define-spaceline-all-the-icons--icon-set-getter "git-stats")
(defcustom spaceline-all-the-icons-icon-set-git-stats 'diff-icons
  "The Icon set to use for the `all-the-icons-git-status' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "GitHub   - %s / %s"
                               (all-the-icons-octicon "diff-added" :v-adjust 0.0)
                               (all-the-icons-octicon "diff-removed" :v-adjust 0.0)) diff-icons)
          (const :tag "Arrows   - 🡑 / 🡓" arrows)))

(defconst spaceline-all-the-icons-icon-set--git-stats
  `((diff-icons (,(all-the-icons-octicon "diff-added" :v-adjust 0.1)
                 ,(all-the-icons-octicon "diff-removed" :v-adjust 0.1)
                 ,(all-the-icons-octicon-family)))
    (arrows (,(propertize "🡑" 'display '(raise 0.0))
             ,(propertize "🡓" 'display '(raise 0.0))))))

;; Flycheck Slim Icons
(define-spaceline-all-the-icons--icon-set-getter "flycheck-slim")
(defcustom spaceline-all-the-icons-icon-set-flycheck-slim 'solid
  "The Icon set to use for the `all-the-icons-flycheck-status' in SLIM mode."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Solid Icons   - %s %s %s"
                               (all-the-icons-material "error" :v-adjust -0.2)
                               (all-the-icons-material "help" :v-adjust -0.2)
                               (all-the-icons-material "info" :v-adjust -0.2)) solid)
          (const :tag ,(format "Outline Icons - %s %s %s"
                               (all-the-icons-material "error_outline" :v-adjust -0.2)
                               (all-the-icons-material "help_outline" :v-adjust -0.2)
                               (all-the-icons-material "info_outline" :v-adjust -0.2)) outline)
          (const :tag "Circles       - • • •" dots)))

(defconst spaceline-all-the-icons-icon-set--flycheck-slim
  `((solid   (,(all-the-icons-material "error" :v-adjust -0.2)
              ,(all-the-icons-material "help" :v-adjust -0.2)
              ,(all-the-icons-material "info" :v-adjust -0.2)
              ,(all-the-icons-material-family)))
    (outline (,(all-the-icons-material "error_outline" :v-adjust -0.2)
              ,(all-the-icons-material "help_outline" :v-adjust -0.2)
              ,(all-the-icons-material "info_outline" :v-adjust -0.2)
              ,(all-the-icons-material-family)))
    (dots ("•" "•" "•"))))

;; Sun Time Icons
(define-spaceline-all-the-icons--icon-set-getter "sun-time")
(defcustom spaceline-all-the-icons-icon-set-sun-time 'rise/set
  "The Icon set to use for the `all-the-icons-flycheck-status' in SLIM mode."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Sun Up / Down Icons   - %s / %s"
                               (all-the-icons-wicon "sunrise" :v-adjust -0.2)
                               (all-the-icons-wicon "sunset" :v-adjust -0.2)) rise/set)
          (const :tag ,(format "Sun / Moon Icons      - %s / %s"
                               (all-the-icons-wicon "day-sunny" :v-adjust -0.2)
                               (all-the-icons-wicon "night-clear" :v-adjust -0.2)) sun/moon)
          (const :tag "Arrows                - 🡑 / 🡓" arrows)))

(defconst spaceline-all-the-icons-icon-set--sun-time
  `((rise/set ((sunrise . ,(all-the-icons-wicon "sunrise" :v-adjust 0))
               (sunset . ,(all-the-icons-wicon "sunset" :v-adjust 0))))
    (sun/moon ((sunrise . ,(all-the-icons-wicon "day-sunny" :v-adjust 0))
               (sunset . ,(all-the-icons-wicon "night-clear" :v-adjust 0))))
    (arrows   ((sunrise . ,(propertize "🡑" 'display '(raise 0.1)))
               (sunset .  ,(propertize "🡓" 'display '(raise 0.1)))))))

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

(defcustom spaceline-all-the-icons-flycheck-alternate nil
  "Whether or not to use the alternate slim version of `flycheck-status'.

This is used whenever `spaceline-all-the-icons-slim-render' is non-nil,
and shows three dots with numbers,  i.e.
  - non-nil : •1•2•1
  - nil     : ✖ 3 issues 1 i"
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

(defface spaceline-all-the-icons-info-face
  '((t (:foreground "#63B2FF")))
  "Face for `all-the-icons' info feedback in the modeline."
  :group 'spaceline-all-the-icons)

(defface spaceline-all-the-icons-sunrise-face
  '((t (:foreground "#f6c175" :inherit powerline-active2)))
  "Face for `all-the-icons' info feedback in the modeline."
  :group 'spaceline-all-the-icons)

(defface spaceline-all-the-icons-sunset-face
  '((t (:foreground "#fe7714" :inherit powerline-active2)))
  "Face for `all-the-icons' info feedback in the modeline."
  :group 'spaceline-all-the-icons)

;;; Helper functions
(defun spaceline-all-the-icons--separator (icon &optional left-padding right-padding)
  "Wrapper to render vertical line separator ICON with optional LEFT-PADDING & RIGHT-PADDING."
  (if spaceline-all-the-icons-slim-render " "
    (let ((raise (if (string= "|" icon) 0.2 0.0))
          (height (if (string= "|" icon) 0.9 1.2)))
      (concat
       (propertize (or left-padding "") 'face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit))
       (propertize icon
                   'face `(:height ,(spaceline-all-the-icons--height height) :inherit)
                   'display `(raise ,raise))
       (propertize (or right-padding left-padding "") 'face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit))))))

(defun spaceline-all-the-icons--highlight ()
  "Return the `mouse-face' highlight face to be used when propertizing text.
This is done as a function rather than a static face as it
doesn't inherit all properties of a face."
  `((foreground-color . ,(face-foreground 'spaceline-all-the-icons-info-face))))

(defun spaceline-all-the-icons--highlight-background ()
  "Return a `mouse-face' to highlight the background when focussed."
  `((background-color . ,(face-foreground 'spaceline-all-the-icons-info-face))))

;;; First Divider Segments
(spaceline-define-segment all-the-icons-modified
  "An `all-the-icons' segment depiciting the current buffers state"
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((string= buffer-state "-") (car (spaceline-all-the-icons-icon-set-modified)))
                ((string= buffer-state "*") (cdr (spaceline-all-the-icons-icon-set-modified)))
                ((string= buffer-state "%") "lock"))))

    (propertize (all-the-icons-faicon icon :v-adjust 0.0)
                'face `(:family ,(all-the-icons-faicon-family) :height ,(spaceline-all-the-icons--height 1.1) :inherit)
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode)))
  :tight t)

(spaceline-define-segment all-the-icons-bookmark
  "An `all-the-icons' segment allowing for easy bookmarking of files"
  (progn
    (unless (boundp 'bookmark-alist) (bookmark-all-names)) ;; Force bookmarks to load
    (let-alist (spaceline-all-the-icons-icon-set-bookmark)
      (let* ((bookmark-name (buffer-file-name))
             (bookmark (find-if (lambda (it) (string= bookmark-name (car it))) bookmark-alist)))

        (propertize (all-the-icons-faicon (if bookmark .icon.on .icon.off) :v-adjust 0.1)
                    'face      `(:family ,(all-the-icons-faicon-family) :height ,(spaceline-all-the-icons--height):inherit)
                    'help-echo  (if bookmark .echo.off .echo.on)
                    'mouse-face (spaceline-all-the-icons--highlight)
                    'local-map  (make-mode-line-mouse-map
                                 'mouse-1
                                 `(lambda () (interactive)
                                    (if ,(car bookmark)
                                        (bookmark-delete ,(car bookmark))
                                      (bookmark-set ,bookmark-name))
                                    (force-mode-line-update)))))))

  :when (buffer-file-name) :enabled nil)

(spaceline-define-segment all-the-icons-dedicated
  "An `all-the-icons' segment to indicate and allow for making windows dedicated"
  (pcase-let*
      ((window (get-buffer-window (current-buffer)))
       (dedicated (window-dedicated-p window))
       (`(,icon . ,family) (funcall (if dedicated 'car 'cadr) (spaceline-all-the-icons-icon-set-dedicated)))

       (icon-f (all-the-icons--function-name family))
       (family-f (all-the-icons--family-name family)))

    (propertize (funcall icon-f icon)
                'display    '(raise 0.1)
                'face       `(:family ,(funcall family-f) :height ,(spaceline-all-the-icons--height) :inherit)
                'help-echo  "Toggle `window-dedidcated' for this window"
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map  (make-mode-line-mouse-map
                             'mouse-1
                             `(lambda () (interactive)
                                (set-window-dedicated-p ,window (not ,dedicated))
                                (force-mode-line-update)))))
  :enabled nil)

(spaceline-define-segment all-the-icons-window-number
  "An `all-the-icons' segment depicting the current window number"
  (let ((face `(:height ,(spaceline-all-the-icons--height 1.4) :inherit))
        (window-num
         (cond
          ((bound-and-true-p winum-mode) (winum-get-number))
          ((bound-and-true-p window-numbering-mode) (window-numbering-get-number)))))
    (if (> window-num 9) window-num ;; Return the string version of the window number
      (let ((icon
             (pcase spaceline-all-the-icons-icon-set-window-numbering
               ('solid   (format "%c" (+ window-num 10121)))
               ('circle  (format "%c" (+ window-num 9311)))
               ('string  (progn
                           (plist-put face :height (spaceline-all-the-icons--height 1.2))
                           (number-to-string window-num)))
               ('square  (progn
                           (plist-put face :height (spaceline-all-the-icons--height 1.2))
                           (plist-put face :family (all-the-icons-material-family))
                           (all-the-icons-material (format "filter_%s" window-num) :v-adjust -0.2))))))

        (propertize icon 'face face))))

  :when (and
         (or (bound-and-true-p winum-mode)
             (bound-and-true-p window-numbering-mode))
         (or spaceline-all-the-icons-window-number-always-visible
             (> (length (window-list)) 1))))

(spaceline-define-segment all-the-icons-buffer-size
  "An `all-the-icons' segment depicting the buffer size"
  (propertize (format-mode-line "%I")
              'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)
              'display '(raise 0.1))
  :tight t)

;;; Second Divider Segments
(spaceline-define-segment all-the-icons-projectile
  "An `all-the-icons' segment to indicate the current `projectile' project"
  (let ((help-echo "Switch Project")
        (raise (if spaceline-all-the-icons-slim-render 0.1 0.2))
        (height (if spaceline-all-the-icons-slim-render 1.0 0.8))
        (local-map (make-mode-line-mouse-map 'mouse-1 'projectile-switch-project))
        (project-id (if (and (fboundp 'projectile-project-p) (projectile-project-p))
                        (projectile-project-name) "×")))

    (concat
     (spaceline-all-the-icons--separator "|" nil " ")
     (propertize project-id
                 'face `(:height ,(spaceline-all-the-icons--height height) :inherit)
                 'mouse-face (spaceline-all-the-icons--highlight)
                 'display `(raise ,raise)
                 'help-echo help-echo
                 'local-map local-map)
     (spaceline-all-the-icons--separator "|" " " "")))
  :tight t)

(spaceline-define-segment all-the-icons-mode-icon
  "An `all-the-icons' segment indicating the current buffer's mode with an icon"
  (let ((icon (all-the-icons-icon-for-buffer)))
    (propertize icon
                'help-echo (format "Major-mode: `%s'" major-mode)
                'display '(raise 0)
                'face `(:height ,(spaceline-all-the-icons--height 1.1)
                        :family ,(all-the-icons-icon-family-for-buffer)
                        :inherit)))
  :when (not (symbolp (all-the-icons-icon-for-buffer))))

(spaceline-define-segment all-the-icons-buffer-id
  "An `all-the-icons' segment to display current buffer id"
  (let* ((buffer-id (if (and (buffer-file-name)
                             (fboundp 'projectile-project-p)
                             (projectile-project-p))
                        (file-truename (buffer-file-name))
                      (format-mode-line "%b")))

         (project-root    (ignore-errors (file-truename (projectile-project-root))))
         (buffer-relative (if project-root
                              (or (cadr (split-string buffer-id project-root)) buffer-id)
                              buffer-id))

         (path (file-name-directory buffer-relative))
         (file (file-name-nondirectory buffer-relative))

         (height (if spaceline-all-the-icons-slim-render 1.0 0.8))
         (raise  (if spaceline-all-the-icons-slim-render 0.1 0.2))
         (help-echo (format "Major-mode: `%s'" major-mode))

         (file-face `(:height ,(spaceline-all-the-icons--height height)))
         (show-path? (and (not spaceline-all-the-icons-slim-render) path active))

         (mouse-f (if (and (fboundp 'projectile-project-p)
                           (projectile-project-p))
                      'projectile-find-file
                      'find-file)))

    (if (not (and spaceline-all-the-icons-highlight-file-name show-path?))
        (add-to-list 'file-face :inherit t)
      (plist-put file-face :background (face-background default-face))
      (plist-put file-face :foreground (or spaceline-all-the-icons-file-name-highlight
                                           (face-background highlight-face))))

    (propertize
     (concat
      (propertize (if show-path? path "")
                  'face `(:height ,(spaceline-all-the-icons--height height) :inherit)
                  'display `(raise ,raise)
                  'help-echo help-echo)
      (propertize file
                  'face file-face
                  'display `(raise ,raise)
                  'help-echo help-echo))
     'mouse-face (spaceline-all-the-icons--highlight)
     'local-map (make-mode-line-mouse-map 'mouse-1 mouse-f)))
  :tight t)

;;; Third Divider Segments
(spaceline-define-segment all-the-icons-process
  "An `all-the-icons' segment to depict the current process"
  (propertize
   (concat
    (when (or (symbolp (all-the-icons-icon-for-buffer)) mode-line-process) (format-mode-line "%m"))
    (when mode-line-process (format-mode-line mode-line-process)))
   'face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit)
   'display '(raise 0.2))
  :tight t)

(spaceline-define-segment all-the-icons-position
  "An `all-the-icons' Line & Column indicator"
  (propertize (format-mode-line "%l:%c")
              'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)
              'display '(raise 0.1))
  :tight t)

(spaceline-define-segment all-the-icons-region-info
  "An `all-the-icons' indicator of the currently highlighted region"
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

(spaceline-define-segment all-the-icons-fullscreen
  "An `all-the-icons' indicator to toggle fullscreen settings"
  (let* ((fullscreen? (frame-parameter nil 'fullscreen))
         (icon (all-the-icons-material (if fullscreen? "fullscreen_exit" "fullscreen"))))

    (propertize icon
                'display '(raise -0.2)
                'help-echo "Toggle frame fullscreen"
                'mouse-face (spaceline-all-the-icons--highlight)
                'face `(:height ,(spaceline-all-the-icons--height 1.3) :family ,(all-the-icons-material-family) :inherit)
                'local-map (make-mode-line-mouse-map 'mouse-1 'toggle-frame-fullscreen)))

  :tight t :enabled nil)

(spaceline-define-segment all-the-icons-text-scale
  "An `all-the-icons' indicator to show how much text has been scaled"
  (let* ((zoom (if (string= (substring text-scale-mode-lighter 0 1) "+") "in" "out"))
         (icon (all-the-icons-material (format "zoom_%s" zoom)))
         (text (substring text-scale-mode-lighter 1)))

    (when (not (string= text "0"))
      (concat
       (propertize icon
                   'display '(raise -0.2)
                   'face `(:family ,(all-the-icons-material-family) :height ,(spaceline-all-the-icons--height 1.2) :inherit))
       (propertize text 'face `(:height ,(spaceline-all-the-icons--height)) 'display '(raise 0.1)))))
  :tight t :enabled nil)

;; Fourth divider segments
(spaceline-define-segment all-the-icons-vc-icon
  "An `all-the-icons' segment to depict the current VC system with an icon"
  (cond ((string-match "Git[:-]" vc-mode)
         (propertize (all-the-icons-alltheicon "git")
                     'face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-alltheicon-family) :inherit)
                     'display '(raise 0.1)))
        ((string-match "SVN-" vc-mode)
         (propertize (all-the-icons-material "cloud_download" :v-adjust -0.2)
                     'face `(:height ,(spaceline-all-the-icons--height 1.2) :family ,(all-the-icons-material-family))))
        (t ""))

  :when (and active vc-mode (not spaceline-all-the-icons-slim-render)))

(defun spaceline-all-the-icons--vc-git ()
  "Get the formatted GIT Version Control Icon based on variable `vc-mode'."
  (let* ((branch (cadr (split-string vc-mode "Git[:-]")))
         (git-branch (all-the-icons-octicon (if (string= branch "master") "git-merge" "git-branch")))
         (local-map (get-text-property 1 'local-map branch)))
    (propertize
     (concat
      (propertize git-branch
                  'face `(:family ,(all-the-icons-octicon-family) :height ,(spaceline-all-the-icons--height) :inherit)
                  'display '(raise 0.1))
      (propertize (format " %s" branch)
                  'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)
                  'display '(raise 0.1)))
     'mouse-face (spaceline-all-the-icons--highlight)
     'local-map local-map)))

(defun spaceline-all-the-icons--vc-svn ()
  "Get the formatted SVN Version Control Icon based on variable `vc-mode'."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format "%s" revision)
                 'face `(:height ,(spaceline-all-the-icons--height 0.9))
                 'display '(raise 0.1)))))

(spaceline-define-segment all-the-icons-vc-status
  "An `all-the-icons' segment to depict the current VC system with an icon"
  (cond ((string-match "Git[:-]" vc-mode) (spaceline-all-the-icons--vc-git))
        ((string-match "SVN-" vc-mode) (spaceline-all-the-icons--vc-svn))
        (t ""))

  :when (and active vc-mode))

(defun spaceline-all-the-icons--git-stats (icon text face &optional family)
  "Wrapper to render git statistics ICON with TEXT using FACE.
When FAMILY is provided, put `:family' property into face."
  (let* ((height (if (> (length (spaceline-all-the-icons-icon-set-git-stats)) 2) 1.0 1.2))
         (icon-face `(:foreground ,(face-foreground face) :height ,(spaceline-all-the-icons--height height))))
    (when family (plist-put icon-face :family family))
    (concat
     (propertize icon 'face icon-face)
     (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.2)))
     (propertize (format "%s" text) 'face `(:foreground ,(face-foreground face) :height ,(spaceline-all-the-icons--height))))))

(spaceline-define-segment all-the-icons-git-status
  "An `all-the-icons' segment to display Added/Removed stats for files under git VC."
  (destructuring-bind (added . removed) (git-gutter:statistic)
    (let ((icon-fam (caddr (spaceline-all-the-icons-icon-set-git-stats)))
          (added-icon (car (spaceline-all-the-icons-icon-set-git-stats)))
          (removed-icon (cadr (spaceline-all-the-icons-icon-set-git-stats))))
      (propertize
       (concat
        (unless (zerop added)
          (spaceline-all-the-icons--git-stats added-icon added 'success icon-fam))
        (unless (or (zerop added) (zerop removed))
          (propertize " " 'face `(:height ,(if spaceline-all-the-icons-slim-render 0.2 1.0))))
        (unless (zerop removed)
          (spaceline-all-the-icons--git-stats removed-icon removed 'error icon-fam)))
       'help-echo "View Diff of current file"
       'mouse-face (spaceline-all-the-icons--highlight)
       'local-map (make-mode-line-mouse-map 'mouse-1 'vc-ediff))))

  :when (and active
             (fboundp 'git-gutter:statistic)
             (not (equal '(0 . 0) (git-gutter:statistic)))))

(defun spaceline-all-the-icons--flycheck-pip (icon text face &optional family)
  "Wrapper to render flycheck status ICON with TEXT using FACE.
When FAMILY is provided, put `:family' property into face."
  (let* ((height 1.0)
         (raise (if (> (length (spaceline-all-the-icons-icon-set-flycheck-slim)) 3) -0.2 0.0))
         (icon-face `(:foreground ,(face-foreground face) :height ,(spaceline-all-the-icons--height height))))
    (when family (plist-put icon-face :family family))
    (when text
     (concat
      (propertize icon 'face icon-face 'display `(raise ,raise))
      (propertize (format "%s" text) 'face `(:foreground ,(face-foreground face) :height ,(spaceline-all-the-icons--height)))))))

(defun spaceline-all-the-icons--flycheck-status-slim ()
  "Render the mode line for Flycheck Status slim mode."
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (let* ((get-text (lambda (text) (cond ((eq 'running flycheck-last-status-change) "?")
                                     ((zerop (or text 0)) nil)
                                     (t text))))

           (error-text (funcall get-text .error))
           (warn-text  (funcall get-text .warning))
           (info-text  (funcall get-text .info))

           (error-icon (car (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (warn-icon (cadr (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (help-icon (caddr (spaceline-all-the-icons-icon-set-flycheck-slim)))

           (family (cadddr (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (space (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.6)))))

      (mapconcat
       'identity
       (remove-if
        'null
        `(,(spaceline-all-the-icons--flycheck-pip error-icon error-text 'error family)
          ,(spaceline-all-the-icons--flycheck-pip warn-icon warn-text 'warning family)
          ,(spaceline-all-the-icons--flycheck-pip help-icon info-text 'spaceline-all-the-icons-info-face family)))
       space))))

(defun spaceline-all-the-icons--flycheck-finished ()
  "Get the string for finished status of Flycheck."
  (let* ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                  (+ (or .warning 0) (or .error 0))))
         (plural (if (eq 1 count) "" "s")))
    (if flycheck-current-errors (format "✖ %s Issue%s" count plural) "✔ No Issues")))

(defun spaceline-all-the-icons--flycheck-status ()
  "Render the mode line for Flycheck Status in a more verbose fashion."
  (let* ((text (pcase flycheck-last-status-change
                 (`finished    (spaceline-all-the-icons--flycheck-finished))
                 (`running     "⟲ Running")
                 (`no-checker  "⚠ No Checker")
                 (`not-checked "✖ Disabled")
                 (`errored     "⚠ Error")
                 (`interrupted "⛔ Interrupted")))
         (face (cond
                ((string-match "✔" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(face-foreground 'success)))
                ((string-match "⚠" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(face-foreground 'warning)))
                ((string-match "✖ [0-9]" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(face-foreground 'error)))
                ((string-match "✖ Disabled" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(face-foreground 'font-lock-comment-face)))
                (t `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))))

     (propertize text 'face face 'display '(raise 0.1))))

(spaceline-define-segment all-the-icons-flycheck-status
  "An `all-the-icons' segment to show the `flycheck-last-status-change'."
  (propertize (if (or spaceline-all-the-icons-flycheck-alternate
                      spaceline-all-the-icons-slim-render)
                  (spaceline-all-the-icons--flycheck-status-slim)
                  (spaceline-all-the-icons--flycheck-status))
              'help-echo "Show Flycheck Errors"
              'mouse-face (spaceline-all-the-icons--highlight)
              'local-map (make-mode-line-mouse-map 'mouse-1 'flycheck-list-errors))

  :tight t
  :when (and active (bound-and-true-p flycheck-last-status-change)))

(spaceline-define-segment all-the-icons-flycheck-status-info
  "An `all-the-icons' segment to show the info section of `flycheck-last-status-change'."
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (unless (zerop (or .info 0))
      (propertize (format "%s %s" .info (all-the-icons-faicon "info" :v-adjust 0.0 :height (spaceline-all-the-icons--height 0.8)))
                  'face `(:foreground ,(face-foreground 'spaceline-all-the-icons-info-face))
                  'help-echo "Show Flycheck Errors"
                  'mouse-face (spaceline-all-the-icons--highlight)
                  'local-map (make-mode-line-mouse-map 'mouse-1 'flycheck-list-errors))))
    :when (and active
               (not spaceline-all-the-icons-slim-render)
               (bound-and-true-p flycheck-last-status-change)))

(defvar spaceline-all-the-icons--package-updates nil)
(defun spaceline-all-the-icons--count-package-updates (&rest args)
  "Function to count the number of package upgrades available.

ARGS are provided as part of advice.  Opens a packages menu and
sets `spaceline-all-the-icons--package-updates' to the number of
available updates then restores the current buffer."
  (let ((cb (current-buffer)))
    (package-list-packages-no-fetch)
    (with-current-buffer "*Packages*"
      (setq spaceline-all-the-icons--package-updates (length (package-menu--find-upgrades))))
    (switch-to-buffer cb)))

(defun spaceline-all-the-icons-setup-advice ()
  "Set up advice in order to count package upgrades."
  (spaceline-all-the-icons--count-package-updates)
  (advice-add 'package-menu-execute :after 'spaceline-all-the-icons--count-package-updates)
  (advice-add 'package-refresh-contents :after 'spaceline-all-the-icons--count-package-updates))

(spaceline-define-segment all-the-icons-package-updates
  "An `all-the-icons' segment to display the number of package updates"
  (let ((face `(:height ,(spaceline-all-the-icons--height 0.9)))
        (new-text  (when spaceline-all-the-icons-slim-render
                       (format "%s" (all-the-icons-material "new_releases" :v-adjust -0.2))))
        (update-text (concat
                      (format "%s" spaceline-all-the-icons--package-updates)
                      (unless spaceline-all-the-icons-slim-render " updates"))))

    (propertize
     (concat
      (propertize (all-the-icons-octicon "package" :v-adjust 0.1)
                  'face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-octicon-family)))
      (propertize (or new-text "")
                  'face `(:height ,(spaceline-all-the-icons--height 1.2) :family ,(all-the-icons-material-family)))
      (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.4)))
      (propertize update-text
                  'face face
                  'display '(raise 0.1)))
     'help-echo "Open Packages Menu"
     'mouse-face (spaceline-all-the-icons--highlight)
     'local-map (make-mode-line-mouse-map 'mouse-1 'package-list-packages)))

  :when (and active spaceline-all-the-icons--package-updates))

;; First Right divider segments
(spaceline-define-segment all-the-icons-hud
  "An `all-the-icons' segment to show the position through buffer HUD indicator."
  (let ((color (face-foreground default-face))
        (height (frame-char-height))
        (ws (window-start))
        (we (window-end))
        pmax pmin)
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (propertize " "
                'display (pl/percent-xpm height pmax pmin we ws (* (frame-char-width) 1) color nil)
                'face default-face))
  :tight t
  :when (and active
             (not (string= "All" (format-mode-line "%p")))))

(spaceline-define-segment all-the-icons-buffer-position
  "An `all-the-icons' segment to show the buffer position as a percentage"
  (propertize
   (if (string-match "\%" (format-mode-line "%p"))
       (format-mode-line "%p%%")
     (format-mode-line "%p"))
   'face `(:height ,(spaceline-all-the-icons--height) :inherit))
  :enabled nil :when (not spaceline-all-the-icons-slim-render))

;; Second Right divider segments
(spaceline-define-segment all-the-icons-battery-status
  "An `all-the-icons' segment to show the battery information"
  (let* ((charging?  (string= "AC" (cdr (assoc ?L fancy-battery-last-status))))
         (percent    (string-to-int (cdr (assoc ?p fancy-battery-last-status))))
         (time       (cdr (assoc ?t fancy-battery-last-status)))

         (icon-alist
          (cond
           (charging? '((icon . "charging") (inherit . success) (height . 1.3) (raise . 0.0)))
           ((> percent 95) '((icon . "full") (inherit . success)))
           ((> percent 70) '((icon . "three-quarters")))
           ((> percent 30) '((icon . "half")))
           ((> percent 15) '((icon . "quarter") (inherit . warning)))
           (t '((icon . "empty") (inherit . error)))))

         (icon-set (if charging? 'alltheicon 'faicon))
         (icon-f   (all-the-icons--function-name icon-set))
         (family-f (all-the-icons--family-name icon-set))

         (icon-face `(:height ,(spaceline-all-the-icons--height) :family ,(funcall family-f) :background ,(face-background default-face)))
         (text-face `(:height ,(spaceline-all-the-icons--height 0.9) :background ,(face-background default-face))))

    (let-alist icon-alist
      (when .height (plist-put icon-face :height (spaceline-all-the-icons--height .height)))
      (when .inherit
        (plist-put icon-face :foreground (face-foreground .inherit))
        (plist-put text-face :foreground (face-foreground .inherit)))
      (propertize
       (concat
        (propertize (funcall icon-f (format "battery-%s" .icon))
                    'face icon-face
                    'display `(raise ,(or .raise 0.0)))
        (propertize (cond
                     (spaceline-all-the-icons-slim-render "")
                     (charging? (format " %s%%%%" percent))
                     (t (format " %s" time)))
                    'face text-face
                    'display '(raise 0.1)))
       'help-echo `(format "%s Remaining" ,time)
       'mouse-face (spaceline-all-the-icons--highlight))))

  :global-override fancy-battery-mode-line
  :when (and active (bound-and-true-p fancy-battery-mode)))

(spaceline-define-segment all-the-icons-time
  "An `all-the-icons' segment to to display the time and a clock icon"
  (let* ((time (string-to-number (format-time-string "%I")))
         (icon (all-the-icons-wicon (format "time-%s" time) :v-adjust 0.0)))
    (propertize
     (concat
      (propertize (format-time-string "%H:%M ") 'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit) 'display '(raise 0.1))
      (propertize icon
                  'face `(:height ,(spaceline-all-the-icons--height 0.9) :family ,(all-the-icons-wicon-family) :inherit)
                  'display '(raise 0.1)))
      'help-echo `(format-time-string "%H:%M")
      'mouse-face (spaceline-all-the-icons--highlight)))
  :tight t
  :enabled t)

;; Middle Divider Segments
(spaceline-define-segment all-the-icons-which-function
  "An `all-the-icons' segment to show the `which-function-mode' function"
  (let* ((current (format-mode-line which-func-current))
         (unknown? (string= current which-func-unknown))
         (function-icon (all-the-icons-fileicon "cold-fusion" :v-adjust 0))
         (question-icon (all-the-icons-faicon "question"))

         (text-face '(:family ,(all-the-icons-faicon-family) :inherit))
         (icon-face `(:family ,(all-the-icons-fileicon-family) :inherit)))

    (when (string-match "{\\(.*\\)}" current) (setq current (match-string 1 current)))

    (unless spaceline-all-the-icons-slim-render
      (propertize
       (concat (propertize function-icon 'face icon-face) " "
               (propertize (if unknown? question-icon current)
                           'face `(if unknown? ,text-face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit))
                           'display '(raise 0.2)))
       'mouse-face (spaceline-all-the-icons--highlight)
       'local-map which-func-keymap
       'help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end")))

  :when (and active
             (bound-and-true-p which-function-mode)
             (bound-and-true-p which-func-mode)))

;; Optional Segments
(defun spaceline-all-the-icons-anzu-update-func (here total)
  "Update func to be set as `anzu-mode-line-update-function.'
Displays HERE and TOTAL to indicate how many search results have been found."
  (let* ((status (case anzu--state
                   (search (format "(%s/%d%s)"
                                   (anzu--format-here-position here total)
                                   total (if anzu--overflow-p "+" "")))
                   (replace (format "(%d/%d)" here total))
                   (replace-query (format "(%d replace)" total))))
         (icon (case anzu--state
                 (search "search")
                 (replace "refresh")
                 (replace-query "find_replace")))
         (anzu-face (if (and (zerop total)
                             (not (string= isearch-string "")))
                        'anzu-mode-line-no-match 'anzu-mode-line))
         (text-face `(:height ,(spaceline-all-the-icons--height 1.1) :inherit ,anzu-face))
         (icon-face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-material-family) :inherit ,anzu-face)))
    
    (concat " "
     (propertize (all-the-icons-material icon) 'face icon-face)
     (propertize status 'face text-face) " ")))

;; Weather Segments
(defmacro define-spaceline-all-the-icons--sun-segment (type)
  "Macro to declare `spaceline' segment to TYPE (one of \"sunset\" \"sunrise\") times."
  `(spaceline-define-segment ,(intern (format "all-the-icons-%s" type))
     ,(format "An `all-the-icons' segment to depict the %s time with icons." type)
     (let* ((time (yahoo-weather-info-format yahoo-weather-info ,(format "%%(%s-time)" type)))
            (icon (cdr (assoc (quote ,(intern type)) (spaceline-all-the-icons-icon-set-sun-time))))

            (help-echo (format "%s at %s" ,(capitalize type) time))

            (text-face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit))
            (icon-face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit ,(intern ,(format "spaceline-all-the-icons-%s-face" type)))))

       (unless (eq spaceline-all-the-icons-icon-set-sun-time 'arrows)
         (plist-put icon-face :family (all-the-icons-wicon-family)))

       (propertize
        (concat
         (unless spaceline-all-the-icons-slim-render (propertize time 'face text-face 'display '(raise 0.1)))
         (unless spaceline-all-the-icons-slim-render (propertize " " 'face '(:height 0.5 :inherit)))
         (propertize icon 'face icon-face))
        'help-echo help-echo
        'mouse-face (spaceline-all-the-icons--highlight)))
     :tight t
     :when (and active
                (bound-and-true-p yahoo-weather-mode)
                (bound-and-true-p yahoo-weather-info))))

(define-spaceline-all-the-icons--sun-segment "sunrise")
(define-spaceline-all-the-icons--sun-segment "sunset")

(defun spaceline-all-the-icons--temperature-color ()
  "Convert CELSIUS into a color temperature Hex Code."
  (let* ((yahoo-weather-use-F nil)
         (celsius (string-to-number (yahoo-weather-info-format yahoo-weather-info "%(temperature)")))
         (normal (max 13 (- 100 (* celsius 4))))
         (clamp (lambda (i) (max 0 (min 255 i))))
         (r (funcall clamp (if (< normal 67)
                               255
                               (* 329.698727446 (expt (- normal 60) -0.1332047592)))))
         (g (funcall clamp (if (< normal 67)
                               (- (* 99.4708025861 (log normal)) 161.1195681661)
                               (* 288.1221695283 (expt (- normal 60) -0.0755148492)))))
         (b (funcall clamp (cond
                            ((> normal 65) 255)
                            ((< normal 20) 0)
                            (t (- (* 138.5177312231 (log (- normal 10))) 305.0447927307))))))
    (format "#%02X%02X%02X" r g b)))

(spaceline-define-segment all-the-icons-temperature
  "An `all-the-icons' segment to display the current temperature"
  (let* ((yahoo-weather-temperture-format "%d")
         (temperature (yahoo-weather-info-format yahoo-weather-info "%(temperature)"))
         (icon (if yahoo-weather-use-F "°F" "°C"))

         (icon-face `(:height ,(spaceline-all-the-icons--height 0.9)
                      :family ,(all-the-icons-wicon-family)
                      :foreground ,(spaceline-all-the-icons--temperature-color)
                      :background ,(face-background 'powerline-active2)))
         (text-face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))
    (propertize
     (concat
      (propertize (all-the-icons-wicon "thermometer-exterior") 'face icon-face)
      (unless spaceline-all-the-icons-slim-render (concat
                            (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.4) :inherit))
                            (propertize temperature 'face text-face)
                            (propertize icon 'face text-face))))
     'help-echo (format "Temperature is currently %s%s" temperature icon)
     'mouse-face (spaceline-all-the-icons--highlight)
     'display '(raise 0.1)))

  :tight t
  :when (and active
             (bound-and-true-p yahoo-weather-mode)
             (bound-and-true-p yahoo-weather-info)))

(spaceline-define-segment all-the-icons-weather
  "An `all-the-icons' segment to display an icon for the current weather"
  (let* ((weather (yahoo-weather-info-format yahoo-weather-info "%(weather)"))
         (help-echo (format "The weather in `%s' is currently `%s'" yahoo-weather-location weather))
         (icon (all-the-icons-icon-for-weather (downcase weather)))
         (icon-face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))
    
    (when (= 1 (length icon)) (plist-put icon-face :family (all-the-icons-wicon-family)))
    
    (propertize icon
                'face icon-face
                'display '(raise 0.1)
                'help-echo help-echo
                'mouse-face (spaceline-all-the-icons--highlight)))
  :tight t
  :when (and active
             (bound-and-true-p yahoo-weather-mode)
             (bound-and-true-p yahoo-weather-info)))

(provide 'spaceline-all-the-icons-segments)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons-segments.el ends here
