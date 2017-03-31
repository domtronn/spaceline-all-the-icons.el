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
(require 'all-the-icons)

;; Customization
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

(defmacro define-icon-set-getter (name)
  "Macro to create a getter function for icon set NAME."
  `(defun ,(intern (format "spaceline-all-the-icons-icon-set-%s" name)) ()
     ,(format "The Icon set to use for the `all-the-icons-%s' indicator." name)
     (let* ((icon-name (symbol-value (intern ,(format "spaceline-all-the-icons-icon-set-%s" name))))
            (icon-set (symbol-value (intern ,(format "spaceline-all-the-icons-icon-set--%s" name))))
            (result (alist-get icon-name icon-set)))
       (unless result (error "Unable to find key `%s' - See `spaceline-all-the-icons-icon-set-%s'" icon-name ,name))
       (car result))))

;; Icon set getters
(define-icon-set-getter "modified")
(define-icon-set-getter "bookmark")
(define-icon-set-getter "dedicated")
(define-icon-set-getter "git-stats")
(define-icon-set-getter "flycheck-slim")

;;; Modified Icon
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
    (pin        (("thumb-tack" . faicon)
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
(defcustom spaceline-all-the-icons-icon-set-git-stats 'github
  "The Icon set to use for the `all-the-icons-git-status' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "GitHub   - %s / %s"
                               (all-the-icons-octicon "diff-added" :v-adjust 0.0)
                               (all-the-icons-octicon "diff-removed" :v-adjust 0.0)) github)
          (const :tag "Arrows   - ↑ / ↓" arrows)))

(defconst spaceline-all-the-icons-icon-set--git-stats
  `((github (,(all-the-icons-octicon "diff-added")
             ,(all-the-icons-octicon "diff-removed")
             ,(all-the-icons-octicon-family)))
    (arrows ("↑" "↓"))))

;; Flycheck Slim Icons
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

;;; Helper functions
(defun spaceline-all-the-icons--separator (icon &optional left-padding right-padding)
  "Wrapper to render vertical line separator ICON with optional LEFT-PADDING & RIGHT-PADDING."
  (if spaceline-all-the-icons-slim-render " "
    (let ((raise (if (equal "|" icon) 0.2 0.0))
          (height (if (equal "|" icon) 0.9 1.2)))
      (concat
       (propertize (or left-padding "") 'face '(:height 0.8 :inherit))
       (propertize icon
                   'face `(:height ,height :inherit)
                   'display `(raise ,raise))
       (propertize (or right-padding left-padding "") 'face '(:height 0.8 :inherit))))))

(defun spaceline-all-the-icons--highlight ()
  "Return the `mouse-face' highlight face to be used when propertizing text.
This is done as a function rather than a static face as it
doesn't inherit all properties of a face."
  `((foreground-color . ,(face-foreground 'spaceline-all-the-icons-info-face))))

;;; First Divider Segments
(spaceline-define-segment all-the-icons-modified
  "An `all-the-icons' segment depiciting the current buffers state"
  (let* ((buffer-state (format-mode-line "%*"))
         (icon (cond
                ((equal buffer-state "-") (car (spaceline-all-the-icons-icon-set-modified)))
                ((equal buffer-state "*") (cdr (spaceline-all-the-icons-icon-set-modified)))
                ((equal buffer-state "%") "lock"))))

    (propertize (all-the-icons-faicon icon :v-adjust 0.0)
                'face `(:family ,(all-the-icons-faicon-family) :height 1.1 :inherit)
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map (make-mode-line-mouse-map 'mouse-1 'read-only-mode)))
  :tight t)

(spaceline-define-segment all-the-icons-bookmark
  "An `all-the-icons' segment allowing for easy bookmarking of files"
  (progn
    (unless (boundp 'bookmark-alist) (bookmark-all-names)) ;; Force bookmarks to load
    (let-alist (spaceline-all-the-icons-icon-set-bookmark)
      (let* ((bookmark-name (buffer-file-name))
             (bookmark (find-if (lambda (it) (equal bookmark-name (car it))) bookmark-alist)))

        (propertize (all-the-icons-faicon (if bookmark .icon.on .icon.off) :v-adjust 0.1)
                    'face      `(:family ,(all-the-icons-faicon-family) :inherit)
                    'help-echo  (if bookmark .echo.off .echo.on)
                    'mouse-face (spaceline-all-the-icons--highlight)
                    'local-map  (make-mode-line-mouse-map
                                 'mouse-1
                                 `(lambda () (interactive)
                                    (if ,(car bookmark)
                                        (bookmark-delete ,(car bookmark))
                                      (bookmark-set ,bookmark-name))
                                    (force-window-update)))))))

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
                'face       `(:family ,(funcall family-f) :inherit)
                'help-echo  "Toggle `window-dedidcated' for this window"
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map  (make-mode-line-mouse-map
                             'mouse-1
                             `(lambda () (interactive)
                                (set-window-dedicated-p ,window (not ,dedicated))
                                (force-window-update)))))
  :enabled nil)

(spaceline-define-segment all-the-icons-window-number
  "An `all-the-icons' segment depicting the current window number"
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
               ('string  (progn
                           (plist-put face :height 1.2)
                           (number-to-string window-num)))
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

(spaceline-define-segment all-the-icons-buffer-size
  "An `all-the-icons' segment depicting the buffer size"
  (propertize (format-mode-line "%I")
              'face '(:height 0.9 :inherit)
              'display '(raise 0.1))
  :tight t)

;;; Second Divider Segments
(spaceline-define-segment all-the-icons-projectile
  "An `all-the-icons' segment to indicate the current `projectile' project"
  (let ((help-echo "Switch Project")
        (raise (if spaceline-all-the-icons-slim-render 0.1 0.2))
        (local-map (make-mode-line-mouse-map 'mouse-1 'projectile-switch-project))
        (project-id (if (and (fboundp 'projectile-project-p) (projectile-project-p))
                        (projectile-project-name) "×")))

    (concat
     (spaceline-all-the-icons--separator "|" nil " ")
     (propertize project-id
                 'face `(:height ,(if spaceline-all-the-icons-slim-render 1.0 0.8) :inherit)
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
                'display `(raise ,(if spaceline-all-the-icons-slim-render -0.1 0.0))
                'face `(:height ,(if spaceline-all-the-icons-slim-render 1.3 1.1)
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
         (buffer-relative (or (cadr (split-string buffer-id project-root)) buffer-id))

         (path (file-name-directory buffer-relative))
         (file (file-name-nondirectory buffer-relative))

         (height (if spaceline-all-the-icons-slim-render 1.0 0.8))
         (raise  (if spaceline-all-the-icons-slim-render 0.1 0.2))
         (help-echo (format "Major-mode: `%s'" major-mode))

         (file-face `(:height ,height))
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
                  'face `(:height ,height :inherit)
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
   'face `(:height 0.8 :inherit)
   'display '(raise 0.2))
  :tight t)

(spaceline-define-segment all-the-icons-position
  "An `all-the-icons' Line & Column indicator"
  (propertize (format-mode-line "%l:%c")
              'face `(:height 0.9 :inherit)
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
                'face `(:height 1.3 :family ,(all-the-icons-material-family) :inherit)
                'local-map (make-mode-line-mouse-map 'mouse-1 'toggle-frame-fullscreen)))

  :tight t :enabled nil)

(spaceline-define-segment all-the-icons-text-scale
  "An `all-the-icons' indicator to show how much text has been scaled"
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

;; Fourth divider segments
(spaceline-define-segment all-the-icons-vc-icon
  "An `all-the-icons' segment to depict the current VC system with an icon"
  (cond ((string-match "Git[:-]" vc-mode)
         (propertize (all-the-icons-alltheicon "git")
                     'face `(:height 1.1 :family ,(all-the-icons-alltheicon-family) :inherit)
                     'display '(raise 0.1)))
        ((string-match "SVN-" vc-mode)
         (propertize (all-the-icons-material "cloud_download" :v-adjust -0.2)
                     'face `(:height 1.2 :family ,(all-the-icons-material-family))))
        (t ""))

  :when (and active vc-mode (not spaceline-all-the-icons-slim-render)))

(defun spaceline-all-the-icons--vc-git ()
  "Get the formatted GIT Version Control Icon based on variable `vc-mode'."
  (let* ((branch (cadr (split-string vc-mode "Git[:-]")))
         (git-branch (all-the-icons-octicon (if (equal branch "master") "git-merge" "git-branch")))
         (local-map (get-text-property 1 'local-map branch))
         (mouse-face (get-text-property 1 'mouse-face branch)))
    (propertize
     (concat
      (propertize git-branch
                  'face `(:family ,(all-the-icons-octicon-family) :inherit)
                  'display '(raise 0.1))
      (propertize (format " %s" branch)
                  'face `(:height 0.9 :inherit)
                  'display '(raise 0.1)))
     'mouse-face mouse-face
     'local-map local-map)))

(defun spaceline-all-the-icons--vc-svn ()
  "Get the formatted SVN Version Control Icon based on variable `vc-mode'."
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format "%s" revision)
                 'face '(:height 0.9)
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
         (icon-face `(:foreground ,(face-foreground face) :height ,height)))
    (when family (plist-put icon-face :family family))
    (concat
     (propertize icon 'face icon-face 'display '(raise 0.1))
     (propertize " " 'face '(:height 0.2))
     (propertize (format "%s" text) 'face `(:foreground ,(face-foreground face))))))

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
         (icon-face `(:foreground ,(face-foreground face) :height ,height)))
    (when family (plist-put icon-face :family family))
    (when text
     (concat
      (propertize icon 'face icon-face 'display `(raise ,raise))
      (propertize (format "%s" text) 'face `(:foreground ,(face-foreground face)))))))

(defun spaceline-all-the-icons--flycheck-status-slim ()
  "Render the mode line for Flycheck Status slim mode."
  (let-alist (flycheck-count-errors flycheck-current-errors)
    (let* ((get-text (lambda (text) (cond ((eq 'running flycheck-last-status-change) "?")
                                     ((zerop (or text 0)) nil)
                                     (t text))))

           (error-text (funcall get-text .error))
           (warn-text  (funcall get-text .warn))
           (info-text  (funcall get-text .info))

           (error-icon (car (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (warn-icon (cadr (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (help-icon (caddr (spaceline-all-the-icons-icon-set-flycheck-slim)))

           (family (cadddr (spaceline-all-the-icons-icon-set-flycheck-slim)))
           (space (propertize " " 'face '(:height 0.6))))

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
                ((string-match "✔" text) `(:height 0.9 :foreground ,(face-foreground 'success)))
                ((string-match "⚠" text) `(:height 0.9 :foreground ,(face-foreground 'warning)))
                ((string-match "✖ [0-9]" text) `(:height 0.9 :foreground ,(face-foreground 'error)))
                ((string-match "✖ Disabled" text) `(:height 0.9 :foreground ,(face-foreground 'font-lock-comment-face)))
                (t '(:height 0.9 :inherit)))))

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
      (propertize (format "%s %s" .info (all-the-icons-faicon "info" :v-adjust 0.0 :height 0.8))
                  'face `(:foreground ,(face-foreground 'spaceline-all-the-icons-info-face))
                  'help-echo "Show Flycheck Errors"
                  'mouse-face (spaceline-all-the-icons--highlight)
                  'local-map (make-mode-line-mouse-map 'mouse-1 'flycheck-list-errors))))
    :when (and active
               (not spaceline-all-the-icons-slim-render)
               (bound-and-true-p flycheck-last-status-change)))

(defvar spaceline-all-the-icons--package-updates nil)
(defun spaceline-all-the-icons--count-package-udpates (&rest args)
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
  (advice-add 'package-menu-execute :after 'spaceline-all-the-icons--count-package-updates)
  (advice-add 'package-refresh-contents :after 'spaceline-all-the-icons--count-package-updates))

(spaceline-define-segment all-the-icons-package-updates
  "An `all-the-icons' segment to display the number of package updates"
  (let ((face '(:height 0.9))
        (new-text  (when spaceline-all-the-icons-slim-render
                       (format "%s" (all-the-icons-material "new_releases" :v-adjust -0.2))))
        (update-text (concat
                      (format "%s" spaceline-all-the-icons--package-updates)
                      (unless spaceline-all-the-icons-slim-render " updates"))))

    (propertize
     (concat
      (propertize (all-the-icons-octicon "package" :v-adjust 0.1)
                  'face `(:height 1.1 :family ,(all-the-icons-octicon-family)))
      (propertize (or new-text "")
                  'face `(:height 1.2 :family ,(all-the-icons-material-family)))
      (propertize " " 'face '(:height 0.4))
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
        (height (or powerline-height (frame-char-height)))
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
             (not (equal "All" (format-mode-line "%p")))))

(spaceline-define-segment all-the-icons-buffer-position
  "An `all-the-icons' segment to show the buffer position as a percentage"
  (if (string-match "\%" (format-mode-line "%p"))
      (format-mode-line "%p%%")
      (format-mode-line "%p"))
  :enabled nil :when (not spaceline-all-the-icons-slim-render))

;; Second Right divider segments
(spaceline-define-segment all-the-icons-battery-status
  "An `all-the-icons' segment to show the battery information"
  (let* ((charging?  (equal "AC" (alist-get ?L fancy-battery-last-status)))
         (percent    (string-to-int (alist-get ?p fancy-battery-last-status)))
         (time       (alist-get ?t fancy-battery-last-status))

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

         (icon-face `(:height 1.0 :family ,(funcall family-f) :background ,(face-background default-face)))
         (text-face `(:height 0.9 :background ,(face-background default-face))))

    (let-alist icon-alist
      (when .height (plist-put icon-face :height .height))
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
      (propertize (format-time-string "%H:%M ") 'face '(:height 0.9 :inherit) 'display '(raise 0.1))
      (propertize icon
                  'face `(:height 0.9 :family ,(all-the-icons-wicon-family) :inherit)
                  'display '(raise 0.1)))
      'help-echo `(format-time-string "%H:%M")
      'mouse-face (spaceline-all-the-icons--highlight)))
  :tight t
  :enabled t)

;; Middle Divider Segments
(spaceline-define-segment all-the-icons-which-function
  "An `all-the-icons' segment to show the `which-function-mode' function"
  (let* ((current (format-mode-line which-func-current))
         (unknown? (equal current which-func-unknown))
         (icon (if unknown? (all-the-icons-octicon "circle-slash") "ƒ"))

         (face '(:height 0.8 :inherit))
         (icon-face `(:height ,(if unknown? 1.2 1.0) :inherit)))

    (when unknown? (plist-put icon-face :family (all-the-icons-octicon-family)))
    (when (string-match "{\\(.*\\)}" current) (setq current (match-string 1 current)))

    (propertize
     (concat (propertize icon 'face icon-face 'display '(raise 0.1))
             (unless unknown? (propertize "::" 'face face 'display '(raise 0.2)))
             (unless unknown? (propertize current 'face face 'display '(raise 0.2))))
     'mouse-face (spaceline-all-the-icons--highlight)
     'local-map which-func-keymap
     'help-echo "mouse-1: go to beginning\n\
mouse-2: toggle rest visibility\n\
mouse-3: go to end"))

  :when (and active
             (bound-and-true-p which-function-mode)
             (bound-and-true-p which-func-mode)))

(provide 'spaceline-all-the-icons)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons.el ends here
