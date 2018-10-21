;;; spaceline-all-the-icons-segments.el --- Segments used by Spaceline All The Icons Theme

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

(require 'memoize)
(require 'spaceline)
(require 'all-the-icons)

;;; Forward declarations of Optional Dependencies
(declare-function projectile-project-root "ext:projectile.el")
(declare-function yahoo-weather-info-format "ext:yahoo-weather.el")
(declare-function flycheck-count-errors  "ext:flycheck.el")
(declare-function anzu--format-here-position "ext:anzu.el")
(declare-function neo-buffer--get-filename-current-line "ext:neotree.el")
(declare-function neo-buffer--get-node-index "ext:neotree.el")
(declare-function neo-buffer--get-nodes "ext:neotree.el")
(declare-function git-gutter-hunk-end-line "ext:git-gutter.el")
(declare-function git-gutter-hunk-start-line "ext:git-gutter.el")
(declare-function git-gutter-hunk-type "ext:git-gutter.el")
(declare-function git-gutter-hunk-content "ext:git-gutter.el")
(declare-function diff-hl-changes "ext:diff-hl.el")
(declare-function paradox-list-packages "ext:paradox.el")
(declare-function winum-get-number "ext:winum.el")
(declare-function window-numbering-get-number "ext:window-numbering.el")
(declare-function eyebrowse--get "ext:eyebrowse.el")
(declare-function mc/num-cursors "ext:multiple-cursors.el")
(declare-function fancy-narrow-active-p "ext:fancy-narrow.el")

(defvar flycheck-current-errors)
(defvar flycheck-last-status-change)
(defvar anzu--state)
(defvar anzu--overflow-p)
(defvar neo-buffer--start-node)
(defvar git-gutter:diffinfos)
(defvar git-gutter+-diffinfos)
(defvar org-clock-current-task)

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

;;; Window Numbering Icon
(defcustom spaceline-all-the-icons-icon-set-window-numbering 'circle
  "The Icon set to use for the `all-the-icons-window-number' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag "Circle Outline - ①" circle)
          (const :tag "Circle Solid   - ➊" solid)
          (const :tag "Normal String  - 1" string)
          (const :tag ,(format "Square         - %s" (all-the-icons-material "filter_1" :v-adjust 0.0)) square)))

;;; Eyebrowse Icon
(defcustom spaceline-all-the-icons-icon-set-eyebrowse-slot 'circle
  "The Icon set to use for the `all-the-icons-eyebrowse-slot' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag "Circle Outline - ①" circle)
          (const :tag "Circle Solid   - ➊" solid)
          (const :tag "Normal String  - 1" string)
          (const :tag ,(format "Square         - %s" (all-the-icons-material "filter_1" :v-adjust 0.0)) square)))

;;; Git Statistics Icon
(define-spaceline-all-the-icons--icon-set-getter "git-stats")
(defcustom spaceline-all-the-icons-icon-set-git-stats 'diff-icons
  "The Icon set to use for the `all-the-icons-git-status' indicator."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "GitHub   - %s / %s / %s"
                               (all-the-icons-octicon "diff-added" :v-adjust 0.0)
                               (all-the-icons-octicon "diff-removed" :v-adjust 0.0)
                               (all-the-icons-octicon "diff-modified" :v-adjust 0.0)) diff-icons)
          (const :tag "Arrows   - 🡑 / 🡓 / •" arrows)))

(defconst spaceline-all-the-icons-icon-set--git-stats
  `((diff-icons (,(all-the-icons-octicon "diff-added" :v-adjust 0.1)
                 ,(all-the-icons-octicon "diff-removed" :v-adjust 0.1)
                 ,(all-the-icons-octicon "diff-modified" :v-adjust 0.1)))
    (arrows (,(propertize "🡑" 'display '(raise 0.0))
             ,(propertize "🡓" 'display '(raise 0.0))
             ,(propertize "•" 'display '(raise 0.0))))))

;;; Flycheck Slim Icons
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
              ,(all-the-icons-material "info" :v-adjust -0.2)))
    (outline (,(all-the-icons-material "error_outline" :v-adjust -0.2)
              ,(all-the-icons-material "help_outline" :v-adjust -0.2)
              ,(all-the-icons-material "info_outline" :v-adjust -0.2)))
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

;;; Git Ahead Icons
(define-spaceline-all-the-icons--icon-set-getter "git-ahead")
(defcustom spaceline-all-the-icons-icon-set-git-ahead 'arrow
  "The Icon set to use for the `all-the-icons-git-ahead'."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Arrow   - 🡅") arrow)
          (const :tag ,(format "Commit  - %s" (all-the-icons-octicon "git-commit" :v-adjust -0.2)) commit)))

(defconst spaceline-all-the-icons-icon-set--git-ahead
  `((arrow ,(propertize "🡅" 'face '(:inherit)))
    (commit ,(propertize (all-the-icons-octicon "git-commit" :v-adjust 0)
                         'face `(:family ,(all-the-icons-octicon-family) :inherit)))))

;;; Git Status Icons
(define-spaceline-all-the-icons--icon-set-getter "vc-icon-git")
(defcustom spaceline-all-the-icons-icon-set-vc-icon-git 'git-logo
  "The Icon set to use for the `all-the-icons-vc-icon' when in a git repository."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Git Name    - %s" (all-the-icons-faicon "git" :v-adjust -0.2)) git-name)
          (const :tag ,(format "Git Logo    - %s" (all-the-icons-alltheicon "git" :v-adjust -0.2)) git-logo)
          (const :tag ,(format "GitHub      - %s" (all-the-icons-faicon "github" :v-adjust -0.2)) github-logo)
          (const :tag ,(format "GitHub Name - %s" (all-the-icons-octicon "logo-github" :v-adjust -0.2)) github-name)
          (const :tag ,(format "Octocat     - %s" (all-the-icons-faicon "github-alt" :v-adjust -0.2)) octocat)
          (const :tag ,(format "GitLab      - %s" (all-the-icons-faicon "gitlab" :v-adjust -0.2)) gitlab)))

(defconst spaceline-all-the-icons-icon-set--vc-icon-git
  `((git-name ,(all-the-icons-faicon "git"))
    (git-logo ,(all-the-icons-alltheicon "git"))
    (github-logo ,(all-the-icons-faicon "github"))
    (github-name ,(all-the-icons-octicon "logo-github"))
    (octocat ,(all-the-icons-faicon "github-alt"))
    (gitlab ,(all-the-icons-faicon "gitlab"))))

;;; Multiple Cursors Icons
(define-spaceline-all-the-icons--icon-set-getter "mc")
(defcustom spaceline-all-the-icons-icon-set-mc 'caret
  "The Icon set to use for the `all-the-icons-multiple-cursors' segment."
  :group 'spaceline-all-the-icons-icon-set
  :type `(radio
          (const :tag ,(format "Typing Caret  - %s" (all-the-icons-faicon "i-cursor" :v-adjust -0.2)) caret)
          (const :tag ,(format "Mouse Pointer - %s" (all-the-icons-faicon "mouse-pointer" :v-adjust -0.2)) pointer)))

(defconst spaceline-all-the-icons-icon-set--mc
  `((caret ,(all-the-icons-faicon "i-cursor" :v-adjust 0.1))
    (pointer ,(all-the-icons-faicon "mouse-pointer" :v-adjust 0.1))))

;; Custom settings
(defcustom spaceline-all-the-icons-window-number-always-visible nil
  "Whether or not to show the window number all the time or when there are multiple windows."
  :group 'spaceline-all-the-icons
  :type 'boolean)

(defcustom spaceline-all-the-icons-eyebrowse-display-name t
  "Whether or not to disable the current workspace name as part of the modeline."
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

(defcustom spaceline-all-the-icons-clock-always-visible t
  "Whether or not to always show the time & clock segment.

When non-nil, this will display the clock & time segment all the time.
When nil, this segment will only display when in a fullscreen frame."
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

(defcustom spaceline-all-the-icons-hide-long-buffer-path nil
  "Whether or not to hide the buffer path when longer than 1/4 `window-text-width'."
  :group 'spaceline-all-the-icons
  :type 'boolean)

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

(defun spaceline-all-the-icons--memoized-file-truename (file-path)
  "A function to be memoized when calculating the truename of FILE-PATH."
  (file-truename file-path))
(memoize 'spaceline-all-the-icons--memoized-file-truename)

(defun spaceline-all-the-icons--highlight ()
  "Return the `mouse-face' highlight face to be used when propertizing text.
This is done as a function rather than a static face as it
doesn't inherit all properties of a face."
  `((foreground-color . ,(spaceline-all-the-icons--face-foreground 'spaceline-all-the-icons-info-face))))

(defun spaceline-all-the-icons--highlight-background ()
  "Return a `mouse-face' to highlight the background when focussed."
  `((background-color . ,(spaceline-all-the-icons--face-foreground 'spaceline-all-the-icons-info-face))))

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
    (let-alist (spaceline-all-the-icons-icon-set-bookmark)
      (let* ((bookmark-name (buffer-file-name))
             (bookmark (cl-find-if (lambda (it) (string= bookmark-name (car it))) bookmark-alist)))

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

  :when (and (buffer-file-name)
             (boundp 'bookmark-alist)) :enabled nil)

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

(defun spaceline-all-the-icons--window-number-show-p ()
  "Function to decide whether `window-number' segment should be shown."
  (or spaceline-all-the-icons-window-number-always-visible
      (> (length (window-list)) 1)))

(defun spaceline-all-the-icons--window-number ()
  "Ubiquitous function to return the current window number."
  (let ((window-num
         (cond
           ((bound-and-true-p winum-mode) (winum-get-number))
           ((bound-and-true-p window-numbering-mode) (window-numbering-get-number)))))
    (when (numberp window-num) window-num)))

(defun spaceline-all-the-icons--window-number-icon (window-num &optional icon-set)
  "Return the icon to use for WINDOW-NUM from ICON-SET.
ICON-SET defaults to `spaceline-all-the-icons-icon-set-window-numbering'."
  (let* ((face `(:height ,(spaceline-all-the-icons--height 1.2) :inherit))
         (icon-set (if (> window-num 9) 'string
                     (or icon-set
                         spaceline-all-the-icons-icon-set-window-numbering)))
         (icon (cl-case icon-set
                 (solid   (format "%c" (+ window-num 10121)))
                 (circle  (format "%c" (+ window-num 9311)))
                 (string  (progn (number-to-string window-num)))
                 (square  (progn
                            (setq face (append `(:height ,(spaceline-all-the-icons--height 0.9)) face))
                            (setq face (append `(:family ,(all-the-icons-material-family)) face))
                            (all-the-icons-material (format "filter_%s" window-num) :v-adjust -0.1))))))
    (propertize icon 'face face)))

(spaceline-define-segment all-the-icons-window-number
  "An `all-the-icons' segment depicting the current window number"
  (spaceline-all-the-icons--window-number-icon
   (spaceline-all-the-icons--window-number)
   spaceline-all-the-icons-icon-set-window-numbering)
  :when (and
         (spaceline-all-the-icons--window-number)
         (spaceline-all-the-icons--window-number-show-p)))

(spaceline-define-segment all-the-icons-eyebrowse-workspace
  "An `all-the-icons' segment to display the current eyebrowse
  workspace. Requires `eyebrowse-mode' to be enabled."
  (let* ((num (eyebrowse--get 'current-slot))
         (tag (when num (cl-caddr (assoc num (eyebrowse--get 'window-configs)))))

         (eyebrowse-slot (spaceline-all-the-icons--window-number-icon
                          num spaceline-all-the-icons-icon-set-eyebrowse-slot))
         (eyebrowse-new
          (propertize (all-the-icons-octicon "eye")
                               'display '(raise 0.1)
                               'face `( :height ,(spaceline-all-the-icons--height 1.2) :family ,(all-the-icons-octicon-family) :inherit)))
         (eyebrowse-tag-p (and (not (string= "" tag)) spaceline-all-the-icons-eyebrowse-display-name))
         (eyebrowse-tag
          (when eyebrowse-tag-p
            (concat
             (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator nil "")
             (propertize tag
                         'display '(raise 0.2)
                         'face `(:slant italic :height ,(spaceline-all-the-icons--height 0.9) :inherit))))))

    (concat (propertize eyebrowse-new
                        'mouse-face (spaceline-all-the-icons--highlight)
                        'local-map (make-mode-line-mouse-map 'mouse-1 'eyebrowse-create-window-config)
                        'help-echo "Create new Eyebrowse window config")
            (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator nil "")
            (propertize (concat eyebrowse-slot  eyebrowse-tag)
                        'mouse-face (spaceline-all-the-icons--highlight)
                        'local-map (make-mode-line-mouse-map 'mouse-1 'eyebrowse-switch-to-window-config)
                        'help-echo "Switch Eyebrowse window config")))
  :when (bound-and-true-p eyebrowse-mode))

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
     (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator nil " ")
     (propertize project-id
                 'face `(:height ,(spaceline-all-the-icons--height height) :inherit)
                 'mouse-face (spaceline-all-the-icons--highlight)
                 'display `(raise ,raise)
                 'help-echo help-echo
                 'local-map local-map)
     (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator " " "")))
  :tight t)

(spaceline-define-segment all-the-icons-mode-icon
  "An `all-the-icons' segment indicating the current buffer's mode with an icon"
  (let ((icon (all-the-icons-icon-for-mode major-mode)))
    (unless (symbolp icon)
      (propertize icon
                  'help-echo (format "Major-mode: `%s'" major-mode)
                  'display '(raise 0)
                  'face `(:height ,(spaceline-all-the-icons--height 1.1)
                          :family ,(all-the-icons-icon-family-for-mode major-mode)
                          :inherit)))))

(spaceline-define-segment all-the-icons-buffer-id
  "An `all-the-icons' segment to display current buffer id"
  (let* ((height (if spaceline-all-the-icons-slim-render 1.0 0.8))
         (raise  (if spaceline-all-the-icons-slim-render 0.1 0.2))

         (help-echo (format "Major-mode: `%s'" major-mode))

         (file-face `(:height ,(spaceline-all-the-icons--height height)))
         (show-path? (and active
                          spaceline-all-the-icons-buffer-path-p
                          (spaceline-all-the-icons--buffer-path)
                          (not spaceline-all-the-icons-slim-render)))

         (have-projectile? (and (fboundp 'projectile-project-p) (projectile-project-p)))
         (show-projectile? (and spaceline-all-the-icons-projectile-p have-projectile?))

         (buffer-id (if (and (buffer-file-name)
                             (or show-path? show-projectile?))
                        (file-name-nondirectory (buffer-file-name))
                        (format-mode-line "%b")))

         (mouse-f (if have-projectile? 'projectile-find-file 'find-file)))

    (if (not (and spaceline-all-the-icons-highlight-file-name
                  show-path?))
        (add-to-list 'file-face :inherit t)
      (setq file-face (append `(:background ,(spaceline-all-the-icons--face-background default-face)) file-face))
      (setq file-face (append `(:foreground ,(or spaceline-all-the-icons-file-name-highlight
                                                 (spaceline-all-the-icons--face-background highlight-face))) file-face)))

    (propertize buffer-id
                'face file-face
                'display `(raise ,raise)
                'help-echo help-echo
                'mouse-face (spaceline-all-the-icons--highlight)
                'local-map (make-mode-line-mouse-map 'mouse-1 mouse-f)))
  :tight t)

(defun spaceline-all-the-icons--buffer-path ()
  "Get buffer path based on home directory and function `projectile-project-root'."
  (when (buffer-file-name)
    (let* ((name (spaceline-all-the-icons--memoized-file-truename (buffer-file-name)))

           (project-root (when (and (not (file-remote-p (buffer-file-name)))
                                    spaceline-all-the-icons-projectile-p
                                    (fboundp 'projectile-project-p)
                                    (projectile-project-p))
                           (spaceline-all-the-icons--memoized-file-truename
                            (ignore-errors (projectile-project-root)))))

           (path-relative (or (cadr (split-string name project-root))
                              (replace-regexp-in-string (getenv "HOME") "~" name)))
           (limit (/ (window-text-width) 4))
           (result (file-name-directory path-relative)))

      (unless (and spaceline-all-the-icons-hide-long-buffer-path
                   (> (length result) limit)) result))))

(spaceline-define-segment all-the-icons-buffer-path
  "An `all-the-icons' segment to display the path for the current buffer.
It is only enabled when you're not in a project or if the projectile segment is disabled."
  (let ((height (if spaceline-all-the-icons-slim-render 1.0 0.8))
        (raise  (if spaceline-all-the-icons-slim-render 0.1 0.2))
        (path   (spaceline-all-the-icons--buffer-path)))

    (when path
      (propertize path
                  'face `(:height ,(spaceline-all-the-icons--height height) :inherit)
                  'display `(raise ,raise))))

  :when (and active
             (buffer-file-name)
             (not spaceline-all-the-icons-slim-render)))

;;; Third Divider Segments
(spaceline-define-segment all-the-icons-process
  "An `all-the-icons' segment to depict the current process"
  (let* ((process (format-mode-line mode-line-process))
         (show-mode? (or (symbolp (all-the-icons-icon-for-mode major-mode)) mode-line-process)))

    (propertize
     (concat
      (when show-mode? (format-mode-line "%m"))
      (when process process))

     'face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit)
     'mouse-face (spaceline-all-the-icons--highlight)
     'display '(raise 0.2)))
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
       (propertize text 'face `(:height ,(spaceline-all-the-icons--height) :inherit) 'display '(raise 0.1)))))
  :tight t :enabled nil
  :when (bound-and-true-p text-scale-mode-lighter))

(spaceline-define-segment all-the-icons-multiple-cursors
  "An `all-the-icons' segment to display the number of multiple cursors active."
  (concat
   (propertize (spaceline-all-the-icons-icon-set-mc)
               'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit))
   (propertize " " 'display '(space . (:width (2))))
   (propertize (format "%d" (mc/num-cursors)) 'face `(:height (spaceline-all-the-icons--height 0.9) :inherit)
               'display '(raise 0.1)))

  :when (bound-and-true-p multiple-cursors-mode))

(spaceline-define-segment all-the-icons-narrowed
  "An `all-the-icons' segment to indicate whether the current buffer is narrowed."
  (propertize (all-the-icons-faicon "filter" :v-adjust 0.1)
              'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)
              'help-echo "mouse-1: Widen the current file"
              'mouse-face (spaceline-all-the-icons--highlight)
              'local-map (make-mode-line-mouse-map 'mouse-1 'widen))

  :tight t
  :when (or (buffer-narrowed-p)
            (and (bound-and-true-p fancy-narrow-mode)
                 (fancy-narrow-active-p))))

;;; Fourth divider segments
(spaceline-define-segment all-the-icons-vc-icon
  "An `all-the-icons' segment to depict the current VC system with an icon"
  (cond ((string-match "Git[:-]" vc-mode)
         (propertize (spaceline-all-the-icons-icon-set-vc-icon-git)
                     'face `(:height ,(spaceline-all-the-icons--height 1.1)
                             :family ,(all-the-icons-icon-family (spaceline-all-the-icons-icon-set-vc-icon-git))
                             :inherit)
                     'display '(raise 0.1)))
        ((string-match "SVN-" vc-mode)
         (propertize (all-the-icons-material "cloud_download" :v-adjust -0.2)
                     'face `(:height ,(spaceline-all-the-icons--height 1.2) :family ,(all-the-icons-material-family)))))

  :when (and active vc-mode (not spaceline-all-the-icons-slim-render)))

(defun spaceline-all-the-icons--vc-git ()
  "Get the formatted GIT Version Control Icon based on variable `vc-mode'."
  (let* ((branch (cadr (split-string vc-mode "Git[:-]")))
         (git-branch (all-the-icons-octicon (if (string= branch "master") "git-merge" "git-branch")))
         (local-map (get-text-property 1 'local-map branch)))
    (propertize
     (concat
      (propertize git-branch
                  'face `(:family ,(all-the-icons-octicon-family) :height ,(spaceline-all-the-icons--height 0.9) :inherit)
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

(defun spaceline-all-the-icons--git-stats (icon text face)
  "Wrapper to render git statistics ICON with TEXT using FACE.
When FAMILY is provided, put `:family' property into face."
  (let* ((family (all-the-icons-icon-family icon))
         (height (if family 1.0 1.2))
         (icon-face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
                      :height ,(spaceline-all-the-icons--height height))))

    (when family (setq icon-face (append `(:family ,family) icon-face)))
    (concat
     (propertize icon 'face icon-face)
     (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.2)))
     (propertize (format "%s" text)
                 'face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
                         :height ,(spaceline-all-the-icons--height height))))))

(defmacro spaceline-all-the-icons--git-stats-reducer (name el-f sl-f hunk-f type-f)
  "Macro to define reducer to calculate Added, Deleted & Modified lines in git.
NAME should be an id to define that reducer.  EL-F & SL-F are
forms that, will calculate the end & start lines of a diff
respetively.  HUNK-F is a form which will return the hunk
contents.  TYPE-F is a form which will evaluate the
type, (i.e. added, deleted, modified) of a diff/hunk."
  `(defun ,(intern (format "spaceline-all-the-icons--git-stats-reducer-%s" name)) (acc it)
     ,(format "A reducer to count added, deleted & modified lines for `%s'" name)
     (cl-destructuring-bind (added removed modified) acc
       (let ((lines (1+ (- ,el-f ,sl-f)))
             (hunk ,hunk-f)
             (type ,type-f))
         (cl-case type
           (deleted (list added (+ removed (with-temp-buffer (insert hunk) (1- (count-lines (point-min) (point-max))))) modified))
           (added (list (+ added lines) removed modified))
           (modified (list added removed (+ modified lines))))))))

(spaceline-all-the-icons--git-stats-reducer diffinfos (git-gutter-hunk-end-line it) (git-gutter-hunk-start-line it) (git-gutter-hunk-content it) (git-gutter-hunk-type it))
(spaceline-all-the-icons--git-stats-reducer +diffinfos (plist-get it :end-line) (plist-get it :start-line) (plist-get it :content) (plist-get it :type))
(spaceline-all-the-icons--git-stats-reducer diffhl (cadr it) 1 (make-string (1+ (cadr it)) 10) (cl-case (cl-caddr it) (insert 'added) (change 'modified) ('delete 'deleted)))

(defun spaceline-all-the-icons--git-statistics ()
  "Function to return a list of added, removed and modified lines in current file."
  (cond
   ((bound-and-true-p git-gutter+-diffinfos)
    (cl-reduce 'spaceline-all-the-icons--git-stats-reducer-+diffinfos git-gutter+-diffinfos :initial-value '(0 0 0)))
   ((bound-and-true-p git-gutter-mode)
    (cl-reduce 'spaceline-all-the-icons--git-stats-reducer-diffinfos git-gutter:diffinfos :initial-value '(0 0 0)))
   ((and (bound-and-true-p diff-hl-mode) (ignore-errors (diff-hl-changes)))
    (cl-reduce 'spaceline-all-the-icons--git-stats-reducer-diffhl (diff-hl-changes) :initial-value '(0 0 0)))
   (t '(0 0 0))))

(spaceline-define-segment all-the-icons-git-status
  "An `all-the-icons' segment to display Added/Removed stats for files under git VC."
  (cl-destructuring-bind (added removed modified) (spaceline-all-the-icons--git-statistics)
    (cl-destructuring-bind (added-icon removed-icon modified-icon) (spaceline-all-the-icons-icon-set-git-stats)
      (let* ((space (propertize " " 'face `(:height ,(if spaceline-all-the-icons-slim-render 0.2 1.0))))
             (icons (list
                     (unless (zerop added) (spaceline-all-the-icons--git-stats added-icon added 'success))
                     (unless (zerop removed) (spaceline-all-the-icons--git-stats removed-icon removed 'error))
                     (unless (zerop modified) (spaceline-all-the-icons--git-stats modified-icon modified 'warning)))))
        (propertize
         (mapconcat 'identity (cl-remove-if 'not icons) space)
         'help-echo "View Diff of current file"
         'mouse-face (spaceline-all-the-icons--highlight)
         'local-map (make-mode-line-mouse-map 'mouse-1 'vc-ediff)))))

  :when (and active
             (not (equal '(0 0 0) (spaceline-all-the-icons--git-statistics)))))

(defvar spaceline-all-the-icons--git-ahead 0 "The number of commits ahead the current buffer is.")
(defun spaceline-all-the-icons--git-ahead-update (&rest args)
  "Update the current git ahead  ARGS is just placeholder."
  (when (and spaceline-all-the-icons-git-ahead-p
             buffer-file-name vc-mode (string-match "Git" vc-mode))
    (setq-local spaceline-all-the-icons--git-ahead
                (with-temp-buffer
                  (ignore-errors (vc-git-log-outgoing (current-buffer) ""))
                  (if (string-match-p "^fatal:" (buffer-string)) 0
                    (count-lines (point-min) (point-max)))))))

(spaceline-define-segment all-the-icons-git-ahead
  "An `all-the-icons' segment to display the number of commits a git branch is a head of upstream."
  (when (> spaceline-all-the-icons--git-ahead 0)
    (propertize
     (concat
      (spaceline-all-the-icons-icon-set-git-ahead)
      (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.3) :inherit))
      (propertize (format "%s" spaceline-all-the-icons--git-ahead) 'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))
     'mouse-face (spaceline-all-the-icons--highlight)
     'help-echo (format "You are currently %s commit%s ahead of `%s'"
                        spaceline-all-the-icons--git-ahead
                        (if (= spaceline-all-the-icons--git-ahead 1) "" "s")
                        (cadr (split-string vc-mode "Git[:-]")))))
  :tight t
  :enabled nil
  :when (and active vc-mode buffer-file-name
             (string-match "Git" vc-mode)))

;;; Flycheck
(defun spaceline-all-the-icons--flycheck-pip (icon text face)
  "Wrapper to render flycheck status ICON with TEXT using FACE.
When FAMILY is provided, put `:family' property into face."
  (let* ((height 1.0)
         (family (all-the-icons-icon-family icon))
         (raise (if (> (length (spaceline-all-the-icons-icon-set-flycheck-slim)) 3) -0.2 0.0))
         (icon-face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
                      :height ,(spaceline-all-the-icons--height height))))
    (when family (setq icon-face (append `(:family ,family) icon-face)))
    (when text
     (concat
      (propertize icon 'face icon-face 'display `(raise ,raise))
      (propertize (format "%s" text)
                  'face `(:foreground ,(spaceline-all-the-icons--face-foreground face)
                          :height ,(spaceline-all-the-icons--height)))))))

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
           (help-icon (cl-caddr (spaceline-all-the-icons-icon-set-flycheck-slim)))

           (space (propertize " " 'face `(:height ,(spaceline-all-the-icons--height 0.6)))))

      (mapconcat
       'identity
       (cl-remove-if
        'null
        `(,(spaceline-all-the-icons--flycheck-pip error-icon error-text 'error)
          ,(spaceline-all-the-icons--flycheck-pip warn-icon warn-text 'warning)
          ,(spaceline-all-the-icons--flycheck-pip help-icon info-text 'spaceline-all-the-icons-info-face)))
       space))))

(defun spaceline-all-the-icons--flycheck-finished ()
  "Get the string for finished status of Flycheck."
  (let* ((count (let-alist (flycheck-count-errors flycheck-current-errors)
                  (+ (or .warning 0) (or .error 0))))
         (plural (if (eq 1 count) "" "s")))
    (if flycheck-current-errors (format "✖ %s Issue%s" count plural) "✔ No Issues")))

(defun spaceline-all-the-icons--flycheck-status ()
  "Render the mode line for Flycheck Status in a more verbose fashion."
  (let* ((text (cl-case flycheck-last-status-change
                 (finished    (spaceline-all-the-icons--flycheck-finished))
                 (running     (concat (all-the-icons-faicon "refresh") " Running"))
                 (no-checker  "⚠ No Checker")
                 (not-checked "✖ Disabled")
                 (errored     "⚠ Error")
                 (interrupted "⛔ Interrupted")))
         (face (cond
                ((string-match "✔" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(spaceline-all-the-icons--face-foreground 'success)))
                ((string-match "⚠" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(spaceline-all-the-icons--face-foreground 'warning)))
                ((string-match "✖ [0-9]" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(spaceline-all-the-icons--face-foreground 'error)))
                ((string-match "✖ Disabled" text) `(:height ,(spaceline-all-the-icons--height 0.9) :foreground ,(spaceline-all-the-icons--face-foreground 'font-lock-comment-face)))
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
                  'face `(:foreground ,(spaceline-all-the-icons--face-foreground 'spaceline-all-the-icons-info-face))
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

(spaceline-define-segment all-the-icons-package-updates
  "An `all-the-icons' segment to display the number of package updates"
  (let ((face `(:height ,(spaceline-all-the-icons--height 0.9)))
        (mouse-f (if (fboundp 'paradox-list-packages) 'paradox-list-packages 'package-list-packages))
        (new-text    (when spaceline-all-the-icons-slim-render
                       (format "%s" (all-the-icons-material "new_releases" :v-adjust -0.2))))
        (update-text (concat
                      (format "%s" spaceline-all-the-icons--package-updates)
                      (unless spaceline-all-the-icons-slim-render
                        (format " update%s"
                                (if (= 1 spaceline-all-the-icons--package-updates) "" "s"))))))

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
     'local-map (make-mode-line-mouse-map 'mouse-1 mouse-f)))

  :when (and active
             (numberp spaceline-all-the-icons--package-updates)
             (> spaceline-all-the-icons--package-updates 0)))

;;; First Right divider segments
;; Org task
(spaceline-define-segment all-the-icons-org-clock-current-task
  "An `all-the-icons' segment to display the current org-clock task."
  (let ((face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit)))
    (propertize
     (concat
      (propertize (all-the-icons-faicon "check-circle" :v-adjust 0.1)
                  'face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-faicon-family) :inherit))
      " "
      (propertize (truncate-string-to-width org-clock-current-task 20 nil nil "…")
                  'face face
                  'display '(raise 0.1)))
     'help-echo "Go to task"
     'mouse-face (spaceline-all-the-icons--highlight)
     'local-map (make-mode-line-mouse-map 'mouse-1 #'org-clock-goto)))
  :when (and active
             (bound-and-true-p org-clock-current-task)))

(spaceline-define-segment all-the-icons-hud
  "An `all-the-icons' segment to show the position through buffer HUD indicator."
  (let ((color (spaceline-all-the-icons--face-foreground default-face))
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

;;; Second Right divider segments
(spaceline-define-segment all-the-icons-battery-status
  "An `all-the-icons' segment to show the battery information"
  (let* ((charging?  (string= "AC" (cdr (assoc ?L fancy-battery-last-status))))
         (percent    (string-to-number (cdr (assoc ?p fancy-battery-last-status))))
         (time       (cdr (assoc ?t fancy-battery-last-status)))

         (icon-alist
          (cond
           (charging? '((icon . "charging") (inherit . fancy-battery-charging) (height . 1.3) (raise . 0.0)))
           ((> percent 95) '((icon . "full") (inherit . fancy-battery-charging)))
           ((> percent 70) '((icon . "three-quarters")))
           ((> percent 30) '((icon . "half")))
           ((> percent 15) '((icon . "quarter") (inherit . fancy-battery-discharging)))
           (t '((icon . "empty") (inherit . fancy-battery-critical)))))

         (icon-set (if charging? 'alltheicon 'faicon))
         (icon-f   (all-the-icons--function-name icon-set))
         (family-f (all-the-icons--family-name icon-set))

         (icon-face `(:family ,(funcall family-f) :background ,(spaceline-all-the-icons--face-background default-face)))
         (text-face `(:height ,(spaceline-all-the-icons--height 0.9) :background ,(spaceline-all-the-icons--face-background default-face))))

    (let-alist icon-alist
      (setq icon-face (append `(:height ,(spaceline-all-the-icons--height (when .height .height))) icon-face))
      (if (not .inherit)
          (setq icon-face (append icon-face '(:inherit))
                text-face (append text-face '(:inherit)))
        (setq icon-face (append icon-face `(:inherit ,(macroexpand .inherit)))
              text-face (append text-face `(:inherit ,(macroexpand .inherit)))))

      (propertize
       (concat
        (propertize (funcall icon-f (format "battery-%s" .icon))
                    'face icon-face
                    'display `(raise ,(or .raise 0.0)))
        (propertize (cond
                     (spaceline-all-the-icons-slim-render "")
                     (charging? (format " %s%%%% " percent))
                     (t (format " %s " time)))
                    'face text-face
                    'display '(raise 0.1)))
       'help-echo `(format "%s Remaining" ,time)
       'mouse-face (spaceline-all-the-icons--highlight))))

  :global-override fancy-battery-mode-line
  :when (and active (bound-and-true-p fancy-battery-mode)))

(spaceline-define-segment all-the-icons-time
  "An `all-the-icons' segment to to display the time and a clock icon"
  (let* ((time (string-to-number (format-time-string "%I")))
         (time-icon (all-the-icons-wicon (format "time-%s" time) :v-adjust 0.0))
         (date-icon (all-the-icons-octicon "calendar" :v-adjust 0.0)))

    (propertize
     (concat
      (propertize (format-time-string "%H:%M ") 'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit) 'display '(raise 0.1))
      (propertize time-icon
                  'face `(:height ,(spaceline-all-the-icons--height 0.9) :family ,(all-the-icons-wicon-family) :inherit)
                  'display '(raise 0.1))

      (when (bound-and-true-p display-time-day-and-date)
        (concat
         (propertize (format-time-string " %a %b %d ") 'face `(:height ,(spaceline-all-the-icons--height 0.9) :inherit) 'display '(raise 0.1))
         (propertize date-icon
                     'face `(:height ,(spaceline-all-the-icons--height 1.1) :family ,(all-the-icons-octicon-family) :inherit)
                     'display '(raise 0.1))))

      (propertize " " 'display '(space . (:width 1))))
      'help-echo (format-time-string "%c")
      'mouse-face (spaceline-all-the-icons--highlight)))
  :tight t :enabled t
  :when (or spaceline-all-the-icons-clock-always-visible
            (frame-parameter nil 'fullscreen)))

;; Middle Divider Segments
(spaceline-define-segment all-the-icons-which-function
  "An `all-the-icons' segment to show the `which-function-mode' function"
  (let* ((current (format-mode-line which-func-current))
         (unknown? (string= current which-func-unknown))
         (function-icon (all-the-icons-fileicon "cold-fusion" :v-adjust 0))
         (question-icon (all-the-icons-faicon "question"))

         (text-face `(:family ,(all-the-icons-faicon-family) :inherit))
         (icon-face `(:family ,(all-the-icons-fileicon-family) :inherit)))

    (when (string-match "{\\(.*\\)}" current) (setq current (match-string 1 current)))

    (unless spaceline-all-the-icons-slim-render
      (propertize
       (concat (propertize function-icon 'face icon-face) " "
               (propertize (if unknown? question-icon current)
                           'face (if unknown? text-face `(:height ,(spaceline-all-the-icons--height 0.8) :inherit))
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

;;; Anzu
(defun spaceline-all-the-icons-anzu-update-func (here total)
  "Update function to be set as `anzu-mode-line-update-function'.
Displays HERE and TOTAL to indicate how many search results have been found."
  (let* ((status (cl-case anzu--state
                   (search (format "(%s/%d%s)"
                                   (anzu--format-here-position here total)
                                   total (if anzu--overflow-p "+" "")))
                   (replace (format "(%d/%d)" here total))
                   (replace-query (format "(%d replace)" total))))
         (icon (cl-case anzu--state
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
         (setq icon-face (append `(:family ,(all-the-icons-wicon-family)) icon-face)))

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

(defun spaceline-all-the-icons--temperature-color (info)
  "Convert weather INFO into a color temperature Hex Code.
INFO should be an object similar to `yahoo-weather-info'."
  (let* ((yahoo-weather-use-F nil)
         (celsius (string-to-number (yahoo-weather-info-format info "%(temperature)")))
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
                      :foreground ,(spaceline-all-the-icons--temperature-color yahoo-weather-info)
                      :background ,(spaceline-all-the-icons--face-background 'powerline-active2)))
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

    (when (= 1 (length icon)) (setq icon-face (append `(:family ,(all-the-icons-wicon-family)) icon-face)))

    (propertize icon
                'face icon-face
                'display '(raise 0.1)
                'help-echo help-echo
                'mouse-face (spaceline-all-the-icons--highlight)))
  :tight t
  :when (and active
             (bound-and-true-p yahoo-weather-mode)
             (bound-and-true-p yahoo-weather-info)))

(spaceline-define-segment all-the-icons-minor-modes
  "An `all-the-icons' segment to display minor modes, prefering to use the diminished values."
  (reduce
   (lambda (acc minor-mode)
     (let* ((lighter  (mapconcat 'identity (split-string (format-mode-line (cadr minor-mode))) " "))
            (display? (and (not (string= "" lighter))
                           (boundp (car minor-mode))
                           (symbol-value (car minor-mode))))
            (display (or (get-text-property 0 'display lighter) '(raise 0.1)))
            (face (append (get-text-property 0 'face lighter) '(:height 0.9 :inherit)))
            (lighter (propertize
                      lighter
                      'face face
                      'display display
                      'mouse-face (spaceline-all-the-icons--highlight)
                      'help-echo (concat (symbol-name (car minor-mode))
                                         "\nmouse-1: Display minor mode menu"
                                         "\nmouse-2: Show help for minor mode"
                                         "\nmouse-3: Toggle minor mode")
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line down-mouse-1]
                                     (powerline-mouse 'minor 'menu lighter))
                                   (define-key map [mode-line mouse-2]
                                     (powerline-mouse 'minor 'help lighter))
                                   (define-key map [mode-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu lighter))
                                   (define-key map [header-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu lighter))
                                   map))))
       (append acc (when display? `(,lighter)))))
   minor-mode-alist
   :initial-value '())
  :enabled nil
  :separator (spaceline-all-the-icons--separator spaceline-all-the-icons-primary-separator))

(spaceline-define-segment all-the-icons-nyan-cat
  "Shows the infamous nyan cat.  Requires `nyan-mode' to be enabled."
  (nyan-create)
  :when (and active (bound-and-true-p nyan-mode)))

;; Paradox Segments
(spaceline-define-segment all-the-icons-paradox-line-count
  "An `all-the-icons' segment to display the line number created by `paradox--update-mode-line-front-space'"
  (let* ((total-lines (int-to-string (length tabulated-list-entries)))
         (current-line (format-mode-line (format "%%%sl" (length total-lines)))))
    (format "(%s/%s)" current-line total-lines))
  :when (derived-mode-p 'paradox-menu-mode))

(spaceline-define-segment all-the-icons-paradox-filter
  "An `all-the-icons' segment to display the current `paradox--current-filter'"
  (propertize
   (concat
    (propertize (all-the-icons-faicon "filter")
                'face `(:family ,(all-the-icons-faicon-family) :inherit)
                'display '(raise 0.1))
    (propertize (format " %s" paradox--current-filter) 'display '(raise 0.1)))

   'mouse-face (spaceline-all-the-icons--highlight)
   'local-map (make-mode-line-mouse-map 'mouse-1 'paradox-filter-clear)
   'help-echo (format "Clear current fitler: %s" paradox--current-filter))

  :when (and (derived-mode-p 'paradox-menu-mode)
             paradox--current-filter))

(defmacro define-spaceline-all-the-icons--paradox-segment (type icon mouse &rest body)
  "Macro to declare `spaceline' segment for paradox information of TYPE.
ICON should be an `all-the-icons' icon to display before the segment.
MOUSE should be a cons cell containing a help-echo string and a function
to call on MOUSE-1.
BODY is the form to evaluate to get the text to display."
  `(spaceline-define-segment ,(intern (format "all-the-icons-paradox-status-%s" type))
     ,(format "An `all-the-icons' segment to depict the number of %s packages in `paradox'." type)
     (let* ((text ,@body)
            (text-face `(:foreground ,(spaceline-all-the-icons--face-foreground 'font-lock-keyword-face)
                         :background ,(spaceline-all-the-icons--face-background 'powerline-active1)))
            (icon-face `(:family ,(all-the-icons-icon-family ,icon)
                         :background ,(spaceline-all-the-icons--face-background 'powerline-active1)))
            (num-face (cond
                       ((eq ',type 'new) 'success)
                       ((eq ',type 'upgrade) 'warning)
                       ((eq ',type 'installed) 'spaceline-all-the-icons-info-face))))

       (when (not (zerop text))
         (propertize
          (concat
           (propertize ,icon 'face icon-face)
           (propertize (capitalize ,(format " %s: " type)) 'face text-face 'display '(raise 0.1))
           (propertize (int-to-string text) 'face `((foreground-color . ,(spaceline-all-the-icons--face-foreground num-face))) 'display '(raise 0.1)))
          'help-echo ,(car mouse)
          'local-map (make-mode-line-mouse-map 'mouse-1 ,(cdr mouse))
          'mouse-face (spaceline-all-the-icons--highlight))))

     :when (derived-mode-p 'paradox-menu-mode)))

(define-spaceline-all-the-icons--paradox-segment upgrade
  (all-the-icons-faicon "arrow-circle-up" :v-adjust 0.1)
  ("Mark packages for upgrading" . 'package-menu-mark-upgrades)
  paradox--upgradeable-packages-number)

(define-spaceline-all-the-icons--paradox-segment new
  (all-the-icons-material "new_releases" :v-adjust -0.1) nil
  (cdr (assoc-string "new" paradox--package-count)))

(define-spaceline-all-the-icons--paradox-segment
  installed
  (all-the-icons-octicon "package" :v-adjust 0.1)
  ("Filter to installed packages" . (lambda () (interactive) (package-menu-filter "status:installed")))
  (+ (cdr (assoc-string "installed" paradox--package-count))
     (cdr (assoc-string "dependency" paradox--package-count))
     (cdr (assoc-string "unsigned" paradox--package-count))));

(spaceline-define-segment all-the-icons-paradox-total
  "An `all-the-icons' segment to display the total number of packages found"
  (let ((text-face `(:foreground ,(spaceline-all-the-icons--face-foreground 'font-lock-keyword-face)
                     :background ,(spaceline-all-the-icons--face-background 'powerline-active2))))
    (propertize
     (concat
      (propertize "Total: " 'face text-face 'display '(raise 0.1))
      (propertize (int-to-string (length package-archive-contents)) 'display '(raise 0.1)))
     'mouse-face (spaceline-all-the-icons--highlight)
     'help-echo "Refresh remote package data"
     'local-map (make-mode-line-mouse-map 'mouse-1 'paradox--refresh-remote-data)))

  :when (derived-mode-p 'paradox-menu-mode))

;;; Neotree Segments
(defun spaceline-all-the-icons--neotree-index ()
  "Return the current index in a `NeoTree' buffer."
  (unless (derived-mode-p 'neotree-mode) (error "Not in a NeoTree buffer"))
  (when (neo-buffer--get-filename-current-line)
   (let* ((current (neo-buffer--get-filename-current-line))
          (parent  (file-name-directory current))

          (dirs  (car (neo-buffer--get-nodes parent)))
          (files (cdr (neo-buffer--get-nodes parent)))

          (max   (+ (length dirs) (length files)))
          (index (1+ (if (file-directory-p current)
                         (neo-buffer--get-node-index current dirs)
                       (+ (length dirs)
                          (neo-buffer--get-node-index current files))))))

     (format "[%s/%s]" index max))))

(spaceline-define-segment all-the-icons-neotree-index
  "An `all-the-icons' segment to display your current index within `NeoTree'."
  (spaceline-all-the-icons--neotree-index)
  :when (and (derived-mode-p 'neotree-mode)
             (neo-buffer--get-filename-current-line)))

(defmacro define-spaceline-all-the-icons--neotree-segment (type icon &rest body)
  "Macro to declare `spaceline' segment for NeoTree segment TYPE.
ICON should be an `all-the-icons' icon to display before number.
BODY is the form to evaluate to get the number of things."
  `(prog1
       (defun ,(intern (format "spaceline-all-the-icons--neotree-%s" type)) ()
         ,(format "Return the number of %s in `NeoTree'" type)
         (unless (derived-mode-p 'neotree-mode) (error "Not in a NeoTree buffer"))
         (let* ((icon-family (all-the-icons-icon-family ,icon))

                (current (or (neo-buffer--get-filename-current-line)
                             neo-buffer--start-node))
                (parent  (file-name-directory current))
                (things  (length ,@body)))

           (when (not (zerop things))
             (concat
              (propertize ,icon 'face `(:family ,icon-family :inherit))
              (propertize " " 'face `(:height 0.4 :inherit))
              (format "%s" things)))))
     (spaceline-define-segment ,(intern (format "all-the-icons-neotree-%s" type))
       ,(format "An `all-the-icons' spaceline segment to the number of %s in `neotree'." type)
       (,(intern (format "spaceline-all-the-icons--neotree-%s" type)))
       :tight t
       :when (derived-mode-p 'neotree-mode))))

(define-spaceline-all-the-icons--neotree-segment dirs
  (all-the-icons-faicon "folder-o" :v-adjust -0.1)
  (car (neo-buffer--get-nodes parent)))

(define-spaceline-all-the-icons--neotree-segment files
  (all-the-icons-faicon "files-o" :v-adjust -0.1 :height 0.8)
  (cdr (neo-buffer--get-nodes parent)))

(spaceline-define-segment all-the-icons-neotree-open-bracket
  "An `all-the-icons' segment to open bracket in neotree"
  "("
  :tight t
  :when (and (derived-mode-p 'neotree-mode)
             (or (and spaceline-all-the-icons-neotree-dirs-p (spaceline-all-the-icons--neotree-dirs))
                 (and spaceline-all-the-icons-neotree-files-p (spaceline-all-the-icons--neotree-files)))))

(spaceline-define-segment all-the-icons-neotree-close-bracket
  "An `all-the-icons' segment to close bracket in neotree"
  ")"
  :tight t
  :when (and (derived-mode-p 'neotree-mode)
             (or (and spaceline-all-the-icons-neotree-dirs-p (spaceline-all-the-icons--neotree-dirs))
                 (and spaceline-all-the-icons-neotree-files-p (spaceline-all-the-icons--neotree-files)))))

(spaceline-define-segment all-the-icons-neotree-context
  "An `all-the-icons' segment to display the current context/directory your in in `NeoTree'"
  (let* ((file-name (neo-buffer--get-filename-current-line))
         (current (if file-name file-name neo-buffer--start-node))
         (parent  (if file-name (file-name-directory current) current))

         (index-s (spaceline-all-the-icons--neotree-index))
         (file-s (spaceline-all-the-icons--neotree-files))
         (dirs-s (spaceline-all-the-icons--neotree-dirs))

         (context (file-name-nondirectory (directory-file-name parent)))
         (context-max-length (- (window-width)
                                (length file-s)
                                (length dirs-s)
                                (if index-s (1+ (length index-s)) 0)
                                (when (or file-s dirs-s) 5) 1))

         (context-text (if (<= (length context) context-max-length) context
                         (substring context 0 (- context-max-length 2)))))

    (propertize
     (concat
      (propertize (format "%s "(all-the-icons-faicon "folder-open-o" :v-adjust 0))
                  'face `( :foreground ,(spaceline-all-the-icons--face-background (funcall spaceline-highlight-face-func))
                           :background ,(spaceline-all-the-icons--face-background line-face)
                           :family ,(all-the-icons-faicon-family)))
      (propertize context-text
                  'face `((foreground-color . ,(spaceline-all-the-icons--face-background (funcall spaceline-highlight-face-func)))))
      (unless (<= (length context) context-max-length) (propertize "…" 'face 'font-lock-comment-face)))

     'mouse-face (spaceline-all-the-icons--highlight)
     'help-echo (format "Open `%s' in %s" context (if (file-directory-p parent) "`dired'" "buffer"))
     'local-map (make-mode-line-mouse-map 'mouse-1 `(lambda () (interactive) (find-file ,parent)))))

  :when (derived-mode-p 'neotree-mode))

(provide 'spaceline-all-the-icons-segments)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons-segments.el ends here
