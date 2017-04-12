;;; spaceline-all-the-icons.el --- A Spaceline theme using All The Icons

;; Copyright (C) 2017  Dominic Charlesworth <dgc336@gmail.com>

;; Author: Dominic Charlesworth <dgc336@gmail.com>
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "24.4") (spaceline "2.0.0"))
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

;; This package is a theme for `spaceline' and recreates most of the
;; segments available in that package using icons from
;; `all-the-icons'.  Icon fonts allow for more tailored and detailed
;; information in the mode line.

;; Currently this package provides segmets for the following functions
;; without the need for optional dependencies

;; - `modified'          Whether or not the current buffer has been modified
;; - `dedicated'         Whether or not the current buffer is dedicated
;; - `buffer-path'       The Path of the current buffer
;; - `buffer-id'         The id of the current buffer
;; - `buffer-size'       The size of the buffer
;; - `mode-icon'         The Major Mode displayed as an icon
;; - `process'           The currently running process
;; - `position'          The Line/Column current position
;; - `region-info'       Count of lines and words currently in region
;; - `fullscreen'        An indicator of whether or not window is fullscreen
;; - `text-scale'        The amount of global text scale
;; - `vc-icon'           The current Version Control Icon
;; - `vc-status'         The VC status, e.g. branch or revision
;; - `git-ahead'         The number of commits ahead of upstream
;; - `package-updates'   The number of packages available for update
;; - `hud'               A widget displaying how far through the buffer you are
;; - `buffer-position'   A percentage or word describing buffer position
;; - `time'              The Current Time with icon

;; There are also some segments that require optional dependencies,
;; this is a list of them and their required packages.

;; - `bookmark' [`bookmark']                           Whether or not the current buffer has been modified
;; - `window-number' [`winum' or `window-numbering']   The current window number
;; - `projectile' [`projectile']                       The current project you're working in
;; - `git-status' [`git-gutter']                       Number of added/removed lines in current buffer
;; - `flycheck-status' [`flycheck']                    A summary of Errors/Warnings/Info in buffer
;; - `flycheck-status-info' [`flycheck']               A summary dedicated to Info statuses in buffer
;; - `which-function' [`which-function']               Display the name of function your point is in
;; - `weather' [`yahoo-weather']                       Display an icon of the current weather
;; - `temperature' [`yahoo-weather']                   Display the current temperature with a coloured thermometer
;; - `sunrise' [`yahoo-weather']                       Display an icon to show todays sunrise time
;; - `sunset' [`yahoo-weather']                        Display an icon to show todays sunset time
;; - `battery-status' [`fancy-battery']                Display a colour coded battery with time remaining

;;; Code:

(require 'spaceline)

(require 'spaceline-all-the-icons-segments)
(require 'spaceline-all-the-icons-separators)

;; Declare Customization Groups
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

(defun spaceline-all-the-icons--height (&optional height)
  "Scale `powerline-height' by HEIGHT."
  (if (bound-and-true-p powerline-height)
      (* (or height 1) (or powerline-height 1))
      (or height 1)))

;;; Full Modeline Definition
(spaceline-compile
 "all-the-icons"
 '((all-the-icons-anzu
    :face mode-line
    :skip-alternate t)

   ((all-the-icons-modified
     all-the-icons-bookmark
     all-the-icons-dedicated
     all-the-icons-window-number
     all-the-icons-buffer-size) :face highlight-face :skip-alternate t)

   all-the-icons-separator-left-active-1

   ((all-the-icons-projectile
     all-the-icons-mode-icon
     ((all-the-icons-buffer-path
       all-the-icons-buffer-id) :separator ""))
    :face default-face)

   all-the-icons-separator-left-active-2

   ((all-the-icons-process
     all-the-icons-position
     all-the-icons-region-info
     all-the-icons-fullscreen
     all-the-icons-text-scale)
    :face highlight-face
    :separator (spaceline-all-the-icons--separator "|" " "))

   all-the-icons-separator-left-active-3
   all-the-icons-separator-left-inactive

   ((all-the-icons-vc-icon
     all-the-icons-vc-status
     ((all-the-icons-git-ahead
       all-the-icons-git-status) :separator " ")
     ((all-the-icons-flycheck-status
       all-the-icons-flycheck-status-info) :separator " ")
     all-the-icons-package-updates)
    :face other-face
    :separator (spaceline-all-the-icons--separator "·" " "))

   all-the-icons-separator-left-active-4

   ((all-the-icons-which-function)
    :face powerline-active2
    :separator ""))

 '(
   ((all-the-icons-weather
     all-the-icons-temperature
     all-the-icons-sunrise
     all-the-icons-sunset)
    :face powerline-active2
    :separator (spaceline-all-the-icons--separator "·" " "))

   ((all-the-icons-player-volume
     all-the-icons-player-controls
     all-the-icons-track
     all-the-icons-player-controls-shuffle)
    :face powerline-active2)

   all-the-icons-separator-right-active-1
   ((all-the-icons-hud
     all-the-icons-buffer-position)
    :separator " " :when active)

   all-the-icons-separator-right-active-2
   all-the-icons-separator-right-inactive

   ((all-the-icons-battery-status
     all-the-icons-time)
    :separator (spaceline-all-the-icons--separator "|" " ") :face default-face)))

;; Interactive Functions
(defun spaceline-all-the-icons-theme ()
  "Set `mode-line-format' to be `spaceline-ml-all-the-icons'."
  (interactive)
  (setq-default mode-line-format spaceline-all-the-icons-theme))

(defconst spaceline-all-the-icons-theme '("%e" (:eval (spaceline-ml-all-the-icons)))
  "Constant version of variable `spaceline-all-the-icons-theme' to allow to be set manually.")

(defun spaceline-all-the-icons-toggle-slim ()
  "Wrapper to toggle `spaceline-all-the-icons-slim-render' setting."
  (interactive)
  (setq spaceline-all-the-icons-slim-render
        (not spaceline-all-the-icons-slim-render)))

(defun spaceline-all-the-icons--setup-anzu ()
  "Setup function for enabling command `anzu-mode' in `spaceline-all-the-icons' theme."
  (setq-default anzu-mode-line-update-function 'spaceline-all-the-icons-anzu-update-func))

(defun spaceline-all-the-icons--setup-package-updates ()
  "Set up advice in order to count package upgrades."
  (spaceline-all-the-icons--count-package-updates)
  (advice-add 'package-menu-execute :after 'spaceline-all-the-icons--count-package-updates)
  (advice-add 'package-refresh-contents :after 'spaceline-all-the-icons--count-package-updates))
;; Debugging functions
(defun spaceline-all-the-icons--turn-off (segment) "Turn spaceline SEGMENT off." (funcall (intern (format "spaceline-toggle-all-the-icons-%s-off" segment))))
(defun spaceline-all-the-icons--turn-on (segment) "Turn spaceline SEGMENT on." (funcall (intern (format "spaceline-toggle-all-the-icons-%s-on" segment))))
(defun spaceline-all-the-icons--get-active-segments ()
  "Get a list of all currently active segment names."
  (let* ((segments (apropos-internal "^spaceline-all-the-icons-.*-p$"))
         (active-segments (cl-remove-if-not (lambda (s) (and (boundp s) (symbol-value s))) segments)))
    (mapcar
     (lambda (segment) (prog2
                      (string-match "^spaceline-all-the-icons-\\(.*?\\)-p$" (format "%s" segment))
                      (match-string 1 (format "%s" segment)))) active-segments)))

(defun spaceline-all-the-icons--debug-segments (&optional pfx)
  "Programatically toggle active segments and report any that throw errors.
When PFX is non-nil, disable erroring segments at the same time."
  (interactive "P")
  (let* ((active-segments (spaceline-all-the-icons--get-active-segments))
         (errors (cl-remove-if-not
                  (lambda (segment)
                    (mapc 'spaceline-all-the-icons--turn-off active-segments)
                    (spaceline-all-the-icons--turn-on segment)
                    (string= "" (format-mode-line spaceline-all-the-icons-theme)))
                  active-segments)))
    (mapc 'spaceline-all-the-icons--turn-on active-segments)
    (if (not errors)
        (message "%s Spaceline is working fine!" (all-the-icons-wicon "stars"))
      (when pfx (mapc 'spaceline-all-the-icons--turn-off errors))
      (error "%s Errors found in Spaceline Segments: [%s]"
             (all-the-icons-faicon "fire-extinguisher")
             (mapconcat 'identity errors ", ")))))

(provide 'spaceline-all-the-icons)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons.el ends here
