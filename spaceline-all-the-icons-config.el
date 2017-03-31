;;; spaceline-all-the-icons-config.el --- A Spaceline theme using All The Icons

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
(require 'spaceline-all-the-icons)
(require 'spaceline-all-the-icons-separators)

;;; Full Modeline Definition
(spaceline-compile
 "all-the-icons"
 '(((all-the-icons-modified
     all-the-icons-bookmark
     all-the-icons-dedicated
     all-the-icons-window-number
     all-the-icons-buffer-size) :face highlight-face :skip-alternate t)

   all-the-icons-separator-left-active-1

   ((all-the-icons-projectile
     all-the-icons-mode-icon
     all-the-icons-buffer-id)
    :face default-face)

   all-the-icons-separator-left-active-2

   ((all-the-icons-process
     all-the-icons-position
     all-the-icons-region-info
     all-the-icons-fullscreen
     all-the-icons-text-scale)
    :separator (spaceline-all-the-icons--separator "|" " ") :face highlight-face)

   all-the-icons-separator-left-active-3
   all-the-icons-separator-left-inactive

   ((all-the-icons-vc-icon
     all-the-icons-vc-status
     all-the-icons-git-status
     ((all-the-icons-flycheck-status
       all-the-icons-flycheck-status-info) :separator " ")
     all-the-icons-package-updates)
    :separator (spaceline-all-the-icons--separator "·" " ") :face other-face)

   all-the-icons-separator-left-active-4)

 '(all-the-icons-separator-right-active-1
   ((all-the-icons-hud
     all-the-icons-buffer-position)
    :separator " " :when active)

   all-the-icons-separator-right-active-2
   all-the-icons-separator-right-inactive

   ((all-the-icons-battery-status
     all-the-icons-time "")
    :separator (spaceline-all-the-icons--separator "|" " ") :face default-face)))

;; Interactive Functions
(defun spaceline-all-the-icons-theme ()
  "Set `mode-line-format' to be `spaceline-ml-all-the-icons'."
  (interactive)
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-all-the-icons)))))

(defconst spaceline-all-the-icons-theme '("%e" (:eval (spaceline-ml-all-the-icons)))
  "Constant version of `spaceline-all-the-icons-theme' to allow to be set manually.")

(defun spaceline-all-the-icons-toggle-slim ()
  "Wrapper to toggle `spaceline-all-the-icons-slim-render' setting."
  (interactive)
  (setq spaceline-all-the-icons-slim-render
        (not spaceline-all-the-icons-slim-render)))

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; spaceline-all-the-icons-config.el ends here
