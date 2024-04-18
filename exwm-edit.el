;;; exwm-edit.el --- Edit mode for EXWM -*- lexical-binding: t; -*-

;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/exwm-edit
;; Created: 2018-05-16
;; Keywords: convenience
;; License: GPL v3
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.4-pre

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Similar to atomic-chrome https://github.com/alpha22jp/atomic-chrome
;; except this package is made to work with EXWM https://github.com/ch11ng/exwm
;; and it works with any editable element of any app
;;
;; The idea is very simple - when you press the keybinding,
;; it simulates [C-a (select all) + C-c (copy)],
;; then opens a buffer and yanks (pastes) the content so you can edit it,
;; after you done - it grabs (now edited text) and pastes back to where it's started
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'exwm)

(defvar exwm-edit--last-window-configuration nil
  "Window configuration before popping to \"exwm-edit\" buffer.")

(defvar exwm-edit-last-kill nil
  "Used to check if the text box is empty.
If this is the same value as (car KILL-RING) returns after copying the text-box,
the text box might be empty (because empty text boxes don't add to the KILL-RING).")

(defvar exwm-edit-yank-delay 0.3
  "The delay to use when yanking into the Emacs buffer.
It takes a while for copy in exwm to transfer to Emacs yank.
If this is too low an old yank may be used instead.")

(defvar exwm-edit-paste-delay 0.05
  "The delay to use when pasting text back into the exwm buffer.
If this is too low the text might not be pasted into the exwm buffer")

(defvar exwm-edit-clean-kill-ring-delay 0.10
  "The delay to clean the `kill-ring' after pasting the text back 
to the exwm-buffer.")

(defgroup exwm-edit nil
  "Edit mode for EXWM"
  :group 'applications
  :prefix "exwm-edit-")

(defcustom exwm-edit-display-buffer-action '(display-buffer-pop-up-window)
  "Display buffer action for \"*exwm-edit*\" buffers.
Passed to `display-buffer', which see."
  :type display-buffer--action-custom-type)

(defcustom exwm-edit-copy-over-contents t
  "If non-nil, copy over the contents of the exwm text box. 
This is then inserted into the `exwm-edit' buffer."
  :type 'boolean)

(defcustom exwm-edit-compose-hook nil
  "Customizable hook, runs after `exwm-edit--compose' buffer created."
  :type 'hook)

(defcustom exwm-edit-compose-minibuffer-hook nil
  "Customizable hook, runs after `exwm-edit--compose-minibuffer' buffer created."
  :type 'hook)

(defcustom exwm-edit-before-finish-hook nil
  "Customizable hook, runs before `exwm-edit--finish'."
  :type 'hook)

(defcustom exwm-edit-before-cancel-hook nil
  "Customizable hook, runs before `exwm-edit--cancel'."
  :type 'hook)

(defun exwm-edit--finish ()
  "Called when done editing buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-finish-hook)
  (let ((text (filter-buffer-substring (point-min) (point-max))))
    (kill-buffer)
    (exwm-edit--send-to-exwm-buffer text)))

(defun exwm-edit--send-to-exwm-buffer (text)
  "Sends TEXT to the exwm window."
  (kill-new text)
  (set-window-configuration exwm-edit--last-window-configuration)
  (setq exwm-edit--last-window-configuration nil)
  (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
  (if (string= text "")
      ;; If everything is deleted in the exwm-edit buffer, then simply delete the selected text in the exwm buffer
      (run-with-timer exwm-edit-paste-delay nil (lambda () (exwm-input--fake-key 'delete)))

    (run-with-timer exwm-edit-paste-delay nil (lambda ()
						(exwm-input--fake-key ?\C-v)
						;; Clean up the kill ring
						;; It needs to be run on a timer because of some reason
						(run-with-timer exwm-edit-clean-kill-ring-delay nil (lambda ()
												      (pop kill-ring)
												      ;; Kill-ring weirdness
												      (if kill-ring
													  (kill-new (car kill-ring))
													(kill-new ""))))))))

(defun exwm-edit--cancel ()
  "Called to cancel editing in a buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-cancel-hook)
  (kill-buffer)
  (set-window-configuration exwm-edit--last-window-configuration)
  (setq exwm-edit--last-window-configuration nil)
  (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
  (exwm-input--fake-key 'right)
  (when kill-ring
    (kill-new (car kill-ring))))

(defvar exwm-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c '") 'exwm-edit--finish)
    (define-key map (kbd "C-c C-'") 'exwm-edit--finish)
    (define-key map (kbd "C-c C-c") 'exwm-edit--finish)
    (define-key map [remap save-buffer] 'exwm-edit--finish)
    (define-key map (kbd "C-c C-k") 'exwm-edit--cancel)
    map)
  "Keymap for minor mode `exwm-edit-mode'.")

(define-minor-mode exwm-edit-mode
  "Minor mode enabled in `exwm-edit--compose' buffer"
  :init-value nil
  :interactive nil
  :lighter " exwm-edit"
  :keymap exwm-edit-mode-map
  (if exwm-edit-mode
      ;; Re-enable the minor mode when changing major mode so the
      ;; major mode's keybindings don't shadow `exwm-edit-mode-map'.
      (add-hook 'after-change-major-mode-hook
                #'exwm-edit-mode nil 'local)
    (remove-hook 'after-change-major-mode-hook
                 #'exwm-edit-mode 'local)))
;; Putting permanent-local means that switching major-mode doesn't
;; reset the variable `exwm-edit-mode' to nil.
(put 'exwm-edit-mode 'permanent-local t)

(defun exwm-edit--buffer-title (str)
  "`exwm-edit' buffer title based on STR."
  (format "*exwm-edit %s *" str))

(defun exwm-edit--yank ()
  "Yank text to Emacs buffer with check for empty strings."
  (run-with-timer exwm-edit-yank-delay nil
		  (lambda ()
		    (let* ((clip-raw (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
			   (clip (when clip-raw (substring-no-properties clip-raw))))
		      (when clip
			(unless (and exwm-edit-last-kill (string= exwm-edit-last-kill clip))
			  (insert clip)))))))

;;;###autoload
(defun exwm-edit--compose (&optional no-copy)
  "Edit text in an EXWM app.
If NO-COPY is non-nil, don't copy over the contents of the exwm text box"
  (interactive)
  (let* ((title (exwm-edit--buffer-title (buffer-name)))
         (existing (get-buffer title))
         (inhibit-read-only t)
         (save-interprogram-paste-before-kill t)
         (selection-coding-system 'utf-8))             ; required for multilang-support
    (when (derived-mode-p 'exwm-mode)
      (setq exwm-edit--last-window-configuration (current-window-configuration))
      (if existing
          (switch-to-buffer-other-window existing)
        (exwm-input--fake-key ?\C-a)
        (unless (or no-copy (not exwm-edit-copy-over-contents))
	  (when (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
	    (setq exwm-edit-last-kill (substring-no-properties (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
	  (exwm-input--fake-key ?\C-c))
        (with-current-buffer (get-buffer-create title)
          (run-hooks 'exwm-edit-compose-hook)
          (exwm-edit-mode 1)
          (pop-to-buffer (current-buffer) exwm-edit-display-buffer-action)
          (setq-local header-line-format
                      (substitute-command-keys
                       "Edit, then exit with `\\[exwm-edit--finish]' or cancel with \ `\\[exwm-edit--cancel]'"))
          (unless (or no-copy (not exwm-edit-copy-over-contents))
	    (exwm-edit--yank)))))))

;;;###autoload
(defun exwm-edit--compose-minibuffer (&optional completing-read-entries no-copy)
  "Edit text in an EXWM app.
If COMPLETING-READ-ENTRIES is non-nil, feed that list into the collection
parameter of `completing-read'
If NO-COPY is non-nil, don't copy over the contents of the exwm text box"
  (interactive)
  (let* ((title (exwm-edit--buffer-title (buffer-name)))
         (inhibit-read-only t)
         (save-interprogram-paste-before-kill t)
         (selection-coding-system 'utf-8))             ; required for multilang-support
    (when (derived-mode-p 'exwm-mode)
      (setq exwm-edit--last-window-configuration (current-window-configuration))
      (exwm-input--fake-key ?\C-a)
      (unless (or no-copy (not exwm-edit-copy-over-contents))
	(when (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
	  (setq exwm-edit-last-kill (substring-no-properties (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
	(exwm-input--fake-key ?\C-c)
	(exwm-edit--yank))
      (run-hooks 'exwm-edit-compose-minibuffer-hook)
      (exwm-edit--send-to-exwm-buffer
       (completing-read "exwm-edit: " completing-read-entries)))))

(provide 'exwm-edit)

;;; exwm-edit.el ends here
