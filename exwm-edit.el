;;; exwm-edit.el --- Edit mode for EXWM -*- lexical-binding: t; -*-

;; Author: Ag Ibragimov
;; URL: https://github.com/agzam/exwm-edit
;; Created: 2018-05-16
;; Keywords: convenience
;; License: GPL v3
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.0.2

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

(defvar exwm-edit--last-exwm-buffer nil
  "Last buffer that invoked `exwm-edit'.")

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

(defcustom exwm-edit-split nil
  "If non-nil `exwm-edit--compose' splits the window.
possible values right/below/nil/t."
  :type 'string
  :group 'exwm-edit)

(defcustom exwm-edit-copy-over-contents t
  "If non-nil, copy over the contents of the exwm text box. 
This is then inserted into the `exwm-edit' buffer."
  :type 'boolean
  :group 'exwm-edit)

(defcustom exwm-edit-bind-default-keys t
  "If non-nil bind default keymaps on load."
  :type 'boolean
  :group 'exwm-edit)

(defcustom exwm-edit-compose-hook nil
  "Customizable hook, runs after `exwm-edit--compose' buffer created."
  :type 'hook
  :group 'exwm-edit)

(defcustom exwm-edit-compose-minibuffer-hook nil
  "Customizable hook, runs after `exwm-edit--compose-minibuffer' buffer created."
  :type 'hook
  :group 'exwm-edit)

(defcustom exwm-edit-before-finish-hook nil
  "Customizable hook, runs before `exwm-edit--finish'."
  :type 'hook
  :group 'exwm-edit)

(defcustom exwm-edit-before-cancel-hook nil
  "Customizable hook, runs before `exwm-edit--cancel'."
  :type 'hook
  :group 'exwm-edit)

(defun exwm-edit--switch ()
  "Restore buffer/window layout after leaving the edit buffer.
Depending on `exwm-edit-split' and amount of visible windows on the screen."
  (funcall
   (if (or (one-window-p) exwm-edit-split) 'switch-to-buffer
     'switch-to-buffer-other-window)
   exwm-edit--last-exwm-buffer))

(defun exwm-edit--finish ()
  "Called when done editing buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-finish-hook)
  (let ((text (buffer-substring-no-properties
	       (point-min)
	       (point-max)))
	(current-buffer (buffer-name)))
    (when exwm-edit-split
      (kill-buffer-and-window))
    (exwm-edit--send-to-exwm-buffer text)
    (unless exwm-edit-split (kill-buffer current-buffer))))

(defun exwm-edit--send-to-exwm-buffer (text)
  "Sends TEXT to the exwm window."
  (kill-new text)
  (exwm-edit--switch)
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
													(kill-new "")))))))
  (setq exwm-edit--last-exwm-buffer nil))

(defun exwm-edit--cancel ()
  "Called to cancell editing in a buffer created by `exwm-edit--compose'."
  (interactive)
  (run-hooks 'exwm-edit-before-cancel-hook)
  (when exwm-edit-split
    (kill-buffer-and-window))
  (let* ((current-buffer (buffer-name)))
    (exwm-edit--switch)
    (exwm-input--set-focus (exwm--buffer->id (window-buffer (selected-window))))
    (exwm-input--fake-key 'right)
    (unless exwm-edit-split (kill-buffer current-buffer)))
  (setq exwm-edit--last-exwm-buffer nil)
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
  :lighter " exwm-edit"
  :keymap exwm-edit-mode-map)

(defun exwm-edit--buffer-title (str)
  "`exwm-edit' buffer title based on STR."
  (concat "*exwm-edit " str " *"))

(defun exwm-edit--turn-on-edit-mode ()
  "Turn on `exwm-edit-mode' if the buffer was created by `exwm-edit--compose'."
  (when (string= (exwm-edit--buffer-title exwm-edit--last-exwm-buffer)
                 (buffer-name (current-buffer)))
    (exwm-edit-mode t)))

;; global-minor-mode is to re-activate exwm-mode-map when major mode of
;; exwm-edit buffer gets changed manually.
(define-global-minor-mode global-exwm-edit-mode
  exwm-edit-mode exwm-edit--turn-on-edit-mode
  :require 'exwm-edit)

(defun exwm-edit--yank ()
  "Yank text to Emacs buffer with check for empty strings."
  (run-with-timer exwm-edit-yank-delay nil
		  (lambda ()
		    (let* ((clip-raw (gui-get-selection 'CLIPBOARD 'UTF8_STRING))
			   (clip (when clip-raw (substring-no-properties clip-raw))))
		      (when clip
			(unless (and exwm-edit-last-kill (string= exwm-edit-last-kill clip))
			  (insert clip)))))))

(defun exwm-edit--display-buffer (buffer)
  "Display BUFFER according to user settings."
  (select-window
   (pcase exwm-edit-split
     ((or 't "right") (split-window-right))
     ("below" (split-window-below))
     (_ (next-window))))
  (switch-to-buffer buffer))

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
      (setq exwm-edit--last-exwm-buffer (buffer-name))
      (unless (bound-and-true-p global-exwm-edit-mode)
        (global-exwm-edit-mode 1))
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
          (exwm-edit--display-buffer (current-buffer))
          (setq-local header-line-format
                      (substitute-command-keys
                       "Edit, then exit with `\\[exwm-edit--finish]' or cancel with \ `\\[exwm-edit--cancel]'"))
          (unless (or no-copy (not exwm-edit-copy-over-contents))
	    (exwm-edit--yank)))))))

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
      (setq exwm-edit--last-exwm-buffer (buffer-name))
      (unless (bound-and-true-p global-exwm-edit-mode)
        (global-exwm-edit-mode 1))
      (progn
        (exwm-input--fake-key ?\C-a)
	(unless (or no-copy (not exwm-edit-copy-over-contents))
	  (when (gui-get-selection 'CLIPBOARD 'UTF8_STRING)
	    (setq exwm-edit-last-kill (substring-no-properties (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
	  (exwm-input--fake-key ?\C-c)
	  (exwm-edit--yank))
	(run-hooks 'exwm-edit-compose-minibuffer-hook)
	(exwm-edit--send-to-exwm-buffer
	 (completing-read "exwm-edit: " completing-read-entries))))))

(when exwm-edit-bind-default-keys
  (exwm-input-set-key (kbd "C-c '") #'exwm-edit--compose)
  (exwm-input-set-key (kbd "C-c C-'") #'exwm-edit--compose))

(provide 'exwm-edit)

;;; exwm-edit.el ends here
