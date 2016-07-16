;;; init.el --- 
;;    
;; Filename: init.el
;; Description: My config for emacs.
;; Author: Stephen Barrett
;; Created: Thu Jul 14 19:00:18 2016 (+0100)
;; Version: 1
;; Package-Requires: ()
;; Last-Updated: Sat Jul 16 16:34:13 2016 (+0100)
;;           By: Stephen Barrett
;;     Update #: 198
;; Keywords: emacs config
;; Compatibility: GNU Emacs: 25.x
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: This is my basic emacs configuration. Feel free to
;; grab anything useful. init.el lives in ~/.emacs.d
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 14-Jul-2016    Stephen Barrett  
;;    Initial pass. Basic support for lisp and haskell execution and
;; some default gui behaviour.  Tuned for use on mac.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some basic gui settings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(tool-bar-mode -1)                    ; no toolbar 
(menu-bar-mode -1)                    ; no menu
(setq inhibit-startup-message t)      ; hide welcome screne
(setq ring-bell-function 'ignore)     ; inhibit bell - it's annoying
(setq echo-keystrokes 0.01)           ; immediate echo
(load-theme 'tango-dark)              ; set tango-dark as theme
(global-hl-line-mode t)               ; highlight the current line
(blink-cursor-mode 0)                 ; stop blinking cursor
(setq-default cursor-type '(bar . 3)) ; set cursor to bar
(when (memq window-system '(mac ns))  ; fix mac mouse scrolling
  (setq mouse-wheel-scroll-amount '(0.01)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global key bindings
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (window-system)
  (progn   ; unbind C-i from tab for later remapping
    (define-key input-decode-map (kbd "C-i") (kbd "H-i"))))  

;; rebind tab key to indent region if active, or default tab
;; behaviour otherwise.
(defun my/tab-replacement (&optional START END)
  (interactive "r")
  (if (use-region-p)                                        
      (indent-region START END)  ; IF active region, use indent-region                                       
    (indent-for-tab-command)))   ; ELSE IF no active region, use default tab command
(global-set-key (kbd "TAB") 'my/tab-replacement)

;; key bindings for window manipulation
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-(") 'delete-otherwindow) ; for progrmmers keyboard
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-)") 'split-window-below) ; for progrmmers keyboard
(global-set-key (kbd "M-3") 'split-window-right) 
(global-set-key (kbd "M-}") 'split-window-right) ; for progrmmers keyboard
(global-set-key (kbd "M-o") 'other-window)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; customer set variable
;; reordering to here because smart-mode-line needs it set fuirst
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (line-number-mode iy-go-to-char expand-region magit dash-at-point highlight-parentheses exec-path-from-shell reveal-in-osx-finder company cus-face color-theme smart-mode-line smex header2 haskell-complete-module auto-compile haskell-mode intero))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#454e51"))))
 '(company-scrollbar-fg ((t (:background "#394143"))))
 '(company-tooltip ((t (:inherit default :background "#454e51"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(font-lock-comment-face ((t (:italic t))))
 '(font-lock-function-name-face ((t (:italic t))))
 '(highlight ((t (:background "grey10" :foreground nil)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add MELPA to package archives.  Also set up use-package so that I
;; can use it to manage subsequent package loads.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list
  'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents ; fetch the list of packages available 
  (package-refresh-contents))
(unless (package-installed-p 'use-package) ; integrate use-package
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package diminish :ensure t)
(use-package bind-key :ensure t)
(use-package package-utils
  :ensure t
  :config (progn
	    (defun package-upgrade-all () ; easier to remember than M-x -upg
	      (interactive)
	      (package-utils-upgrade-all))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode line
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :ensure t
  :config (progn
            (setq sml/theme 'dark)
            (sml/setup)))

(setq column-number-mode t)           ; display cursor position in mode-line

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Last Command
;; The following code displays the last run command on the mode-line.
;; Basic approach is lifted from gnus-notify.el, which adds email
;; message notifications to the mode line.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar display-new-message "")
(defun notify-modeline-form ()
  display-new-message)

(if (featurep 'xemacs)
    (unless (member 'display-new-messages global-mode-string)
      (if (null global-mode-string)
          (setq global-mode-string '("" display-new-messages))
        (setq global-mode-string
              (append global-mode-string
                      '(display-new-messages)))))
  (unless (member '(:eval (notify-modeline-form)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  (list '(:eval (notify-modeline-form)))))))

(defadvice call-interactively (after show-last-command activate)
  "Shows the interactive command that was just run in the message area."
  (unless (eq major-mode 'minibuffer-inactive-mode)
    (setq display-new-message (format "[cmd: %s]" this-command))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company - completion mode
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'global-company-mode)
    (setq company-idle-delay 0)    ; bring company up immediately
    
    (use-package color
      :ensure t
      :functions color-lighten-name
      :config
      (progn
	(let ((bg (face-attribute 'default :background)))
	  (custom-set-faces
	   `(company-tooltip
	     ((t (:inherit default :background, (color-lighten-name bg 10)))))
	   `(company-scrollbar-bg
	     ((t (:background, (color-lighten-name bg 10)))))
	   `(company-scrollbar-fg
	     ((t (:background, (color-lighten-name bg 5)))))
	   `(company-tooltip-selection
	     ((t (:inherit font-lock-function-name-face))))
	   `(company-tooltip-common
	     ((t (:inherit font-lock-constant-face))))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-directory "~/")       ; default directory
(setq backup-directory-alist        ; backups to single directory
      '(("." . "~/MyEmacsBackups")))
(setq insert-directory-program      ; use gls for dired 
      (executable-find "gls"))
(when (memq window-system '(mac ns)) ; enable finder open on mac
  (use-package reveal-in-osx-finder  
    :ensure t
    :bind (("C-c r f" . reveal-in-osx-finder))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :ensure t
    :config (progn 	      
              (setq exec-path-from-shell-check-startup-files nil)
              (exec-path-from-shell-initialize))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smex - M-x enhancement
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ("C-c x x" . execute-extended-command)
         ("C-c s u" . smex-show-unbound-commands))) ; old M-X

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File headers with header2
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package header2
  :ensure t
  :config (progn
            (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
            (add-hook 'c-mode-common-hook   'auto-make-header)
            (add-hook 'haskell-mode-hook    'auto-make-header)           
            (add-hook 'write-file-hooks 'auto-update-file-header))  
  :bind (("C-h C-h" . make-header)
         ("C-h C-r" . make-revision)
         ("C-h C-b" . make-box-comment)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General text editing stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package linum                             ; line numbering globally
	     :ensure t
	     :config
	     (progn 
	       (setq linum-format "%3d\u2502") ; pretify format
	       (global-linum-mode 1)))

(use-package highlight-parentheses   ; Parenthesis highlighting globally
  :ensure t
  :diminish highlight-parentheses-mode
  :config
  (progn 
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
    (global-highlight-parentheses-mode)
    ; make paren highlight update after stuff like paredit changes
    (add-to-list
     'after-change-functions
     '(lambda (&rest x) (hl-paren-highlight)))))

(setq comment-auto-fill-only-comments t)        ; auto fill comment lines
(setq-default auto-fill-function 'do-auto-fill)

(setq indent-tabs-mode nil)                     ; use spaces, not <tab>

(use-package expand-region                ; structured region expansion
  :ensure t  
  :bind (("C-," . er/expand-region)       ; C-, and C-. for expand/contract
	 ("C-." . er/contract-region))
  :config (progn                          ; delete region text on typing
            (delete-selection-mode 1))) 
                                        
(use-package iy-go-to-char               ; see emacs rocks episode 4
  :ensure t                              ; https://github.com/doitian/iy-go-to-char
  :bind (("M-]" . iy-go-to-char)
         ("M-[" . iy-go-to-char-backward)))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font selection
;; ordered for clarity of symbols for haskell programming. the list of
;; font-preferences is reduced to those fonts installed, and then
;; the first available font is selected, if any are available.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl-lib :ensure t)
(defun font-existsp (font)
  "Check to see if the named FONT is available."
  (if (null (x-list-fonts font))
      nil t))
(defun font-avail (fonts)
  "Finds the available fonts."
  (cl-remove-if-not 'font-existsp fonts))
(defvar font-preferences
  '( "Fira Code"
     "PragmataPro"
     "Inconsolata"
     "DejaVu Sans Mono"
     "Bitstream Vera Sans Mono"
     "Anonymous Pro"
     "Menlo"
     "Consolas"))
(unless (eq window-system nil)
  (let ((fonts (font-avail font-preferences)))
    (unless (null fonts)
      (set-face-attribute
       'default nil :font
       (car fonts)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell Stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package haskell-mode
  :ensure t 
  :defines haskell-mode-map
  :functions haskell-complete-module-read
  :bind* ( :map haskell-mode-map
	       ("H-i" . haskell-fast-add-import) ; interactive import
	       ("C-r" . haskell-move-right)      ; nested right indent
	       ("C-l" . haskell-move-left))      ; nested left indent
  :config
  (progn
    (setq haskell-font-lock-symbols 'unicode)
    (setq haskell-stylish-on-save t)

    ;; code for nice nested indent
    (defun haskell-move-right ()  
      (interactive)
      (haskell-move-nested 1))	
    (defun haskell-move-left ()
      (interactive)
      (haskell-move-nested -1))
    
    ;; code to add import, taken from chris done's haskell.el config
    (defun haskell-capitalize-module (m)
      ;; FIXME:
      (with-temp-buffer
	(insert m)
	(upcase-initials-region (point-min) (point-max))
	(buffer-string)))
    (defvar haskell-fast-module-list
      (list)
      "A list of modules.")
    (defun haskell-fast-modules-save ()
      (interactive)
      (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
	(erase-buffer)
	(insert (format "%S" haskell-fast-module-list))
	(basic-save-buffer)
	(bury-buffer)))
    (defun haskell-fast-modules-load ()
      (interactive)
      (with-current-buffer (find-file-noselect "~/.emacs.d/.haskell-modules.el")
	(setq haskell-fast-module-list (read (buffer-string)))
	(bury-buffer)))
    (defun haskell-fast-get-import (custom)
      (if custom
	  (let* ((module (haskell-capitalize-module (read-from-minibuffer "Module: " ""))))
	    (unless (member module haskell-fast-module-list)
	      (add-to-list 'haskell-fast-module-list module))
	    module)
	(let ((module (haskell-capitalize-module
		       (haskell-complete-module-read
			"Module: "
			(append (mapcar #'car haskell-import-mapping)
				haskell-fast-module-list)))))
	  (unless (member module haskell-fast-module-list)
	    (add-to-list 'haskell-fast-module-list module)
	    (haskell-fast-modules-save))
	  module)))
    (defun haskell-fast-add-import (custom)
      "Add an import to the import list.  Sorts and aligns imports,
       unless `haskell-stylish-on-save' is set, in which case we defer
       to stylish-haskell."
      (interactive "P")
      (save-excursion
	(goto-char (point-max))
	(haskell-navigate-imports)
	(let* ((chosen (haskell-fast-get-import custom))
	       (module (let ((mapping (assoc chosen haskell-import-mapping)))
			 (if mapping
			     (cdr mapping)
			   (concat "import " chosen "\n")))))
	  (insert module))
	(haskell-sort-imports)
	(haskell-align-imports)))

    ;; load up interp
    (use-package intero
      :ensure t
      :config
      (progn 
        (add-hook 'haskell-mode-hook 'intero-mode)))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dash API Document Browser
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash-at-point
  :ensure t
  :config (progn
            (add-hook 'haskell-mode-hook
                      (lambda () (setq dash-at-point-docset "haskell")))
            (add-hook 'lisp-mode-hook
                      (lambda () (setq dash-at-point-docset "lisp"))))
  :bind (("C-c d" . dash-at-point)
         ("C-c e" . dash-at-point-with-docset)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Control, ie. Git
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit                 ; Magit for git integration
	     :ensure t
	     :bind (("C-c v c" . magit-clone)
		    ("C-c v v" . magit-status)
		    ("C-c v g" . magit-blame)
		    ("C-c v l" . magit-log-buffer-file)
		    ("C-c v p" . magit-pull))
	     :config
	     (progn
	       (setq magit-save-repository-buffers 'dontask)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lisp Stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-compile     ; automatically recompile compiled lisp 
  :ensure t                   ; files on save.
  :config (progn
            (setq load-prefer-newer t)
            (auto-compile-on-load-mode)
            (auto-compile-on-save-mode)))

(use-package lisp-mode :bind (("C-c C-c" . eval-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
