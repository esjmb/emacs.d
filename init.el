;;; init.el --- 
;; 
;; Filename: init.el
;; Description: My config for emacs.
;; Author: Stephen Barrett
;; Created: Thu Jul 14 19:00:18 2016 (+0100)
;; Version: 1
;; Package-Requires: ()
;; Last-Updated: Mon Aug  8 15:15:38 2016 (+0100)
;;           By: Stephen Barrett
;;     Update #: 812
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
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 6) ((control) . nil)) ;
        mouse-wheel-progressive-speed nil
        scroll-error-top-bottom t
        scroll-preserve-screen-position nil
        scroll-margin 10                  
        scroll-conservatively 100000))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set tripple wheel gestures to cycle buffers
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *my-previous-buffer* t
  "can we switch?")

(defun my-previous-buffer ()
  (interactive)
  ;(message "custom prev: *my-previous-buffer*=%s" *my-previous-buffer*)
  (when *my-previous-buffer*
    (previous-buffer)
    (setq *my-previous-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
                               (setq *my-previous-buffer* t)))))

(defvar *my-next-buffer* t
  "can we switch?")

(defun my-next-buffer ()
  (interactive)
  ;(message "custom prev: *my-next-buffer*=%s" *my-next-buffer*)
  (when *my-next-buffer*
    (next-buffer)
    (setq *my-next-buffer* nil)
    (run-at-time "1 sec" nil (lambda ()
                               (setq *my-next-buffer* t)))))

(global-set-key [triple-wheel-right] 'my-previous-buffer)
(global-set-key [triple-wheel-left] 'my-next-buffer)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [wheel-left] 'ignore)
(global-set-key [wheel-right] 'ignore)

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

;; set M-n and M-p to next and previous lines
(global-set-key (kbd "M-n") 'next-line)
(global-set-key (kbd "M-p") 'previous-line)

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
    (fill-column-indicator smooth-scroll smooth-scrolling centered-cursor-mode flycheck magit-popup rich-minority
                           use-package line-number-mode iy-go-to-char expand-region magit dash-at-point
                           highlight-parentheses exec-path-from-shell reveal-in-osx-finder company cus-face
                           color-theme smart-mode-line smex header2 haskell-complete-module auto-compile
                           haskell-mode intero))))
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
 '(font-lock-comment-warning-face ((t (:background "grey10" :foreground nil))))
 '(font-lock-function-name-face ((t (:italic t))))
 '(font-lock-warning-face ((t (:background "grey10" :foreground nil))))
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
;; The following code displays the last run command on the mode-line,
;; along with the keystrokes.
;; Basic approach is lifted from gnus-notify.el, which adds email
;; message notifications to the mode line.
;;
;; TODO: improve formatting of output so that pointless outputs like
;; self-insert-command for normal keystrokes are not presented.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package cl :ensure t)

(defvar display-new-message-sb "")
(defun notify-modeline-form-sb ()
  display-new-message-sb)

(if (featurep 'xemacs)
    (unless (member 'display-new-messages global-mode-string)
      (if (null global-mode-string)
          (setq global-mode-string '("" display-new-messages))
        (setq global-mode-string
              (append global-mode-string
                      '(display-new-messages)))))
  (unless (member '(:eval (notify-modeline-form-sb)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  (list '(:eval (notify-modeline-form-sb)))))))

(defun execute-extended-command--shorter-1 (name length end-time)
    (cond
     ;((not (time-less-p (current-time) end-time)) nil)
     ((zerop length) (list ""))
     ((equal name "") nil)
     (t
      (nconc (mapcar (lambda (s) (concat (substring name 0 1) s))
                     (execute-extended-command--shorter-1
                      (substring name 1) (1- length) end-time))
             (when (string-match "\\`\\(-\\)?[^-]*" name)
               (execute-extended-command--shorter-1
                (substring name (match-end 0)) length end-time))))))

(defun execute-extended-command--shorter-sb (name)
  (let ((candidates '())
        (max (length name))
        (len 1)
        binding
        (end-time (time-add (current-time) (seconds-to-time 1))))
    (while (and (not binding)
                (time-less-p (current-time) end-time) ; timeout fail
                (progn
                  (unless candidates
                    (setq len (1+ len))
                    (setq candidates (execute-extended-command--shorter-1
                                      name len end-time)))
                  ;; Don't show the help message if the binding isn't
                  ;; significantly shorter than the M-x command the user typed.
                  (< len (- max 5))))
      (let ((candidate (pop candidates)))
        (when (equal name
                       (car-safe (completion-try-completion
                                  candidate obarray 'commandp len)))
          (setq binding candidate))))
    binding))

(defvar extended-command-hash (make-hash-table :test 'equal))
(defun generate-extended-command--shorter-sb (name)
  "Simple caching function to prevent recalculation of shortened command form."
  (let ((val (gethash name extended-command-hash)))
    (if (not val) 
        (let ((val (execute-extended-command--shorter-sb name)))
          (puthash name val extended-command-hash)
          val)          
      val)))

(defvar display-new-message-hash (make-hash-table :test 'equal))

(defadvice call-interactively (after show-last-command activate)
  "Shows the interactive command that was just run in the message area."
  (unless (or (eq major-mode 'minibuffer-inactive-mode)
              (not (symbolp real-this-command)))

    (let* ((tc (symbol-name real-this-command))  ; don't use 'this-command' !
           (val (gethash tc display-new-message-hash))) ; cache previous string generations
      (if (bound-and-true-p val)
          (setq display-new-message-sb val)
        ;; TODO: clean up this by creating a variable that end user can add search strings to
        (unless (or (string= "isearch-printing-char" tc)
                    (cl-search "mouse-" tc)         ; do not change bar
                    (cl-search "wheel-" tc) 
                    (cl-search "isearch-repeat" tc)                                
                    (cl-search "company-ignore" tc)
                    (cl-search "company-select" tc)
                    (cl-search "company-complete" tc)
                    (cl-search "ignore" tc))
          
          (if (or (<= (length tc) 4)
                  (string= "self-insert-command" real-this-command))  ; clear if non command pressed
              (setq display-new-message-sb "")
            (when (not (string= real-this-command "nil"))
              (let* ((kd (key-description (this-command-keys)))
                     (seq (lambda (x)
                            (or (string= x kd)
                                (cl-search "<me" x)
                                (cl-search "wheel-" x))))
                     
                     (kda (mapcar 'key-description
                                  (where-is-internal real-this-command overriding-local-map nil)))
                     (mem (member kd kda)) ;  
                     (kdas (mapconcat 'identity (cl-remove-if seq kda) ", "))
                     (str (format "%s%s" kdas   ; append an M-x shortened version of command             
                                  (if (not mem) 
                                      (let ((s (generate-extended-command--shorter-sb tc)))
                                        (if (or (string= "" s)(string= "nil" s)) ""
                                          (format ", M-x %s" s)))
                                    "")))
                     (dstr  (format "%s%s%s  "
                                    (if mem (format "%s : " kd) "")
                                    real-this-command 
                                    (if (string= "" str) ""
                                      (format ": %s" str)))))
                (progn
                  (puthash tc dstr display-new-message-hash)
                  (setq display-new-message-sb dstr ))))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company - completion mode 
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :config
  (progn 
    (add-hook 'after-init-hook 'global-company-mode)

    ;; add dabbrev to company globally 
    (defun company-my-setup ()
      (when (boundp 'company-backends)
        (make-local-variable 'company-backends)
        ;; remove
        ;(setq company-backends (delete 'company-dabbrev company-backends))
        ;; add 
        (add-to-list 'company-backends 'company-dabbrev)
        (add-to-list 'company-backends 'company-ispell)
	(add-to-list 'company-backends 'company-files t)
	(setq company-dabbrev-other-buffers 'all)
        (setq company-dabbrev-ignore-buffers "nil")
        (setq company-dabbrev-downcase nil)
        (setq company-tooltip-limit 20)))  
    (add-hook 'after-init-hook 'company-my-setup) 
    ;; this is a text 
    (setq company-idle-delay 0)    ; bring company up immediately
    
    (use-package color
      :ensure t
      :functions color-lighten-name
      :config
      (progn 
        (use-package company-quickhelp
          :ensure t
          :config (progn 
                    (company-quickhelp-mode 1)))
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

;; php company stuff
(defun my-php ()
  (add-to-list 'company-backends 'company-my-php-backend))

(defvar my-php-symbol-hash "")
(add-hook 'php-mode-hook 'my-php)
(defun company-my-php-backend (command &optional arg &rest ignored)
  (case command
    (prefix (and (eq major-mode 'php-mode)
                 (company-grab-symbol)))
    (sorted t)
    (candidates (all-completions
                 arg
                 (if (and (boundp 'my-php-symbol-hash)
                          my-php-symbol-hash)
                     my-php-symbol-hash
                   
                   (with-temp-buffer
                     (call-process-shell-command
                      "php -r '$all=get_defined_functions();foreach ($all[\"internal\"] as $fun) { echo $fun . \";\";};'"
                      nil t)
                     (goto-char (point-min))
                     (let ((hash (make-hash-table)))
                       (while (re-search-forward "\\([^;]+\\);" (point-max) t)
                         (puthash (match-string 1) t hash))
                       (setq my-php-symbol-hash hash))))))))

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directories stuff
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq default-directory "~/"             ; default directory
      backup-directory-alist             ; backups to single directory
             '(("." . "~/MyEmacsBackups"))
      delete-by-moving-to-trash t        ; Move to trash when deleting stuff
      trash-directory "~/.Trash/emacs"
      insert-directory-program           ; use gls for dired 
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

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File headers with header2
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package header2
  :ensure t
  :config (progn
            (defun make-box-comment-with-region (start end &optional end-col)
              "Make a box comment with region, if active."
              (interactive "r\nP")                 
              (if (not (use-region-p))
                  (make-box-comment end-col)
                (let ((selection (buffer-substring start end)))
                  (if (= (length selection) 0)
                      (make-box-comment end-col)
                    (progn
                      (kill-region start end)
                      (make-box-comment end-col)
                      (insert (replace-regexp-in-string
                               "\n" (concat "\n" (header-prefix-string))
                               (replace-regexp-in-string
                                (concat "^[ \t" (nonempty-comment-start) "]+["
                                        (nonempty-comment-start) "]+")
                                "" selection))))))))
            (add-hook 'emacs-lisp-mode-hook 'auto-make-header)
            (add-hook 'c-mode-common-hook   'auto-make-header)
            (add-hook 'haskell-mode-hook    'auto-make-header)           
            (add-hook 'write-file-hooks 'auto-update-file-header))  
  :bind (("C-h C-h" . make-header)
         ("C-h C-r" . make-revision)
         ("C-h C-b" . make-box-comment-with-region)))

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

(setq comment-auto-fill-only-comments t  ; auto fill comment lines
      indent-tabs-mode nil)              ; use spaces, not <tab>

(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 120)            ; we're not in the 70s

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



(define-globalized-minor-mode my-global-fci-mode fci-mode turn-on-fci-mode)
(my-global-fci-mode 1)

; disable fill-column-indicator if company mode is live
(defvar-local company-fci-mode-on-p nil)
            
(use-package fill-column-indicator 
  :ensure t
  :defines fci-rule-use-dashes
  :config (progn   
            (setq fci-rule-use-dashes t)

            (defun company-turn-off-fci (&rest ignore)
              (when (boundp 'fci-mode)
                (setq company-fci-mode-on-p fci-mode)
                (when fci-mode (fci-mode -1))))
            
            (defun company-maybe-turn-on-fci (&rest ignore)
              (when company-fci-mode-on-p (fci-mode 1)))
            
            (add-hook 'company-completion-started-hook 'company-turn-off-fci)
            (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
            (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)))
            
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
