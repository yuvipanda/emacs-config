;; My from-scratch emacs config, following along a little bit from the Emacs from Scratch folksm
;; invoke with XDG_CONFIG_HOME=$(pwd) emacs

; Disable the startup message
(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1) ; Disable visible toolbar
(menu-bar-mode -1) ; Disable visible menubar
; Disable visual tooltips at the cursor, instead displaying them in the 'echo bar' at the bottom.
; I like it better at the bottom, but perhaps that's just my experience from doom? We can revisit later
(tooltip-mode -1)

; Without this, you need Ctrl-G or something to kill any particular command.
; Fuxk that, that's what my ESC key is for
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

; Set up our font.
; TODO: Find out what the fuck unit this is. And what does the 'default nil mean?
(set-face-attribute 'default nil :font "JetBrains Mono" :height 180)

; Set a theme!
; 'tango-dark is a *symbol*. How is this different from :font earlier?
(load-theme 'tango-dark)

; Setu package management
(require 'package) ; This is the built in, meh package manager within emacs
                   ; TODO: Figure out what this gets stuff from.
(package-initialize) ; Initialize the built in package manager
(unless package-archive-contents
  (package-refresh-contents)) ; Equivalent of apt-get update
; Add MELPA to list of package sources. I *think* this is the one where you don't have
; to sign your soul away to RMS to publish in.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

; Install use-package, which is the package manager we actually wanna use
; But only if it isn't already installed!
(unless (package-installed-p 'use-package)
  (package-install 'use-package)
  )

; Setup use-package
(require 'use-package)
; This makes :ensure be t by default for all use-package macro calls, so if the package
; is not currently in the system it *will* be installed. This is the behavior I want, as
; otherwise I've to have a separate setup for installation and configuration.
; https://github.com/jwiegley/use-package#package-installation
; Apparently this does *not* keep it up to date or check versions. Which seems fair enough,
; as otherwise you'd need to do a remote call every startup!
(setq use-package-always-ensure t)

; Setup completion package
; This by default seems to enable completion in find-file, M-x, etc.
; FIXME: Put some keybindings here? But I don't want emacs keybindings
(use-package ivy
  :config
  (ivy-mode 1)
  )

; Use same modeline that doom uses! But simpler to start with, for some reason.
; We can customize this later
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(doom-modeline--bar-active use-package counsel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
