;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filename:	  .emacs el lisp�ļ�
;;
;; Copyright (C) 2011
;; Created at:	Aug 12 15:56:07 2011
;; Author : YuChao from SINA
;; E-Mail : yuchao86@gmail.com
;; Description:   �����ļ�������Ϊ".emacs"�������û�HOMEĿ¼���ɡ�
;; �ο���ˮľ�廪�����ĸ������ã�����Emacs������������
;; �ش˸�лEMACS��������http://emacser.com/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;**********************	basic setting	*********************
 
;;Load_path
(add-to-list 'load-path' "~/.emacs.d/plugins")
 
;;todo_path
(setq todo-file-do "~/emacs/todo/do")
(setq todo-file-done "~/emacs/todo/done")
(setq todo-file-top "~/emacs/todo/top")
 
;;Personal information
(setq user-full-name "Yu Chao")
(setq user-mail-address "yuchao86@gmail.com")
 
;;======================	time setting		=====================
;;ʱ����ʾ����
;;����ʱ����ʾ���ã���minibuffer������Ǹ����ϣ����˽�ʲô���ţ�
(display-time-mode 1)
;;ʱ��ʹ��24Сʱ��
(setq display-time-24hr-format t)
;;ʱ����ʾ�������ں;���ʱ��
(setq display-time-day-and-date t)
;;ʱ�����Ա������ʼ�����
(setq display-time-use-mail-icon t)
;;ʱ��ı仯Ƶ�ʣ���λ�������ţ�
(setq display-time-interval 10)
;;��ʾʱ�䣬��ʽ����
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-day-and-date t)
;;----------------------	END	time setting	---------------------
 
;;======================	color setting		=====================
;; ָ����ɫ����Ϊ��ɫ
;;(set-cursor-color "white")
;; �����ɫ����Ϊ��ɫ
;;(set-mouse-color "white")
;; ���ñ�����ɫ��������ɫ
(set-foreground-color "white")
(set-background-color "darkblue")
;; ��������һЩ��ɫ���﷨������ʾ�ı��������⣬����ѡ��ı��������⣬����ѡ��ı�����ѡ��
(set-face-foreground 'highlight "white")
(set-face-background 'highlight "blue")
(set-face-foreground 'region "cyan")
(set-face-background 'region "blue")
(set-face-foreground 'secondary-selection "skyblue")
(set-face-background 'secondary-selection "darkblue")
;;����������һЩ��ɫ
(setq calendar-load-hook
'(lambda ()
(set-face-foreground 'diary-face "skyblue")
(set-face-background 'holiday-face "slate blue")
(set-face-foreground 'holiday-face "white")))
 
;;----------------------	END	color setting	---------------------
 
;;Font
(set-default-font "13")
 
;;��Ҫ������ʱ�ļ�
(setq-default make-backup-files nil)
 
;;�����������Ի���
;;(set-language-environment 'Chinese-GB)
 
;;д�ļ��ı��뷽ʽ
;;(set-buffer-file-coding-system 'gb2312)
(set-buffer-file-coding-system 'utf-8)
 
;;�½��ļ��ı��뷽ʽ
;;(setq default-buffer-file-coding-system 'gb2312)
(setq default-buffer-file-coding-system 'utf-8)
 
;;�ն˷�ʽ�ı��뷽ʽ
(set-terminal-coding-system 'utf-8)
 
;;��������ı��뷽ʽ
;;(set-keyboard-coding-system 'gb2312)
 
;;��ȡ��д���ļ����ı��뷽ʽ
(setq file-name-coding-system 'utf-8)
 
;;�򿪾����� text ģʽ
(setq default-major-mode 'text-mode)
 
;;����������Ϣ
;;(setq inhibit-startup-message t)
 
;;�﷨����
(global-font-lock-mode t)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'LaTeX-hide-environment 'disabled nil)
 
;;��ͼƬ��ʾ����
;;(auto-image-file-mode t)
 
;;�� y/n ��� yes/no
(fset 'yes-or-no-p 'y-or-n-p)
 
;;��ʾ���к�
(column-number-mode t)
;;��ʾ�к�
(setq mouse-yank-at-point t)
 
;;��ʾ����ƥ��
(show-paren-mode t)
 
;;���ع�����
;;(tool-bar-mode nil)
 
;; ���ع�������ʵ�����ٱ����ϵ�emacs-2.23��û�����ģʽ�ġ�
;;(scroll-bar-mode nil)
;;ȥ��������
;;(tool-bar-mode nil)
;;ȥ���˵������ҽ�F10��Ϊ��ʾ�˵�������һʲô�������ˣ���Ҫ�˵����˿�����F10����������F10��ȥ���˵�
(menu-bar-mode nil)
 
;;��꿿�����ָ��ʱ�������ָ���Զ��ÿ�
;;(mouse-avoidance-mode 'animate)
 
;;��Ҫ������������
;;(setq mouse-yank-at-point t)
 
;;������ʾѡ�е�����
(transient-mark-mode t)
 
;;֧��emacs���ⲿ����Ŀ���ճ��
(setq x-select-enable-clipboard t)
 
;;�ڱ�������ʾ��ǰλ��
(setq frame-title-format "Admire@%b")
 
;;���Եݹ��ʹ�� minibuffer
(setq enable-recursive-minibuffers t)
 
;;��ʾ80�оͻ���
(setq default-fill-column 80)
 
;;��ֹ�ն�����
(setq visiable-bell t)
 
;;����tabΪ4���ո�Ŀ��
;;(setq default-tab-width 4)
 
;;��ȱʡ���õĹ���
(setq version-control t)
(setq kept-new-versions 3)
(setq delete-old-versions t)
(setq kept-old-versions 2)
(setq dired-kept-versions 1)
 
;;���ñ���ʱ�İ汾���ƣ��������Ӱ�ȫ��
(mapcar
  (function (lambda (setting)
					 (setq auto-mode-alist
							   (cons setting auto-mode-alist))))
   '(("\\.xml$".  sgml-mode)
	 ("\\\.bash" . sh-mode)
	 ("\\.rdf$".  sgml-mode)
	 ("\\.session" . emacs-lisp-mode)
	 ("\\.l$" . c-mode)
	 ("\\.css$" . css-mode)
	 ("\\.cfm$" . html-mode)
	 ("gnus" . emacs-lisp-mode)
	 ("\\.idl$" . idl-mode)))
 
;;�� dired ���Եݹ�Ŀ�����ɾ��Ŀ¼
(setq dired-recursive-copies 'top)
(setq dired-recursive-deletes 'top)
 
;;ʱ�������(time-stamp)���趨�ĵ��ϴα������Ϣ
;;ֻҪ��������ĵ�����Time-stamp:�����ã��ͻ��Զ�����ʱ���
;;����time-stamp
(setq time-stamp-active t)
;;ȥ��time-stamp�ľ��棿
(setq time-stamp-warn-inactive t)
;;����time-stamp�ĸ�ʽ�������µĸ�ʽ���õ�һ�����ӣ�
(setq time-stamp-format "%:u %02m/%02d/%04y %02H02M02S")
;;���޸�ʱ�����ӵ������ļ��Ķ����
(add-hook 'write-file-hooks 'time-stamp)
 
;;���ĵ�����Զ�����հ�һ�У�����ĳЩϵͳ�����ļ�����Ҫ������
(setq require-final-newline t)
(setq track-eol t)
 
;;ʹ��C-kɾ��ָ�뵽����ĩ�����ж���
(setq-default kill-whole-line t)
 
;;�趨ɾ�������¼Ϊ200�����Է����Ժ����޻ָ�
(setq kill-ring-max 200)
 
;;����ʹ�ò��Һ����ͱ�����Ѱ�ҷ�Χ
(setq apropos-do-all t)
 
;;����aspell������ΪEmacs��ƴд����ѧ
(setq-default ispell-program-name "aspell")
 
;;ʹ��narrow����ʱ��һ������
(put 'narrow-to-region 'disabled nil)
 
;;����Emacs�Զ�����Ϊ��������(���¸�һ��)
;;(split-window-vertically)
 
;;�����ǽ���ǰ����Ϊ��ҳ��һ�У�ͬ�ն��µ�clear�����е�����
(defun line-to-top-of-window ()
"Move the line point is on to top of window."
(interactive)
(recenter 0))
 
;;�����۵�
(load-library "hideshow")
(add-hook 'java-mode-hook 'hs-minor-mode)
(add-hook 'perl-mode-hook 'hs-minor-mode)
;;(add-hook 'php-mode-hook 'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
 
;;��ʱ�Ǻ�
;;��ʱ����Ҫ������һ���ļ�����һЩ������Ȼ��ܿ�����������㵱Ȼ���� ʹ�� bookmark���߼Ĵ�����
;;������Щʵ����̫���ˡ������ӵ��vi������ ma, mb, 'a, 'b �Ĳ���������������ü��� elisp �ﵽ���Ƶ�Ŀ��
(global-set-key [(control ?\.)] 'ska-point-to-register)
(global-set-key [(control ?\,)] 'ska-jump-to-register)
(defun ska-point-to-register()
  "Store cursorposition _fast_ in a register.
Use ska-jump-to-register to jump back to the stored
position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
 
(defun ska-jump-to-register()
  "Switches between current cursorposition and position
that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
		(jump-to-register 8)
		(set-register 8 tmp)))
 
;; go-to-char �ǳ���л Oliver Scholz �ṩ��������� �ҡ�
;;���������һ�� vi �� "f" ��������Ʒ��vi���û�֪����vi�� һ���ر�õ����� "f"�����㰴 "fx", x ������һ���ַ�ʱ
;;��� �ͻ��ƶ�����һ�� "x" ������֮��ֻҪ�� ";"(�ֺ�)�����͵��� ��һ�� "x"��
;;�ٸ�����˵������������;����������������һ���֣������ ���ס�
;;(setq unread-command-events (list last-input-event)))
;;											   ^^^^^
;;����ϣ��Ѹ�ٵĵ�������Ǹ� event ������������ vi �ﰴ "fe"�������굽�� "setq" ���Ǹ� e ���棬��ʱ���ҽ��Ű� ";",
;;��һ����͵���������Ҫ�ĵط����ܷ���ɣ�������������ã����� �㷢������ķǳ��ã�
 
;;��һֱ���� Emacs û������һ�������������� Oliver ���� ��һ�������Ĵ𰸣�
;;������δ���֮�󣬵��㰴 C-c a x (x ������һ���ַ�) ʱ���� ��ͻᵽ��һ�� x �����ٴΰ� x�����͵���һ�� x������ C-c a w w w w ..., C-c a b b b b b b ...
;;�Ҿ��������ʽ�� vi �� "f" Ҫ�졣
(defun wy-go-to-char (n char)
  "Move forward to Nth occurence of CHAR.
Typing `wy-go-to-char-key' again will move forwad to the next Nth
occurence of CHAR."
  (interactive "p\ncGo to char: ")
  (search-forward (string char) nil nil n)
  (while (char-equal (read-char)
			 char)
	(search-forward (string char) nil nil n))
  (setq unread-command-events (list last-input-event)))
(define-key global-map (kbd "C-c a") 'wy-go-to-char)
 
;;======================		Chinese setting		=====================
;;(setq gnus-default-charset 'cn-gb-2312
;;gnus-group-name-charset-group-alist '((".*" . gb2312))
;;gnus-summary-show-article-charset-alist '((1 . cn-gb-2312) (2 . big5) (3 . chinese-gbk) (4 . utf-8))
;;gnus-newsgroup-ignored-charsets '(unknown-8bit x-unknown iso-8859-1)
;;gnus-group-posting-charset-alist '((".*" gb2312 (gb2312))))
;;(define-coding-system-alias 'gb18030 'gb2312)
 
;;------------����(utf-8)ģʽ------------
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(setq ansi-color-for-comint-mode t) ;;��|��shell-mode??,o??��[M`J3
 
;;���� sentence-end ����ʶ�����ı�㡣������ fill ʱ�ھ�ź�� �������ո�
(setq sentence-end "\\([������]\\|����\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
 
;;----------------------		End Chinese setting		---------------------
 
;;��������
(gnus-add-configuration '(article (vertical 1.0 (summary .35 point) (article1.0))))
 
;;Group Buffer����
;;�Զ�����Topic-mode
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)
 
;;Sumarry Buffer����
(setq gnus-summary-line-format ":%U%R %B %s %-60=|%4L |%-20,20f |%&user-date; \n")
 
;;Article Buffer����
;;�趨Ҫ��ʾ��ͷ��Ϣ��ʽ
(setq gnus-visible-headers
"^\\(^To:\\|^CC:\\|^From:\\|^Subject:\\|^Date:\\|^Followup-To:
\\|^X-Newsreader:\\|^User-Agent:\\|^X-Mailer:
\\|Line:\\|Lines:\\|Content-Type:\\|NNTP-Posting-Host\\)")
 
;; session.el
;;(require 'session)
;;	(add-hook 'after-init-hook 'session-initialize)

;;�Զ����У�ÿ��80���ַ�
(add-hook 'message-mode-hook (lambda ()
(setq fill-column 80)
(turn-on-auto-fill)))
 
;;======================	�����������˳��		 =====================;
;;�������ǿ������ģ�������ô�ٸİɡ�
;;(setq font-encoding-alist
;;(append '(("MuleTibetan-0" (tibetan . 0))
;;("GB2312" (chinese-gb2312 . 0))
;;("JISX0208" (japanese-jisx0208 . 0))
;;("JISX0212" (japanese-jisx0212 . 0))
;;("VISCII" (vietnamese-viscii-lower . 0))
;;("KSC5601" (korean-ksc5601 . 0))
;;("MuleArabic-0" (arabic-digit . 0))
;;("MuleArabic-1" (arabic-1-column . 0))
;;("MuleArabic-2" (arabic-2-column . 0))) font-encoding-alist))
;;----------------------	�����������˳��		---------------------
 
;;�����֣�Ĭ�ϵĹ���̫�죬�����Ϊ3��
(defun up-slightly () (interactive) (scroll-up 3))
(defun down-slightly () (interactive) (scroll-down 3))
(global-set-key [mouse-4] 'down-slightly)
(global-set-key [mouse-5] 'up-slightly)
 
;;Լ������
(setq appt-issue-message t)
 
;;**********************		ȫ�ְ����趨		*********************
;;����[f1]Ϊgoto-line
;;(global-set-key [f1] 'info)
(global-set-key [f1] 'goto-line)
(global-set-key [f2] 'eshell)
;;(global-set-key [f3] 'shell)
 
(global-set-key [f4] 'kill-this-buffer)
;;[f11] define to maximise present window
;;ϰ�����ã��򿪣��رղ˵�
(global-set-key [f12] 'menu-bar-mode)
 
;;�ƶ��������
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end]  'end-of-buffer)
 
;;�л�buffer������
(global-set-key [(control o)] 'other-window)
(global-set-key [(control tab)] 'tabbar-forward)
 
;;**********************		���ñ������		*********************
;; ����Alt+/���ͻᵯ���˵������Զ���ȫ
;;(define-key c-mode-base-map [(meta ?/)] 'semantic-ia-complete-symbol-menu)
 
;;�ȼ�����
(global-set-key [f5] 'complile)
	(setq-default compile-command "make")
(global-set-key [f6] 'speedbar)
(global-set-key [f7] 'gdb)
(global-set-key [f8] 'previous-error)
(global-set-key [f9] 'next-error)
;;��������gdb
(global-set-key [f10] 'gdb-many-windows)
 
;;(global-set-key [f9] 'delete-window);F9 �ر�d��ǰ����
;;(global-set-key [f8] 'other-window);F8���ڼ���ת
;;(global-set-key [(f2)] 'ansi-term);F2 �л���shellģʽ
;;(global-set-key [f3] 'split-window-vertically);F3�ָ��
;;(global-set-key [f12] 'my-fullscreen);F12 ȫ��
;;(global-set-key [(f4)] 'compile);����
;;(global-set-key [f5] 'gdb);����gdb
;;(global-set-key [(f6)] 'gdb-many-windows);��������gdb
;;(global-set-key [f1] 'goto-line);����M-gΪgoto-line
;;(global-set-key [f7] 'other-frame);������������
;;(global-set-key [(f3)] 'speedbar);��speedbar
 
;;########### 	CC-mode����  http://cc-mode.sourceforge.net/ 	################
(require 'cc-mode)
(c-set-offset 'inline-open 0)
(c-set-offset 'friend '-)
(c-set-offset 'substatement-open 0)
:;(setq indent-tabs-mode nil)
(setq c-basic-offset 4 )
(setq default-tab-width 4)
;;(setq tab-stop-list())
;;(loop for x downfrom 40 to 1 do
   ;; (setq tab-stop-list (cons(* x 4) tab-stop-list)))
 
;;Ĭ��Linux�ں˱�̷��ȡ��Linux�ں�˵���ĵ�
(setq auto-mode-alist (cons '("/usr/src/linux.*/.*\\.[ch]$" . linux-c-mode)
   auto-mode-alist))
 
(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
   (interactive)
   (c-mode)
   (c-set-style "K&R")
   (setq tab-width 4) ;;Ĭ�Ϸ����8������
   (setq indent-tabs-mode t)
   (setq c-basic-offset 4))  ;;Ĭ�Ϸ����8������
 
;;c-mode��cc-mode������ֻ��4��
(add-hook 'c-mode-hook
		  '(lambda ()
			 (c-set-style "Stroustrup")))
 
;;****************************************************************
;;C/C++���Ա༭����
;;��Щ����
;;(defun my-c-mode-common-hook()
;;(define-key c-mode-base-map [(f7)] 'compile)
;;(setq tab-width 4 indent-tabs-mode nil)
;; hungry-delete and auto-newline
;;(c-toggle-auto-hungry-state 1)
 
;;C++���Ա༭����
;;(defun my-c++-mode-hook()
	;;(setq c-basic-offset 4 )
   ;; (setq tab-width 4 indent-tabs-mode nil)
   ;; (c-set-style "stroustrup")
	;;(define-key c++-mode-map [f3] 'replace-regexp)
;;)
 
;;_____________C______________
;;CԤ��������
;;(setq c-macro-shrink-window-flag t)
;;(setq c-macro-preprocessor "c")
;;(setq c-macro-cppflags " ")
;;(setq c-macro-prompt-flag t)
;;(setq abbrev-mode t)
;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
 
;;___________CPP______________
;;CPPԤ��������
;;(setq c-macro-shrink-window-flag t)
;;(setq c-macro-preprocessor "cpp")
;;(setq c-macro-cppflags " ")
;;(setq c-macro-prompt-flag t)
;;(setq abbrev-mode t)
;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
 
;;����������Զ�����
;;(c-toggle-auto-hungry-state 1)
;;(c-set-style "stroustrup")
;;(add-hook 'c-mode-common-hook 'program-mode)
;;(add-hook 'c++-mode-common-hook 'program-mode)


;;======================			�Զ���ȫ����		  =====================
;;�Զ���ȫ���ܣ����´��������վֱ��Copy�����ģ�����һЩ���Դ˵�˵��
;;�������� hippie-expand �Ĳ�ȫ��ʽ������һ�������б� hippie-expand ������ʹ�ñ���ǰ��
;;�ĺ�������ȫ����˵������ʹ�õ�ǰ��buffer��ȫ������Ҳ������͵���Ŀɼ��Ĵ�����Ѱ�ң���
;;���Ҳ�������ô�����д򿪵�bufferȥ�ң������������ô��kill-ring����ļ�����������б�
;;���list����ǰʹ�õ�ƥ�䷽ʽ���� echo ������ʾ��
;;�ر�����˼���� try-expand-line�������԰��㲹ȫ����һ�����֡��Һܶ�ʱ�����������ִ�����
;;ͬ��ֻ�м����ֲ�һ��������������ȥ����ճ�����¡���ô�Ҿ������������ֵ�ǰ�漸���֡�Ȼ��
;;�ఴ���� M-/ ���ܵõ���һ�С�
(global-set-key [(meta ?/)] 'hippie-expand)
(setq hippie-expand-try-functions-list
'(try-expand-line
try-expand-line-all-buffers
try-expand-list
try-expand-list-all-buffers
try-expand-dabbrev
try-expand-dabbrev-visible
try-expand-dabbrev-all-buffers
try-expand-dabbrev-from-kill
try-complete-file-name
try-complete-file-name-partially
try-complete-lisp-symbol
try-complete-lisp-symbol-partially
try-expand-whole-kill))
 
;;hippie���Զ�������ԣ����ȵ�����senator�ķ��������
(autoload 'senator-try-expand-semantic "senator")
;;----------------------			End �Զ���ȫ		---------------------
 
;;======================		���ñ�̲��			=====================
 
;;**********************			auto-header			*********************
 
(add-to-list 'load-path "~/.emacs.d/plugins/")
;;(require 'auto-header)
;;����auto-header.el�ļ�,�Զ�����ļ�ͷ
;;(require 'auto-header)
;; �����ļ�ͷ�е�����
(setq header-full-name "Yu Chao")
;; ��������
(setq header-email-address "yuchao86@gmail.com")
 
;; ����ÿ�α���ʱҪ���µ���Ŀ
(setq header-update-on-save
	'(  filename
		modified
		counter
		copyright))
;; �����ļ�ͷ����ʾ��ʽ
(setq header-field-list
'(  filename  ;�ļ���
	blank	 ;���У���ͬ
	;;copyright ;;��Ȩ
	version
	author	;����
	created   ;������
	blank
	description   ;����
	;;blank
	;;modified_by ;������
   ;; blank
	;;status  ;״̬���Ƿ񷢲�
	;;����
	;;blank
  ))
;;----------------------		END	auto-header			---------------------
 
;;======================			Load cedet			=====================
;; See cedet/common/cedet.info for configuration details.
;;(load-file "~/.emacs.d/plugins/cedet-1.0pre7/common/cedet.el")
 
;; Enable EDE (Project Management) features
;;(global-ede-mode 1)
 
;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")
 
;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:
 
;; * This enables the database and idle reparse engines
;;(semantic-load-enable-minimum-features)
 
;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
;;(semantic-load-enable-code-helpers)
 
;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)
 
;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)
 
;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)
;;----------------------			END cedet				---------------------
 
;;======================			Load ecb				=====================
(add-to-list 'load-path "~/.emacs.d/plugins/ecb")
(require 'ecb)
;;(require 'ecb-autoloads)
;;��emacs��Ȼ��M-x ecb-activate���ɴ�ecb��
;;----------------------			END	ecb				---------------------
 
;;======================			Load cscope				=====================
(add-to-list 'load-path "~/emacs.d/plugins/cscope-15.7a/contrib/xcscope/")
;;(require 'xcscope)
;;�������ʮ���Ӵ󣬼�ʹ���ж��ļ��������Ҳ�ܷ�ʱ�����Ը��� Emacs ���� ������ cscope.out��
;;(setq cscope-do-not-update-database t)
;;��M-x !��Ȼ��cscope -b��֮��Ϳ�����Դ�����н�����ת�ˡ������cscope�˵�
;;������ƪ����ô��emacs�°�װ��ʹ��cscope�ģ�
;;http://ann77.stu.cdut.edu.cn/EmacsCscope.html
;;----------------------		  	END	cscop				---------------------
 
;;======================			Load color-theme			=====================
;;��ɫ����
;;(load-file "~/.emacs.d/plugins/color-theme.el")
;;(require 'color-theme)
;;(add-hook 'c-mode-common-hook 'color-theme-taylor)
;;(add-hook 'c++-mode-common-hook 'color-theme-taylor)
;;----------------------			END	color-theme			---------------------
 
;;======================			`Load gdb-many-window			=====================
;;gdb-many-window
;;������ܲ������ʹemacs�ĵ��Խ�����VCһ������watch, stacktrace�ȴ��ڣ�����ʵ��ͼ�λ�gdb.
;;��emacs�б���ó���Ȼ��M-x gdb����������ret���ര��gdb�ͳ�����
;;http://www.inet.net.nz/~nickrob/multi-gud.el
;;http://www.inet.net.nz/~nickrob/multi-gdb-ui.el
 
(add-to-list 'load-path"~/.emacs.d/plugins")
(setq gdb-many-windows t)
(load-library "multi-gud.el")
(load-library "multi-gdb-ui.el")
;;----------------------				END	gdb-many-window			---------------------
 
;;======================			Load linum			 	=====================
;;����linum.el(line number)����ʾ�кţ�
;;(add-to-list 'load-path"~/.emacs.d/plugins")
(require 'linum)
(global-linum-mode 1)
;;----------------------			END	linum				---------------------
 
;;======================			Load Doxymacs 			 	=====================
;;����linum.el(line number)����ʾ�кţ�
;;(add-to-list 'load-path"~/.emacs.d/plugins/")
;;(require 'doxymacs)
 
;;(add-hook 'c-mode-common-hook 'doxymacs-mode)
 
 ;;(defun my-doxymacs-font-lock-hook ()
 ;;(if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
	;;	(doxymacs-font-lock)))
  ;;(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)
 
;;  This will add the Doxygen keywords to c-mode and c++-mode only.
;;- Default key bindings are:
;;  - C-c d ? will look up documentation for the symbol under the point.
;;  - C-c d r will rescan your Doxygen tags file.
;;  - C-c d f will insert a Doxygen comment for the next function.
;;  - C-c d i will insert a Doxygen comment for the current file.
;;  - C-c d ; will insert a Doxygen comment for the current member.
;;  - C-c d m will insert a blank multi-line Doxygen comment.
;;  - C-c d s will insert a blank single-line Doxygen comment.
 ;; - C-c d @ will insert grouping comments around the current region.
 
;;----------------------			END	Doxymacs				---------------------
 
;;======================			Load yasnippet				=====================
;;�Զ���ȫ������
(add-to-list 'load-path
                  "~/.emacs.d/plugins/yasnippet-0.6.1c")
    (require 'yasnippet) ;; not yasnippet-bundle
    (yas/initialize)
    (yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")
;;----------------------			END	yasnippet			---------------------

;;================================PHP-Mode===================================

;; PHP �﷨֧��,��ҪMMMģ�����Ӳ�֧�ֻ�ϴ���
(add-to-list 'load-path"~/.emacs.d/plugins/php")
;;php-mode

(require 'php-mode)

;;������չ����

(add-to-list 'auto-mode-alist '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.module\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.inc\\'" . php-mode))

(require 'auto-complete)
(locate-library "auto-complete.el")
;;php �Զ���ʾ���� 

(add-hook 'php-mode-hook

(lambda ()

(require 'php-completion)

(php-completion-mode t)

(define-key php-mode-map (kbd "C-o") 'phpcmp-complete)))

(add-hook  'php-mode-hook

(lambda ()

(when (require 'auto-complete nil t)

(make-variable-buffer-local 'ac-sources)

(add-to-list 'ac-sources 'ac-source-php-completion)

;; if you like patial match,

;; use `ac-source-php-completion-patial' instead of `ac-source-php-completion'.

(add-to-list 'ac-sources 'ac-source-php-completion-patial)

(auto-complete-mode t))))
		  
		  
;;====================================PHP�Զ���ʾ========================
(add-to-list 'load-path "~/.emacs.d/plugins/php/")
(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/plugins/php/")
;;����ʹ��install-elisp-from-emacswiki �������������ļ�·�����Ұ����Ƿ���һ���ļ�������
