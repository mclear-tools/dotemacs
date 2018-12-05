;;; url-http-ntlm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "url-http-ntlm" "url-http-ntlm.el" (0 0 0 0))
;;; Generated autoloads from url-http-ntlm.el

(autoload 'url-ntlm-auth "url-http-ntlm" "\
Return an NTLM HTTP authorization header.
Get the contents of the Authorization header for a HTTP response
using NTLM authentication, to access URL.  Because NTLM is a
two-step process, this function expects to be called twice, first
to generate the NTLM type 1 message (request), then to respond to
the server's type 2 message (challenge) with a suitable response.

url-get-authentication' calls `url-ntlm-auth' once when checking
what authentication schemes are supported (PROMPT and ARGS are
nil), and then twice for every stage of the handshake: the first
time PROMPT is nil, the second, t; ARGS contains the server
response's \"WWW-Authenticate\" header, munged by
`url-parse-args'.

If PROMPT is not t then this function just returns nil.  This is
to avoid calculating responses twice.

OVERWRITE and REALM are ignored.

ARGS is expected to contain the WWW-Authentication header from
the server's last response.  These are used by
`url-http-get-stage' to determine what stage we are at.

\(fn URL &optional PROMPT OVERWRITE REALM ARGS)" nil nil)

(url-register-auth-scheme "ntlm" nil 8)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "url-http-ntlm" '("url-http-ntlm--")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; url-http-ntlm-autoloads.el ends here
