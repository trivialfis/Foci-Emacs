;;; irony-cdb-utils.el --- Summary
;;; Commentary:

;; Copyright (C) 2011-2016  Guillaume Papin
;; Author: Guillaume Papin <guillaume.papin@epitech.eu>

;; Utils for compilation-database processing originally implemented in
;; irony-mode.  Extracted for convenience.

;;; Code:

(defun irony-cdb-json--adjust-compile-options (compile-options file default-dir)
  "Adjust COMPILE-OPTIONS to only use options useful for parsing.

COMPILE-OPTIONS is modified by side effects but the returned list
should be used since elements can change at the head.

Removes the input file, the output file, ...

Relative paths are relative to DEFAULT-DIR."
  ;; compute the absolute path for FILE only once
  (setq file (expand-file-name file default-dir))
  (let* ((head (cons 'nah compile-options))
         (it head)
         opt)
    (while (setq opt (cadr it))
      (cond
       ;; end of options, skip all positional arguments (source files)
       ((string= opt "--")
        (setcdr it nil))
       ;; strip -c
       ((string= "-c" opt)
        (setcdr it (nthcdr 2 it)))
       ;; strip -o <output-file> and -o<output-file>
       ((string-prefix-p "-o" opt)
        (if (string= opt "-o")
            (setcdr it (nthcdr 3 it))
          (setcdr it (nthcdr 2 it))))
       ;; skip input file
       ((string= file (expand-file-name opt default-dir))
        (setcdr it (nthcdr 2 it)))
       (t
        ;; if head of cdr hasn't been skipped, iterate, otherwise check if the
        ;; new cdr need skipping
        (setq it (cdr it)))))
    (cdr head)))

(defsubst irony-cdb-json--compile-command-directory (compile-command)
  "Get directory from COMPILE-COMMAND."
  (cdr (assq 'directory compile-command)))

(defsubst irony-cdb-json--compile-command-file (compile-command)
  "Get filename from COMPILE-COMMAND."
  (cdr (assq 'file compile-command)))

(defun irony--split-command-line-1 (quoted-str)
  "Remove the escaped quotes and backlash from a QUOTED-STR.

Return a list of the final characters in the reverse order.

Only to be consumed by `irony--split-command-line'."
  (let ((len (length quoted-str))
        (i 0)
        ch next-ch
        result)
    (while (< i len)
      (setq ch (aref quoted-str i))
      (when (eq ch ?\\)
        (let ((next-ch (and (< (1+ i) len)
                            (aref quoted-str (1+ i)))))
          (when (member next-ch '(?\\ ?\"))
            (setq ch next-ch)
            (cl-incf i))))
      (push ch result)
      (cl-incf i))
    result))

(defun irony--split-command-line (cmd-line)
  "Split CMD-LINE into a list of arguments.

Takes care of double quotes as well as backslash.

Sadly I had to write this because `split-string-and-unquote'
breaks with escaped quotes in compile_commands.json, such as in:

    /usr/bin/c++ -DLLVM_VERSION_INFO=\\\\\\\"3.2svn\\\\\\\" <args>"
  ;; everytime I write a function like this one, it makes me feel bad
  (let* ((len (length cmd-line))
         (spaces (string-to-list " \f\t\n\r\v"))
         (first-not-spaces-re (concat "[^" spaces "]"))
         (i 0)
         ch
         args cur-arg)
    (while (< i len)
      (setq ch (aref cmd-line i))
      (cond
       ((member ch spaces)              ;spaces
        (when cur-arg
          (setq args (cons (apply 'string (nreverse cur-arg)) args)
                cur-arg nil))
        ;; move to the next char
        (setq i (or (string-match-p first-not-spaces-re cmd-line i)
                    len)))
       ((eq ch ?\")                     ;quoted string
        (let ((endq (string-match-p "[^\\]\"" cmd-line i)))
          (unless endq
            (signal 'irony-parse-error (list "ill formed command line" cmd-line)))
          (let ((quoted-str (substring cmd-line (1+ i) (1+ endq))))
            (setq cur-arg (append (irony--split-command-line-1 quoted-str)
                                  cur-arg)
                  i (+ endq 2)))))
       (t                             ;a valid char
        ;; if it's an escape of: a backslash, a quote or a space push
        ;; only the following char.
        (when (eq ch ?\\)
          (let ((next-ch (and (< (1+ i) len)
                              (aref cmd-line (1+ i)))))
            (when (or (member next-ch '(?\\ ?\"))
                      (member next-ch spaces))
              (setq ch next-ch)
              (cl-incf i))))
        (push ch cur-arg)
        (cl-incf i))))
    (when cur-arg
      (setq args (cons (apply 'string (nreverse cur-arg)) args)))
    (nreverse args)))

(defun irony-cdb--string-suffix-p (suffix string &optional ignore-case)
  "Return non-nil if SUFFIX is a suffix of STRING."
  (let ((start-pos (- (length string) (length suffix))))
    (and (>= start-pos 0)
         (eq t (compare-strings suffix nil nil
                                string start-pos nil ignore-case)))))

(defun irony-cdb--remove-compiler-from-flags (flags)
  "Remove the compiler from FLAGS read from a compilation database.

When using ccache, the compiler might be present in FLAGS since
the compiler is `ccache compiler'."
  (let* ((first (car flags))
         (flags (cdr flags)))
    (if (irony-cdb--string-suffix-p "ccache" first) (cdr flags) flags)))

(defun irony-cdb-json--compile-command-options (compile-command)
  "Return the compile options of COMPILE-COMMAND as a list."
  (let ((command (assq 'command compile-command))
        (arguments (assq 'arguments compile-command)))
    (irony-cdb--remove-compiler-from-flags
     (cond (command (irony--split-command-line (cdr command)))
           (arguments (append (cdr arguments) nil))))))

(defun irony-cdb-json--transform-compile-command (compile-command)
  "Transform a COMPILE-COMMAND in the JSON cdb into list.

The returned value is a list composed of the following elements:
0. The absolute path to the file.
1. The compile options.
2. The invocation directory.  Relative paths in the compile
   options elements are relative to this directory.

Return nil if the compile command is invalid or the compile
options are empty."
  (let* ((directory (irony-cdb-json--compile-command-directory compile-command))
         (path (expand-file-name
                (irony-cdb-json--compile-command-file compile-command) directory))
         (options (irony-cdb-json--compile-command-options compile-command)))
    (when (and path directory options)
      (list path
            (irony-cdb-json--adjust-compile-options options path directory)
            directory))))


(provide 'irony-cdb-utils)
;;; irony-cdb-utils.el ends here
