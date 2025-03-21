;;; or-rag.el --- RAG over Org Roam -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2025-03-20
;; Package-Version: 1.0.0
;; Package-Requires: (llm markdown-mode org-roam)


;; This file is not part of GNU Emacs.


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


;;; Commentary:
;; This package provides RAG functionalities over org-roam files using LLM.
;; This package calls LLM through ahyatt/llm <https://github.com/ahyatt/llm>,
;; so that you need to set one of the llm providers to `or-rag-llm-provider'.


;;; Code:
(require 'llm)
(require 'markdown-mode)
(require 'org-roam)


(defgroup or-rag nil
  "RAG over Org Roam."
  :group 'external)

(defvar or-rag-llm-provider nil
  "LLM provider for or-rag.")


(defcustom or-rag-llm-system-prompt
  "You are skillfull, kind, and friendly assistant.
Users will ask you questions with some context documents.
You must answer their questions based on these context documents.
These context documents are written in emacs org-mode."
  "System Prompt to guid LLM"
  :type '(string)
  :group 'or-rag)


(defcustom or-rag-llm-user-prompt
  "Please answer user question based on context documents.

User Question
-------------
%1$s

Context Documents
-----------------
%2$s
"
  "Prompt Template for RAG.

User question will be inserted at %1$s,
context documents retrieved at %2$s by `format' function"
  :type '(string)
  :group 'or-rag)


(defun or-rag--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT."
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt :context or-rag-llm-system-prompt)
    (make-llm-chat-prompt
     :context or-rag-system-prompt
     :interactions
     (list (make-llm-chat-prompt-interaction :role 'user :content prompt)))))

(defcustom or-rag-response-buffer-name
  "*or-rag*" "Buffer name of LLM response."
  :type '(string)
  :group 'or-rag)


(defun or-rag--response-buffer ()
  "Create and display or-rag response buffer."
  (let ((buffer (generate-new-buffer or-rag-response-buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (display-buffer buffer)
        (markdown-mode)))
    buffer))

(defun or-rag--show-response-streaming (buffer response)
  "Show LLM (partial) RESPONSE at specified BUFFER."
  (save-excursion
    (with-current-buffer buffer
      (erase-buffer)
      (insert response))))

(defun or-rag--chat-streaming (prompt)
  "Chat with LLM streaming using PROMPT."
  (let* ((buffer (or-rag--response-buffer))
         (callback #'(lambda (response)
                       (or-rag--show-response-streaming buffer response))))
    (llm-chat-streaming or-rag-llm-provider
                        (or-rag--make-llm-prompt prompt)
                        callback callback #'ignore)))

(defcustom or-rag-db-location
  (locate-user-emacs-file "or-rag.duckdb")
  "The path to file where the Or-Rag database is stored.")

(defun or-rag--retrieve (question)
  "Retrieve documents for QUESTION")

(defun or-rag--ask (question)
  "Ask QUESTION to LLM."
  (let* ((contexts (or-rag--retrieve question))
         (prompt (format or-rag-llm-user-prompt question contexts)))
    or-rag--chat-streaming prompt))


(provide 'or-rag)
;;; or-rag.el ends here
