;;; ceamx-ai.el --- Ceamx AI helpers                 -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Chris Montgomery

;; Author: Chris Montgomery <chmont@protonmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; Model identifiers can be found here (there's probably an easier way):
;; <https://console.anthropic.com/workbench/6c76c61f-14a2-4c35-9979-a86f848453e9>

(defconst ceamx-ai-claude-opus-current-model "claude-opus-4-1-20250805"
  "Identifier for the current version of the high-end Claude Opus LLM model.
This should be updated when a new version is available.")

(defconst ceamx-ai-claude-sonnet-current-model "claude-sonnet-4-20250514"
  "Identifier for the current version of the general-purpose Claude Sonnet LLM model.
This should be updated when a new version is available.")

(defconst ceamx-ai-claude-haiku-current-model "claude-3-5-haiku-20241022"
  "Identifier for the current version of the task-orientated Claude Haiku LLM model.
This should be updated when a new version is available.")

(provide 'ceamx-ai)
;;; ceamx-ai.el ends here
