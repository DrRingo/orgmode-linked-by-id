;;; orgmode-linked-by-id.el --- Extension for Emacs Org-mode to create cross-links between headers using IDs -*- lexical-binding: t; -*-

;; Copyright (C) 2024 drringo

;; Author: drringo
;; Maintainer: drringo
;; URL: https://github.com/drringo/orgmode-linked-by-id
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1") (helm "3.0"))
;; Keywords: org, links, helm, fuzzy-search

;; This file is part of GNU Emacs.

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

;; Extension for Emacs Org-mode to create cross-links between headers
;; using IDs instead of header content.
;;
;; This extension provides three main features:
;; 1. Link headers in current file
;; 2. Link headers in directory
;; 3. Link to named images/tables
;;
;; Keybindings:
;; - C-c l i or SPC l i: Insert ID link in current file
;; - C-c l f or SPC l f: Insert ID link in directory
;; - C-c l c or SPC l c: Insert link to named image/table

;;; Code:

;; ==============================================================
;; LIÊN KẾT BẰNG ID TRONG ORG-MODE
;; ==============================================================
;; Mô tả: Extension cho Emacs Org-mode để tạo liên kết chéo giữa các header
;; bằng cách sử dụng ID thay vì nội dung header.
;;
;; LÝ DO SỬ DỤNG ID:
;; - Tránh vấn đề khi header thay đổi nội dung
;; - Không cần suy nghĩ về CUSTOM_ID
;; - ID được tạo tự động bằng org-id-get-create (SPC m I)
;;
;; QUY TRÌNH LÀM VIỆC:
;; 1. Tạo ID cho header: SPC m I
;; 2. Tạo liên kết đến header khác:
;;    - C-c l i: Tìm trong file hiện tại
;;    - C-c l f: Tìm trong thư mục (không bao gồm thư mục con)
;;    - C-c l c: Tìm hình ảnh/bảng có tên
;; 3. Chọn header từ danh sách
;; 4. Tùy chỉnh description nếu cần
;; 5. Link được chèn vào vị trí con trỏ

;; Load Helm package
(require 'helm)
(helm-mode 1)

;; ==============================================================
;; PHẦN 1: LIÊN KẾT HEADER TRONG FILE HIỆN TẠI
;; ==============================================================

(require 'org) ; Đảm bảo load org trước khi dùng các hàm org-*

(defun drringo/org-get-headers-with-ids ()
  "Tìm tất cả headers có ID trong file hiện tại và chèn liên kết Org-mode."
  (interactive)
  (let* ((org-ids
          (delq nil
                 (org-map-entries
                  (lambda ()
                    (let ((id (org-entry-get (point) "ID")))
                      (when id
                        (list (org-get-heading t t t t) id))))
                  nil 'file))))
    (if (null org-ids)
        (message "No headers with IDs found in current file")
      (let* ((selection (helm-comp-read "Select header: " (mapcar #'car org-ids))))
        (when selection
          (let* ((id (cadr (assoc selection org-ids)))
                 (description selection)
                 (link (format "[[id:%s][%s]]" id description)))
            (insert link)))))))

;; Thiết lập phím tắt
(global-set-key (kbd "C-c l i") 'drringo/org-get-headers-with-ids)

;; ==============================================================
;; PHẦN 2: LIÊN KẾT HEADER TRONG THƯ MỤC
;; ==============================================================

;; Load các package cần thiết
(require 'org-id)

(defun drringo/org-get-headers-with-ids-in-folder (directory)
  "Search all org headers with IDs in DIRECTORY (no subfolders) and insert an Org-mode link with fuzzy search."
  (interactive "DSelect directory: ")
  (let* ((org-files (drringo/org-get-org-files-in-directory directory))
         (org-ids (drringo/org-collect-ids-from-files org-files)))
    (if (null org-ids)
        (message "No headers with IDs found in directory: %s" directory)
      (let* ((selection (helm-comp-read "Select header: "
                                        (mapcar #'car org-ids))))
        (when selection
          (let* ((id (cadr (assoc selection org-ids)))
                 (description selection)
                 (link (format "[[id:%s][%s]]" id description)))
            (insert link)))))))

(defun drringo/org-get-org-files-in-directory (directory)
  "Trả về danh sách các file .org trong DIRECTORY (không bao gồm thư mục con)."
  (directory-files directory t "\\.org$"))

(defun drringo/org-collect-ids-from-files (files)
  "Thu thập tất cả headers có ID từ danh sách FILES."
  (let (id-headers)
    (dolist (file files)
      (with-current-buffer (find-file-noselect file)
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((id (org-entry-get (point) "ID")))
              (when id
                (push (list (org-get-heading t t t t) id) id-headers))))
          nil 'file))))
    (reverse id-headers)))

;; Thiết lập phím tắt
(global-set-key (kbd "C-c l f") 'drringo/org-get-headers-with-ids-in-folder)

;; ==============================================================
;; PHẦN 3: LIÊN KẾT ĐẾN HÌNH ẢNH/BẢNG CÓ TÊN
;; ==============================================================
;; Mô tả: Tìm kiếm và liên kết đến các đối tượng (hình ảnh, bảng,...) 
;; đã được đánh dấu #+NAME và có #+CAPTION trong tài liệu
;;
;; QUY TRÌNH:
;; 1. Tìm tất cả #+NAME và #+CAPTION, tạo thành cặp
;; 2. Hiển thị danh sách caption cho người dùng chọn
;; 3. Chọn và tạo link đến #+NAME đó
;; 4. Tùy chỉnh description của link (mặc định là CAPTION)

(defun drringo/get-org-image-names-and-captions ()
  "Lấy tất cả các cặp (name . caption) hoặc (caption . caption) nếu không có name, xử lý mọi thứ tự xuất hiện."
  (let (result current-name current-caption)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (cond
         ((looking-at "^#\\+NAME: \\(.*\\)$")
          (setq current-name (match-string 1))
          (forward-line 1))
         ((looking-at "^#\\+CAPTION: \\(.*\\)$")
          (setq current-caption (match-string 1))
          (forward-line 1))
         ((looking-at "^#\\+")
          ;; Gặp directive khác, bỏ qua
          (forward-line 1))
         (t
          ;; Gặp dòng không phải directive, nếu có name/caption thì lưu lại
          (when (or current-name current-caption)
            (push (cons (or current-name current-caption)
                        (or current-caption current-name)) result)
            (setq current-name nil)
            (setq current-caption nil))
          (forward-line 1))))
      ;; Nếu còn sót ở cuối file
      (when (or current-name current-caption)
        (push (cons (or current-name current-caption)
                    (or current-caption current-name)) result)))
    (reverse result)))

;; Sửa lại hàm helm-insert-org-image-link-with-custom-caption để gọi hàm mới
(defun helm-insert-org-image-link-with-custom-caption ()
  "Hiển thị danh sách hình ảnh/bảng qua caption hoặc name, 
   cho phép chỉnh sửa caption trước khi chèn liên kết."
  (interactive)
  (let* ((image-info (drringo/get-org-image-names-and-captions)))
    (if (null image-info)
        (message "No named images or tables found in current document")
      (let* ((captions (mapcar #'cdr image-info))
             (selected-caption (helm-comp-read "Chọn caption hoặc name: " captions))
             ;; Tìm tên hình ảnh tương ứng với caption đã chọn
             (selected-name (car (rassoc selected-caption image-info)))
             ;; Cho phép người dùng thay đổi caption với giá trị mặc định
             (new-caption (read-string "Chỉnh sửa caption (hoặc để nguyên): " selected-caption)))
        ;; Chèn liên kết với caption đã chỉnh sửa
        (when selected-name
          (insert (format "[[%s][%s]]" selected-name new-caption)))))))

;; Thiết lập phím tắt
(global-set-key (kbd "C-c l c") 'helm-insert-org-image-link-with-custom-caption)

;; ==============================================================
;; KẾT THÚC LIÊN KẾT BẰNG ID
;; ==============================================================

(provide 'orgmode-linked-by-id)

;;; orgmode-linked-by-id.el ends here
