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

;; ==============================================================
;; PHẦN 1: LIÊN KẾT HEADER TRONG FILE HIỆN TẠI
;; ==============================================================

(defun my/org-get-headers-with-ids ()
  "Tìm tất cả headers có ID trong file hiện tại và chèn liên kết Org-mode."
  (interactive)
  (let* ((org-ids (delq nil (org-map-entries
                   (lambda ()
                     (let ((id (org-entry-get (point) "ID")))
                       (when id
                         (list (org-get-heading t t t t) id)))))))
         (selection (completing-read "Select header: "
                                     (mapcar 'car org-ids))))
    (when selection
      (let* ((id (cadr (assoc selection org-ids)))
             (description selection)
             (link (format "[[id:%s][%s]]" id description)))
        (insert link)))))

;; Thiết lập phím tắt
(global-set-key (kbd "C-c l i") 'my/org-get-headers-with-ids)
(map! :leader (:prefix "l" :desc "Insert ID in file" "i" #'my/org-get-headers-with-ids))

;; ==============================================================
;; PHẦN 2: LIÊN KẾT HEADER TRONG THƯ MỤC
;; ==============================================================

;; Load các package cần thiết
(require 'org-id)
(require 'ivy)

;; Bật fuzzy matching trong Ivy để tìm kiếm dễ dàng hơn
(setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

(defun my/org-get-headers-with-ids-in-folder (directory)
  "Search all org headers with IDs in DIRECTORY (no subfolders) and insert an Org-mode link with fuzzy search."
  (interactive "DSelect directory: ")
  (let* ((org-files (my/org-get-org-files-in-directory directory))
         (org-ids (my/org-collect-ids-from-files org-files)))
    (if (null org-ids)
        (message "No headers with IDs found in directory: %s" directory)
      (let* ((selection (ivy-completing-read "Select header: "
                                             (mapcar 'car org-ids))))
        (when selection
          (let* ((id (cadr (assoc selection org-ids)))
                 (description selection)
                 (link (format "[[id:%s][%s]]" id description)))
            (insert link)))))))

(defun my/org-get-org-files-in-directory (directory)
  "Trả về danh sách các file .org trong DIRECTORY (không bao gồm thư mục con)."
  (directory-files directory t "\\.org$"))

(defun my/org-collect-ids-from-files (files)
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
(global-set-key (kbd "C-c l f") 'my/org-get-headers-with-ids-in-folder)
(map! :leader (:prefix "l" :desc "Insert ID in folder" "f" #'my/org-get-headers-with-ids-in-folder))



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

;; Load Helm package
(require 'helm)
(helm-mode 1)

(defun get-org-image-names-and-captions ()
  "Lấy tất cả các #+NAME và #+CAPTION của hình ảnh/bảng trong tài liệu Org mode.
   
   Xử lý logic:
   - Khi gặp #+NAME: tạo cặp mới với caption = nil
   - Khi gặp #+CAPTION: cập nhật caption cho name gần nhất
   - Nếu không có name trước đó: tạo entry với caption làm name
   - Nếu name không có caption: sử dụng name làm caption"
  (let (image-info name-caption-pairs)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+\\(NAME\\|CAPTION\\): \\(.*\\)$" nil t)
        (let ((type (match-string 1))
              (value (match-string 2)))
          (cond
           ;; Xử lý #+NAME: tạo cặp mới
           ((string-equal type "NAME")
            (push (cons value nil) name-caption-pairs))
           ;; Xử lý #+CAPTION: cập nhật caption cho name gần nhất
           ((string-equal type "CAPTION")
            (if name-caption-pairs
                (setcdr (car name-caption-pairs) value)
              ;; Nếu không có name trước đó, tạo entry với caption làm name
              (push (cons value value) name-caption-pairs))))))
      ;; Chuyển đổi name-caption-pairs thành image-info
      (dolist (pair name-caption-pairs)
        (let ((name (car pair))
              (caption (cdr pair)))
          (push (cons name (or caption name)) image-info))))
    (reverse image-info)))

(defun helm-insert-org-image-link-with-custom-caption ()
  "Hiển thị danh sách hình ảnh/bảng qua caption hoặc name, 
   cho phép chỉnh sửa caption trước khi chèn liên kết."
  (interactive)
  (let* ((image-info (get-org-image-names-and-captions)))
    (if (null image-info)
        (message "No named images or tables found in current document")
      (let* ((captions (mapcar 'cdr image-info))
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
(map! :leader (:prefix "l" :desc "Insert entity link" "c" #'helm-insert-org-image-link-with-custom-caption))

;; ==============================================================
;; KẾT THÚC LIÊN KẾT BẰNG ID
;; ==============================================================
